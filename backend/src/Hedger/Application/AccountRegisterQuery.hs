module Hedger.Application.AccountRegisterQuery (
    RegisterEntry (..),
    RegisterPage (..),
    registerPage,
)
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, defaultTimeLocale, fromGregorian, parseTimeM)
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.Internal.Cursor (decodeRegisterCursor, encodeRegisterCursor)
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.AccountTransactionsReport qualified as AccountReport
import Hledger.Reports.ReportOptions qualified as ReportOptions
import Hledger.Utils.Regex qualified as HledgerRegex
import Text.Read (readMaybe)

data RegisterEntry = RegisterEntry
    { rawItem :: AccountReport.AccountTransactionsReportItem
    , displayItem :: AccountReport.AccountTransactionsReportItem
    , matchedAccounts :: [Text]
    }

data RegisterPage = RegisterPage
    { entries :: [RegisterEntry]
    , nextCursor :: Maybe Text
    }

defaultPageLimit :: Int
defaultPageLimit = 50

maximumPageLimit :: Int
maximumPageLimit = 200

registerPage :: Snapshot -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either AppError RegisterPage
registerPage snapshot account commodity suppliedFrom suppliedTo suppliedLimit suppliedCursor = do
    canonicalAccount <- canonicalAccountPath account
    start <- parseDay suppliedFrom
    end <- parseDay suppliedTo
    validateRange start end
    limit <- parseLimit suppliedLimit
    offset <- maybe (Right 0) (decodeRegisterCursor snapshot.revision canonicalAccount commodity start end) suppliedCursor
    accountQuery <- accountQueryFor canonicalAccount
    let reportSpec = reportSpecFor snapshot start end
        rawReport = AccountReport.accountTransactionsReport reportSpec snapshot.journal accountQuery
        valuedReport = AccountReport.accountTransactionsReport (valuedReportSpec commodity reportSpec) snapshot.journal accountQuery
    paired <- pairReports accountQuery (reverse rawReport) (reverse valuedReport)
    if offset > length paired
        then Left (InvalidCursor "The cursor position is no longer valid.")
        else do
            let remaining = drop offset paired
                page = take limit remaining
                followingOffset = offset + length page
                cursor =
                    if length remaining > limit
                        then Just (encodeRegisterCursor snapshot.revision canonicalAccount commodity start end followingOffset)
                        else Nothing
            pure RegisterPage{entries = page, nextCursor = cursor}

canonicalAccountPath :: Text -> Either AppError Text
canonicalAccountPath raw =
    let path = Text.strip raw
     in if Text.null path
            then Left (InvalidRequest "An account path is required.")
            else Right path

parseDay :: Maybe Text -> Either AppError (Maybe Day)
parseDay Nothing = Right Nothing
parseDay (Just raw) =
    case parseTimeM True defaultTimeLocale "%F" (Text.unpack raw) of
        Just day | Text.pack (show day) == raw -> Right (Just day)
        _ -> Left InvalidDate

validateRange :: Maybe Day -> Maybe Day -> Either AppError ()
validateRange (Just start) (Just end)
    | start > end = Left InvalidDateRange
validateRange _ _ = Right ()

parseLimit :: Maybe Text -> Either AppError Int
parseLimit Nothing = Right defaultPageLimit
parseLimit (Just raw) =
    case readMaybe (Text.unpack raw) of
        Just value | value >= 1 && value <= maximumPageLimit -> Right value
        _ -> Left (InvalidLimit defaultPageLimit maximumPageLimit)

accountQueryFor :: Text -> Either AppError HledgerQuery.Query
accountQueryFor account =
    case HledgerRegex.toRegex ("^" <> escapeRegex account <> "(:|$)") of
        Left _ -> Left (InvalidRequest "The account path is invalid.")
        Right pattern -> Right (HledgerQuery.Acct pattern)

escapeRegex :: Text -> Text
escapeRegex = Text.concatMap escape
  where
    escape character
        | character `elem` ("\\.^$|?*+()[]{}" :: String) = "\\" <> Text.singleton character
        | otherwise = Text.singleton character

reportSpecFor :: Snapshot -> Maybe Day -> Maybe Day -> ReportOptions.ReportSpec
reportSpecFor snapshot start end =
    ReportOptions.defreportspec
        { ReportOptions._rsDay = maybe (fromGregorian 1970 1 1) id snapshot.latestJournalDate
        , ReportOptions._rsQuery = dateQuery start end
        }

valuedReportSpec :: Text -> ReportOptions.ReportSpec -> ReportOptions.ReportSpec
valuedReportSpec commodity spec =
    spec
        { ReportOptions._rsReportOpts =
            spec._rsReportOpts{ReportOptions.value_ = Just (ReportOptions.AtThen (Just commodity))}
        }

dateQuery :: Maybe Day -> Maybe Day -> HledgerQuery.Query
dateQuery Nothing Nothing = HledgerQuery.Any
dateQuery start end = HledgerQuery.Date (Hledger.DateSpan (Hledger.Exact <$> start) (Hledger.Exact . addDays 1 <$> end))

pairReports :: HledgerQuery.Query -> AccountReport.AccountTransactionsReport -> AccountReport.AccountTransactionsReport -> Either AppError [RegisterEntry]
pairReports accountQuery raw valued
    | length raw /= length valued = Left InternalError
    | otherwise = Right (zipWith (registerEntry accountQuery) raw valued)

registerEntry :: HledgerQuery.Query -> AccountReport.AccountTransactionsReportItem -> AccountReport.AccountTransactionsReportItem -> RegisterEntry
registerEntry accountQuery raw valued =
    RegisterEntry
        { rawItem = raw
        , displayItem = valued
        , matchedAccounts =
            [ posting.paccount
            | posting <- originalTransaction.tpostings
            , HledgerQuery.matchesPosting accountQuery posting
            ]
        }
  where
    (originalTransaction, _, _, _, _, _) = raw
