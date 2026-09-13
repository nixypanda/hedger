module Hedger.Application.AccountRegisterQuery (
    RegisterPageRequest (..),
    RegisterEntry (..),
    RegisterPage (..),
    registerPage,
)
where

import Data.Text (Text)
import Data.Time (Day, addDays, fromGregorian)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.AccountTransactionsReport qualified as AccountReport
import Hledger.Reports.ReportOptions qualified as ReportOptions

import Hedger.Application.Error (AppError (..))
import Hedger.Application.Internal.Cursor (decodeRegisterCursor, encodeRegisterCursor)
import Hedger.Domain.Snapshot (Snapshot (..))

data RegisterPageRequest = RegisterPageRequest
    { account :: Text
    , commodity :: Maybe Text
    , start :: Maybe Day
    , end :: Maybe Day
    , limit :: Int
    , cursor :: Maybe Text
    }

data RegisterEntry = RegisterEntry
    { rawItem :: AccountReport.AccountTransactionsReportItem
    , displayItem :: AccountReport.AccountTransactionsReportItem
    , matchedAccounts :: [Text]
    }

data RegisterPage = RegisterPage
    { entries :: [RegisterEntry]
    , nextCursor :: Maybe Text
    }

registerPage :: Snapshot -> Text -> RegisterPageRequest -> Either AppError RegisterPage
registerPage snapshot resolvedCommodity request = do
    let canonicalAccount = request.account
        start = request.start
        end = request.end
    offset <-
        maybe
            (Right 0)
            (decodeRegisterCursor snapshot.revision canonicalAccount resolvedCommodity start end)
            request.cursor
    let accountQuery = accountQueryFor canonicalAccount
        reportSpec = reportSpecFor snapshot start end
        rawReport = AccountReport.accountTransactionsReport reportSpec snapshot.journal accountQuery
        valuedReport =
            AccountReport.accountTransactionsReport
                (valuedReportSpec resolvedCommodity reportSpec)
                snapshot.journal
                accountQuery
    paired <- pairReports accountQuery (reverse rawReport) (reverse valuedReport)
    if offset > length paired
        then Left (InvalidCursor "The cursor position is no longer valid.")
        else do
            let remaining = drop offset paired
                page = take request.limit remaining
                followingOffset = offset + length page
                cursor =
                    if length remaining > request.limit
                        then
                            Just
                                ( encodeRegisterCursor
                                    snapshot.revision
                                    canonicalAccount
                                    resolvedCommodity
                                    start
                                    end
                                    followingOffset
                                )
                        else Nothing
            pure RegisterPage{entries = page, nextCursor = cursor}

accountQueryFor :: Text -> HledgerQuery.Query
accountQueryFor = HledgerQuery.Acct . HledgerAccountName.accountNameToAccountRegex

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
dateQuery start end =
    HledgerQuery.Date
        ( Hledger.DateSpan
            (Hledger.Exact <$> start)
            (Hledger.Exact . addDays 1 <$> end)
        )

pairReports ::
    HledgerQuery.Query ->
    AccountReport.AccountTransactionsReport ->
    AccountReport.AccountTransactionsReport ->
    Either AppError [RegisterEntry]
pairReports accountQuery raw valued
    | length raw /= length valued = Left InternalError
    | otherwise = Right (zipWith (registerEntry accountQuery) raw valued)

registerEntry ::
    HledgerQuery.Query ->
    AccountReport.AccountTransactionsReportItem ->
    AccountReport.AccountTransactionsReportItem ->
    RegisterEntry
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
