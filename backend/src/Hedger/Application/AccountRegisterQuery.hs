module Hedger.Application.AccountRegisterQuery (
    RegisterPageRequest (..),
    RegisterEntry (..),
    RegisterPage (..),
    registerPage,
) where

import Data.Text (Text)
import Data.Time (addDays, fromGregorian)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.AccountTransactionsReport qualified as AccountReport
import Hledger.Reports.ReportOptions qualified as ReportOptions

import Hedger.Application.Internal.Cursor (decodeRegisterCursor, encodeRegisterCursor)
import Hedger.Application.PageRequest (DateRange, PageLimit, dateRangeBounds, pageLimitValue)
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Snapshot (Snapshot (..))

data RegisterPageRequest = RegisterPageRequest
    { account :: Text
    , commodity :: Maybe Text
    , dateRange :: DateRange
    , limit :: PageLimit
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
    offset <-
        maybe
            (Right 0)
            ( decodeRegisterCursor
                snapshot.revision
                canonicalAccount
                resolvedCommodity
                request.dateRange
            )
            request.cursor
    let accountQuery = accountQueryFor canonicalAccount
        reportSpec = reportSpecFor snapshot request.dateRange
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
                page = take (pageLimitValue request.limit) remaining
                followingOffset = offset + length page
                cursor =
                    if length remaining > pageLimitValue request.limit
                        then
                            Just
                                ( encodeRegisterCursor
                                    snapshot.revision
                                    canonicalAccount
                                    resolvedCommodity
                                    request.dateRange
                                    followingOffset
                                )
                        else Nothing
            pure RegisterPage{entries = page, nextCursor = cursor}

accountQueryFor :: Text -> HledgerQuery.Query
accountQueryFor = HledgerQuery.Acct . HledgerAccountName.accountNameToAccountRegex

reportSpecFor :: Snapshot -> DateRange -> ReportOptions.ReportSpec
reportSpecFor snapshot dateRange =
    ReportOptions.defreportspec
        { ReportOptions._rsDay = maybe (fromGregorian 1970 1 1) id snapshot.latestJournalDate
        , ReportOptions._rsQuery = dateQuery dateRange
        }

valuedReportSpec :: Text -> ReportOptions.ReportSpec -> ReportOptions.ReportSpec
valuedReportSpec commodity spec =
    spec
        { ReportOptions._rsReportOpts =
            spec._rsReportOpts{ReportOptions.value_ = Just (ReportOptions.AtThen (Just commodity))}
        }

dateQuery :: DateRange -> HledgerQuery.Query
dateQuery dateRange =
    case dateRangeBounds dateRange of
        (Nothing, Nothing) -> HledgerQuery.Any
        (start, end) ->
            HledgerQuery.Date
                (Hledger.DateSpan (Hledger.Exact <$> start) (Hledger.Exact . addDays 1 <$> end))

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
