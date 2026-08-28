{- | Shared period-report kernel: the hledger balance/flow report plumbing and
the line-item value objects used by the net-worth and cashflow calculations.
-}
module Hedger.Domain.Internal.PeriodReport (
    TimelinePoint (..),
    BreakdownItem (..),
    ReportSide (..),
    periodicBalanceReport,
    wholeReport,
    reportSide,
    reportWindow,
    reportPeriodsFrom,
    periodsWithData,
    reportTotals,
    reportMissing,
    selectedPeriod,
    breakdownItemsFor,
)
where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, addDays, toGregorian)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Account (atDepth)
import Hedger.Domain.Period (
    Granularity (..),
    ReportPeriod (..),
    atIndex,
    epoch,
    lastMaybe,
    periodsFromDateSpan,
    periodsFromReport,
    takeLast,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed, missingCommodities)

data TimelinePoint = TimelinePoint
    { period :: Text
    , startDate :: Day
    , positive :: Decimal
    , negative :: Decimal
    , net :: Decimal
    , missing :: Set Text
    }

data BreakdownItem = BreakdownItem
    { account :: Text
    , quantity :: Decimal
    }

{- | The rows and per-column totals a calculation projects out of a periodic
report. A report built for one set of account types can use hledger's own totals
row; a consolidated report cannot, because hledger derives that row from the
root account's inclusive balance, which spans every side at once.
-}
data ReportSide = ReportSide
    { columns :: [Hledger.MixedAmount]
    , rows :: [HledgerMultiBalance.MultiBalanceReportRow]
    }

wholeReport :: HledgerMultiBalance.MultiBalanceReport -> ReportSide
wholeReport report = ReportSide{columns = report.prTotals.prrAmounts, rows = report.prRows}

{- | One side of a consolidated report, selected by account type.

hledger clips account names on the postings before it aggregates, so a row's
balance already includes everything beneath it and summing the retained rows is
exact at any depth. The consolidated report is nonetheless built unclipped: a
clipped name spanning both sides could not be attributed to either.
-}
reportSide ::
    Hledger.Journal ->
    [Hledger.AccountType] ->
    HledgerMultiBalance.MultiBalanceReport ->
    ReportSide
reportSide journal accountTypes report =
    ReportSide{columns = columnSums (length report.prDates) kept, rows = kept}
  where
    kept = filter matchesType report.prRows
    matchesType row =
        maybe
            False
            (`elem` accountTypes)
            (HledgerJournal.journalAccountType journal row.prrName.displayFull)

columnSums :: Int -> [HledgerMultiBalance.MultiBalanceReportRow] -> [Hledger.MixedAmount]
columnSums columnCount =
    List.foldl' (zipWith HledgerAmount.maPlus) (replicate columnCount HledgerAmount.nullmixedamt)
        . fmap (.prrAmounts)

periodicBalanceReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    [Hledger.AccountType] ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    -- | Display depth to clip rows to; 'Nothing' leaves the report unclipped.
    Maybe Int ->
    HledgerMultiBalance.MultiBalanceReport
periodicBalanceReport
    snapshot
    transactions
    granularity
    commodity
    accountTypes
    calculation
    accumulation
    depth =
        HledgerMultiBalance.multiBalanceReport reportSpec filteredJournal
      where
        (reportPeriod, reportInterval) = reportWindow granularity filteredJournal windowSpec
        valuation = case accumulation of
            HledgerReportOptions.PerPeriod -> HledgerValuation.AtThen (Just commodity)
            HledgerReportOptions.Cumulative -> HledgerValuation.AtEnd (Just commodity)
            HledgerReportOptions.Historical -> HledgerValuation.AtEnd (Just commodity)
        baseReportOptions =
            HledgerReportOptions.defreportopts
                { HledgerReportOptions.interval_ = reportInterval
                , HledgerReportOptions.depth_ = Hledger.DepthSpec (max 1 <$> depth) []
                , HledgerReportOptions.empty_ = True
                , HledgerReportOptions.balancecalc_ = calculation
                , HledgerReportOptions.balanceaccum_ = accumulation
                , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                }
        reportOptions =
            baseReportOptions
                { HledgerReportOptions.period_ = reportPeriod
                , HledgerReportOptions.value_ = Just valuation
                }
        windowSpec =
            HledgerReportOptions.defreportspec
                { HledgerReportOptions._rsReportOpts = baseReportOptions
                , HledgerReportOptions._rsDay = fromMaybe epoch snapshot.latestJournalDate
                , HledgerReportOptions._rsQuery = windowQuery
                }
        windowQuery =
            HledgerQuery.And
                ( HledgerQuery.Type accountTypes
                    : case snapshot.latestJournalDate of
                        Just latest ->
                            [ HledgerQuery.Date
                                (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 latest)))
                            ]
                        Nothing -> []
                )
        reportSpec =
            HledgerReportOptions.defreportspec
                { HledgerReportOptions._rsReportOpts = reportOptions
                , HledgerReportOptions._rsDay = fromMaybe epoch snapshot.latestJournalDate
                , HledgerReportOptions._rsQuery =
                    HledgerQuery.And
                        [ HledgerQuery.Type accountTypes
                        , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan reportPeriod)
                        ]
                }
        filteredJournal =
            snapshot.journal
                { Hledger.jtxns = transactions
                , Hledger.jpricedirectives = pricesThroughLatest
                }
        pricesThroughLatest =
            -- hledger expands the final interval to its calendar boundary, so
            -- the price oracle must also exclude directives beyond the cutoff.
            case snapshot.latestJournalDate of
                Nothing -> snapshot.journal.jpricedirectives
                Just latest -> filter ((<= latest) . Hledger.pddate) snapshot.journal.jpricedirectives

reportWindow ::
    Granularity ->
    Hledger.Journal ->
    HledgerReportOptions.ReportSpec ->
    (Hledger.Period, Hledger.Interval)
reportWindow granularity journal reportSpec =
    case selectedPeriods of
        [] -> (Hledger.PeriodAll, interval)
        first : rest ->
            let final = fromMaybe first (lastMaybe rest)
             in (Hledger.PeriodBetween first.start (addDays 1 final.end), interval)
  where
    interval = case granularity of
        Monthly -> Hledger.Months 1
        Yearly -> Hledger.Years 1
    periods = periodsFromReport granularity journal reportSpec
    selectedPeriods = case granularity of
        Monthly -> takeLast 24 periods
        Yearly -> periods

reportPeriodsFrom :: Granularity -> HledgerMultiBalance.MultiBalanceReport -> [ReportPeriod]
reportPeriodsFrom granularity report = concatMap (periodsFromDateSpan granularity) report.prDates

periodsWithData ::
    Granularity ->
    [Hledger.Transaction] ->
    [ReportPeriod] ->
    [ReportPeriod]
periodsWithData granularity transactions periods =
    case granularity of
        Monthly ->
            let dataMonths = Set.fromList (fmap (monthIndex . (.tdate)) transactions)
             in filter ((`Set.member` dataMonths) . monthIndex . (.start)) periods
        Yearly -> periods

{- | A total, compact month identifier for a Gregorian day. Using 'Integer'
keeps it safe for every 'Day' supported by 'time', including years outside the
usual application range.
-}
monthIndex :: Day -> Integer
monthIndex day =
    let (year, month, _) = toGregorian day
     in year * 12 + toInteger (month - 1)

reportTotals :: Text -> ReportSide -> [Conversion Decimal]
reportTotals commodity side = fmap (conversionFromMixed commodity) side.columns

reportMissing :: Text -> ReportSide -> Set Text
reportMissing commodity side =
    Set.unions
        [ missingCommodities commodity amount
        | amount <- side.columns <> concatMap (.prrAmounts) side.rows
        ]

selectedPeriod :: Maybe Text -> [ReportPeriod] -> Maybe ReportPeriod
selectedPeriod requested periods =
    case requested >>= (\key -> List.find ((== key) . (.key)) periods) of
        Just period -> Just period
        Nothing -> lastMaybe periods

{- | Ungrouped, depth-collapsed per-account totals for the selected period. The
top-N / \"Other\" grouping is a presentation concern applied downstream.
-}
breakdownItemsFor ::
    Text ->
    Int ->
    Maybe ReportPeriod ->
    [ReportPeriod] ->
    ReportSide ->
    (Text, [BreakdownItem])
breakdownItemsFor commodity depth selected periods side =
    case selected of
        Nothing -> ("", [])
        Just period ->
            let periodIndex = fromMaybe 0 (List.findIndex ((== period.key) . (.key)) periods)
                raw =
                    mapMaybe
                        ( \row -> do
                            amount <- atIndex periodIndex row.prrAmounts
                            quantity <- (conversionFromMixed commodity amount).value
                            if abs quantity < 0.000001
                                then Nothing
                                else
                                    Just
                                        BreakdownItem
                                            { account = row.prrName.displayFull
                                            , quantity = abs quantity
                                            }
                        )
                        side.rows
                depthGrouped =
                    [ BreakdownItem{account, quantity}
                    | (account, quantity) <-
                        Map.toList
                            (Map.fromListWith (+) [(atDepth depth item.account, item.quantity) | item <- raw])
                    ]
             in (period.key, depthGrouped)
