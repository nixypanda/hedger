{- | Shared period-report kernel: the hledger balance/flow report plumbing and
the line-item value objects used by the net-worth and cashflow calculations.
-}
module Hedger.Domain.Internal.PeriodReport (
    TimelinePoint (..),
    BreakdownItem (..),
    periodicBalanceReport,
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
import Data.Text qualified as Text
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)

import Hledger.Data.Dates qualified as HledgerDates
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
    maximumMaybe,
    minimumMaybe,
    monthKey,
    monthStart,
    takeLast,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed)

data TimelinePoint = TimelinePoint
    { period :: Text
    , startDate :: Day
    , positive :: Decimal
    , negative :: Decimal
    , net :: Decimal
    }

data BreakdownItem = BreakdownItem
    { account :: Text
    , quantity :: Decimal
    }

periodicBalanceReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    [Hledger.AccountType] ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    Int ->
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
        (reportPeriod, reportInterval) = reportWindow granularity transactions snapshot.latestJournalDate
        valuation = case accumulation of
            HledgerReportOptions.PerPeriod -> HledgerValuation.AtThen (Just commodity)
            HledgerReportOptions.Cumulative -> HledgerValuation.AtEnd (Just commodity)
            HledgerReportOptions.Historical -> HledgerValuation.AtEnd (Just commodity)
        reportOptions =
            HledgerReportOptions.defreportopts
                { HledgerReportOptions.period_ = reportPeriod
                , HledgerReportOptions.interval_ = reportInterval
                , HledgerReportOptions.value_ = Just valuation
                , HledgerReportOptions.depth_ = Hledger.DepthSpec (Just (max 1 depth)) []
                , HledgerReportOptions.empty_ = True
                , HledgerReportOptions.balancecalc_ = calculation
                , HledgerReportOptions.balanceaccum_ = accumulation
                , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                }
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
        filteredJournal = snapshot.journal{Hledger.jtxns = transactions}

reportWindow :: Granularity -> [Hledger.Transaction] -> Maybe Day -> (Hledger.Period, Hledger.Interval)
reportWindow granularity transactions latestDate =
    case granularity of
        Monthly ->
            case maximumMaybe selectedMonthlyStarts of
                Nothing -> (Hledger.PeriodAll, interval)
                Just finalMonth ->
                    let firstMonth = fromMaybe finalMonth (minimumMaybe selectedMonthlyStarts)
                     in (Hledger.PeriodBetween firstMonth (addGregorianMonthsClip 1 finalMonth), interval)
        Yearly ->
            case latestDate <|> maximumMaybe (fmap (.tdate) transactions) of
                Nothing -> (Hledger.PeriodAll, interval)
                Just latest ->
                    let (finalYear, _, _) = toGregorian latest
                        firstYear = maybe finalYear yearOf (minimumMaybe (fmap (.tdate) transactions))
                     in ( Hledger.PeriodBetween
                            (fromGregorian firstYear 1 1)
                            (fromGregorian (finalYear + 1) 1 1)
                        , interval
                        )
  where
    interval = case granularity of
        Monthly -> Hledger.Months 1
        Yearly -> Hledger.Years 1
    selectedMonthlyStarts =
        takeLast
            24
            (Set.toAscList (Set.fromList (fmap (monthStart . (.tdate)) transactions)))
    left <|> right = case left of
        Just _ -> left
        Nothing -> right
    yearOf day = let (year, _, _) = toGregorian day in year

reportPeriodsFrom :: Granularity -> HledgerMultiBalance.MultiBalanceReport -> [ReportPeriod]
reportPeriodsFrom granularity report = fmap toPeriod report.prDates
  where
    toPeriod dateSpan =
        let start = fromMaybe epoch (HledgerDates.spanStart dateSpan)
            exclusiveEnd = fromMaybe (addDays 1 start) (HledgerDates.spanEnd dateSpan)
         in ReportPeriod
                { key = case granularity of
                    Monthly -> monthKey start
                    Yearly -> let (year, _, _) = toGregorian start in Text.pack (show year)
                , start
                , end = addDays (-1) exclusiveEnd
                }

periodsWithData ::
    Granularity ->
    [Hledger.Transaction] ->
    [ReportPeriod] ->
    [ReportPeriod]
periodsWithData granularity transactions periods =
    case granularity of
        Monthly ->
            let dataMonths = Set.fromList (fmap (monthKey . (.tdate)) transactions)
             in filter ((`Set.member` dataMonths) . (.key)) periods
        Yearly -> periods

reportTotals :: Text -> HledgerMultiBalance.MultiBalanceReport -> [Conversion Decimal]
reportTotals commodity report = fmap (conversionFromMixed commodity) report.prTotals.prrAmounts

reportMissing :: Text -> HledgerMultiBalance.MultiBalanceReport -> Set Text
reportMissing commodity report =
    Set.unions
        [ (conversionFromMixed commodity amount).missing
        | amount <- report.prTotals.prrAmounts <> concatMap (.prrAmounts) report.prRows
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
    HledgerMultiBalance.MultiBalanceReport ->
    (Text, [BreakdownItem])
breakdownItemsFor commodity depth selected periods report =
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
                        report.prRows
                depthGrouped =
                    [ BreakdownItem{account, quantity}
                    | (account, quantity) <-
                        Map.toList
                            (Map.fromListWith (+) [(atDepth depth item.account, item.quantity) | item <- raw])
                    ]
             in (period.key, depthGrouped)
