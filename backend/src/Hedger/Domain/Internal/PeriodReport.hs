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
import Data.Time (Day, addDays)

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
    monthKey,
    periodsFromDateSpan,
    periodsFromReport,
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
    , missing :: Set Text
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
        (reportPeriod, reportInterval) = reportWindow granularity filteredJournal windowSpec
        valuation = case accumulation of
            HledgerReportOptions.PerPeriod -> HledgerValuation.AtThen (Just commodity)
            HledgerReportOptions.Cumulative -> HledgerValuation.AtEnd (Just commodity)
            HledgerReportOptions.Historical -> HledgerValuation.AtEnd (Just commodity)
        baseReportOptions =
            HledgerReportOptions.defreportopts
                { HledgerReportOptions.interval_ = reportInterval
                , HledgerReportOptions.depth_ = Hledger.DepthSpec (Just (max 1 depth)) []
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
                    : case (granularity, snapshot.latestJournalDate) of
                        (Yearly, Just latest) ->
                            [ HledgerQuery.Date
                                (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 latest)))
                            ]
                        _ -> []
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
        filteredJournal = snapshot.journal{Hledger.jtxns = transactions}

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
