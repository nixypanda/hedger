{- | Net worth: the balance-sheet position (assets − liabilities) as a valued
stock, its trajectory over time, and its return decomposition (contributions
versus revaluation).
-}
module Hedger.Domain.NetWorth (
    TimelinePoint (..),
    BreakdownItem (..),
    PerformancePoint (..),
    NetWorthFigures (..),
    computeNetWorth,
)
where

import Data.Decimal (Decimal)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, addDays)

import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    TimelinePoint (..),
    breakdownItemsFor,
    periodicBalanceReport,
    periodsWithData,
    reportMissing,
    reportPeriodsFrom,
    reportTotals,
    selectedPeriod,
 )
import Hedger.Domain.Period (Granularity, ReportPeriod (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), complete, conversionFromMixed, requireComplete)

data PerformancePoint = PerformancePoint
    { period :: Text
    , startDate :: Day
    , netInvestment :: Decimal
    , netWorth :: Decimal
    , gainLoss :: Decimal
    }

data NetWorthFigures = NetWorthFigures
    { timeline :: [TimelinePoint]
    , performance :: [PerformancePoint]
    , incompletePeriods :: Int
    , selectedKey :: Maybe Text
    , currentAssets :: Maybe Decimal
    , currentLiabilities :: Maybe Decimal
    , currentNetWorth :: Maybe Decimal
    , assetsBreakdownPeriod :: Text
    , assetsBreakdownItems :: [BreakdownItem]
    , liabilitiesBreakdownPeriod :: Text
    , liabilitiesBreakdownItems :: [BreakdownItem]
    , missing :: Set Text
    }

computeNetWorth ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Int ->
    Int ->
    NetWorthFigures
computeNetWorth snapshot matched granularity commodity positiveReq negativeReq positiveDepth negativeDepth =
    NetWorthFigures
        { timeline = points
        , performance = mapMaybe (.value) performanceResults
        , incompletePeriods = length (filter (not . Set.null . (.missing)) performanceResults)
        , selectedKey = (.key) <$> selectedPeriod positiveReq dataPeriods
        , currentAssets = assetsCurrent.value
        , currentLiabilities = liabilitiesCurrent.value
        , currentNetWorth = (-) <$> assetsCurrent.value <*> liabilitiesCurrent.value
        , assetsBreakdownPeriod = fst assetsBreakdown
        , assetsBreakdownItems = snd assetsBreakdown
        , liabilitiesBreakdownPeriod = fst liabilitiesBreakdown
        , liabilitiesBreakdownItems = snd liabilitiesBreakdown
        , missing = historyMissing <> performanceMissing <> currentMissing
        }
  where
    assetReport =
        periodicBalanceReport
            snapshot
            matched
            granularity
            commodity
            [Hledger.Asset, Hledger.Cash]
            HledgerReportOptions.CalcChange
            HledgerReportOptions.Historical
            positiveDepth
    liabilityReport =
        periodicBalanceReport
            snapshot
            matched
            granularity
            commodity
            [Hledger.Liability]
            HledgerReportOptions.CalcChange
            HledgerReportOptions.Historical
            negativeDepth
    gainReport =
        periodicBalanceReport
            snapshot
            matched
            granularity
            commodity
            [Hledger.Asset, Hledger.Cash, Hledger.Liability]
            HledgerReportOptions.CalcGain
            HledgerReportOptions.Historical
            1
    reportPeriods = reportPeriodsFrom granularity assetReport
    dataPeriods = periodsWithData granularity matched reportPeriods
    dataPeriodKeys = Set.fromList (fmap (.key) dataPeriods)
    assetTotals = reportTotals commodity assetReport
    liabilityTotals = fmap (fmap abs) (reportTotals commodity liabilityReport)
    allPoints = zipWith3 netWorthPointFrom reportPeriods assetTotals liabilityTotals
    points = filter ((`Set.member` dataPeriodKeys) . (.period)) allPoints
    assetsBreakdown =
        breakdownItemsFor
            commodity
            positiveDepth
            (selectedPeriod positiveReq dataPeriods)
            reportPeriods
            assetReport
    liabilitiesBreakdown =
        breakdownItemsFor
            commodity
            negativeDepth
            (selectedPeriod negativeReq dataPeriods)
            reportPeriods
            liabilityReport
    performanceResults =
        filter
            (maybe False ((`Set.member` dataPeriodKeys) . (.period)) . (.value))
            (performanceFrom commodity reportPeriods allPoints assetTotals liabilityTotals gainReport)
    performanceMissing = reportMissing commodity gainReport
    historyMissing = reportMissing commodity assetReport <> reportMissing commodity liabilityReport
    assetsCurrent = currentReportValue snapshot matched commodity [Hledger.Asset, Hledger.Cash]
    liabilitiesCurrent = fmap abs (currentReportValue snapshot matched commodity [Hledger.Liability])
    currentMissing = assetsCurrent.missing <> liabilitiesCurrent.missing

netWorthPointFrom :: ReportPeriod -> Conversion Decimal -> Conversion Decimal -> TimelinePoint
netWorthPointFrom period assetsResult liabilitiesResult =
    let assets = fromMaybe 0 assetsResult.value
        liabilities = fromMaybe 0 liabilitiesResult.value
     in TimelinePoint
            { period = period.key
            , startDate = period.start
            , positive = assets
            , negative = liabilities
            , net = assets - liabilities
            , missing = assetsResult.missing <> liabilitiesResult.missing
            }

performanceFrom ::
    Text ->
    [ReportPeriod] ->
    [TimelinePoint] ->
    [Conversion Decimal] ->
    [Conversion Decimal] ->
    HledgerMultiBalance.MultiBalanceReport ->
    [Conversion PerformancePoint]
performanceFrom commodity periods netWorthPoints assetResults liabilityResults gainReport =
    zipWith3
        toPerformance
        periods
        netWorthPoints
        (zip3 assetResults liabilityResults (reportTotals commodity gainReport))
  where
    toPerformance period worthPoint (assetResult, liabilityResult, gainResult) =
        let missing = assetResult.missing <> liabilityResult.missing <> gainResult.missing
            point = do
                gain <- gainResult.value
                pure
                    PerformancePoint
                        { period = period.key
                        , startDate = period.start
                        , netInvestment = worthPoint.net - gain
                        , netWorth = worthPoint.net
                        , gainLoss = gain
                        }
         in Conversion{value = if Set.null missing then point else Nothing, missing}

currentReportValue :: Snapshot -> [Hledger.Transaction] -> Text -> [Hledger.AccountType] -> Conversion Decimal
currentReportValue snapshot transactions commodity accountTypes =
    case snapshot.latestJournalDate of
        Nothing -> complete 0
        Just day ->
            let options =
                    HledgerReportOptions.defreportopts
                        { HledgerReportOptions.period_ = Hledger.PeriodTo (addDays 1 day)
                        , HledgerReportOptions.value_ = Just (HledgerValuation.AtDate day (Just commodity))
                        , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
                        , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                        }
                spec =
                    HledgerReportOptions.defreportspec
                        { HledgerReportOptions._rsReportOpts = options
                        , HledgerReportOptions._rsDay = day
                        , HledgerReportOptions._rsQuery =
                            HledgerQuery.And
                                [ HledgerQuery.Type accountTypes
                                , HledgerQuery.Date
                                    (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 day)))
                                ]
                        }
                filteredJournal = snapshot.journal{Hledger.jtxns = transactions}
                (_, total) = HledgerBalance.balanceReport spec filteredJournal
             in requireComplete (conversionFromMixed commodity total)
