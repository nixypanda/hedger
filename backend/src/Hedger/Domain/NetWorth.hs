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
import Data.Time (Day)

import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance

import Hedger.Domain.Internal.NetWorth (
    assetCashPeriodicReport,
    currentAssetCashBalance,
    currentLiabilityBalance,
    gainPeriodicReport,
    liabilityPeriodicReport,
 )
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    TimelinePoint (..),
    breakdownItemsFor,
    periodsWithData,
    reportMissing,
    reportPeriodsFrom,
    reportTotals,
    selectedPeriod,
 )
import Hedger.Domain.Period (Granularity, ReportPeriod (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..))

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
    assetReport = assetCashPeriodicReport snapshot matched granularity commodity positiveDepth
    liabilityReport = liabilityPeriodicReport snapshot matched granularity commodity negativeDepth
    gainReport = gainPeriodicReport snapshot matched granularity commodity
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
    assetsCurrent = currentAssetCashBalance snapshot matched commodity
    liabilitiesCurrent = currentLiabilityBalance snapshot matched commodity
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
