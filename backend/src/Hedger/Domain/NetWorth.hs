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
) where

import Data.Decimal (Decimal)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Internal.NetWorth (assetLiabilityPeriodicReport, gainPeriodicReport)
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    ReportSide,
    TimelinePoint (..),
    breakdownItemsFor,
    periodsWithData,
    reportMissing,
    reportPeriodsFrom,
    reportSide,
    reportTotals,
    selectedPeriod,
    wholeReport,
 )
import Hedger.Domain.Period (
    Granularity,
    ReportPeriod,
    lastMaybe,
    reportPeriodKey,
    reportPeriodStart,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (
    Conversion (..),
    complete,
    conversionMissing,
    conversionValue,
    requireComplete,
 )

data PerformancePoint = PerformancePoint
    { period :: Text
    , startDate :: Day
    , netInvestment :: Decimal
    , netWorth :: Decimal
    , gainLoss :: Decimal
    }

data NetWorthFigures = NetWorthFigures
    { timeline :: [TimelinePoint Decimal]
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
    DisplayDepth ->
    DisplayDepth ->
    NetWorthFigures
computeNetWorth
    snapshot
    matched
    granularity
    commodity
    positiveReq
    negativeReq
    positiveDepth
    negativeDepth =
        NetWorthFigures
            { timeline = points
            , performance = mapMaybe conversionValue performanceResults
            , incompletePeriods =
                length (filter (not . Set.null . conversionMissing) performanceResults)
            , selectedKey = reportPeriodKey <$> selectedPeriod positiveReq dataPeriods
            , currentAssets = conversionValue assetsCurrent
            , currentLiabilities = conversionValue liabilitiesCurrent
            , currentNetWorth =
                (-) <$> conversionValue assetsCurrent <*> conversionValue liabilitiesCurrent
            , assetsBreakdownPeriod = fst assetsBreakdown
            , assetsBreakdownItems = snd assetsBreakdown
            , liabilitiesBreakdownPeriod = fst liabilitiesBreakdown
            , liabilitiesBreakdownItems = snd liabilitiesBreakdown
            , missing = historyMissing <> performanceMissing <> currentMissing
            }
      where
        combinedReport = assetLiabilityPeriodicReport snapshot matched granularity commodity
        assetSide = reportSide snapshot.journal [Hledger.Asset, Hledger.Cash] combinedReport
        liabilitySide = reportSide snapshot.journal [Hledger.Liability] combinedReport
        -- CalcGain is a different balance calculation, so it cannot share the report above.
        gainSide = wholeReport (gainPeriodicReport snapshot matched granularity commodity)
        reportPeriods = reportPeriodsFrom granularity combinedReport
        dataPeriods = periodsWithData granularity matched reportPeriods
        dataPeriodKeys = Set.fromList (fmap reportPeriodKey dataPeriods)
        assetTotals = reportTotals commodity assetSide
        liabilityTotals = fmap (fmap abs) (reportTotals commodity liabilitySide)
        allPoints = zipWith3 netWorthPointFrom reportPeriods assetTotals liabilityTotals
        points = filter ((`Set.member` dataPeriodKeys) . (.period)) allPoints
        assetsBreakdown =
            breakdownItemsFor
                commodity
                positiveDepth
                (selectedPeriod positiveReq dataPeriods)
                reportPeriods
                assetSide
        liabilitiesBreakdown =
            breakdownItemsFor
                commodity
                negativeDepth
                (selectedPeriod negativeReq dataPeriods)
                reportPeriods
                liabilitySide
        performanceResults =
            filter
                (maybe False ((`Set.member` dataPeriodKeys) . (.period)) . conversionValue)
                ( performanceFrom
                    commodity
                    reportPeriods
                    allPoints
                    assetTotals
                    liabilityTotals
                    gainSide
                )
        performanceMissing = reportMissing commodity gainSide
        historyMissing = reportMissing commodity assetSide <> reportMissing commodity liabilitySide
        assetsCurrent = strictLatest assetTotals
        liabilitiesCurrent = strictLatest liabilityTotals
        currentMissing = conversionMissing assetsCurrent <> conversionMissing liabilitiesCurrent

{- | The KPI value of a periodic column series.

The report journal drops price directives after the latest journal date, so
@AtEnd@ of the final interval resolves the same price as @AtDate@ of that date;
the final column is already the latest balance. What separates it from a KPI is
only the completeness rule: a KPI is withheld unless every commodity converted,
whereas the timeline reports a partial sum alongside its missing set.
-}
strictLatest :: [Conversion Decimal] -> Conversion Decimal
strictLatest = maybe (complete 0) requireComplete . lastMaybe

netWorthPointFrom ::
    ReportPeriod ->
    Conversion Decimal ->
    Conversion Decimal ->
    TimelinePoint Decimal
netWorthPointFrom period assetsResult liabilitiesResult =
    let assets = fromMaybe 0 (conversionValue assetsResult)
        liabilities = fromMaybe 0 (conversionValue liabilitiesResult)
     in TimelinePoint
            { period = reportPeriodKey period
            , startDate = reportPeriodStart period
            , positive = assets
            , negative = liabilities
            , net = assets - liabilities
            , missing = conversionMissing assetsResult <> conversionMissing liabilitiesResult
            }

performanceFrom ::
    Text ->
    [ReportPeriod] ->
    [TimelinePoint Decimal] ->
    [Conversion Decimal] ->
    [Conversion Decimal] ->
    ReportSide ->
    [Conversion PerformancePoint]
performanceFrom commodity periods netWorthPoints assetResults liabilityResults gainSide =
    zipWith3
        toPerformance
        periods
        netWorthPoints
        (zip3 assetResults liabilityResults (reportTotals commodity gainSide))
  where
    toPerformance period worthPoint (assetResult, liabilityResult, gainResult) =
        let missing =
                conversionMissing assetResult
                    <> conversionMissing liabilityResult
                    <> conversionMissing gainResult
            point = do
                gain <- conversionValue gainResult
                pure
                    PerformancePoint
                        { period = reportPeriodKey period
                        , startDate = reportPeriodStart period
                        , netInvestment = worthPoint.net - gain
                        , netWorth = worthPoint.net
                        , gainLoss = gain
                        }
         in case NonEmpty.nonEmpty (Set.toList missing) of
                Just uncoverted -> Unconverted uncoverted
                Nothing -> maybe NotApplicable Converted point
