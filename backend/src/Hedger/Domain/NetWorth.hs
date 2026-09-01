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
    computeNetWorthResumed,

    -- * The checkpoint cache 'computeNetWorthResumed' takes and returns
    QueryKey (..),
    OpeningBalances,
    emptyOpeningBalances,
    openingBalanceCount,
    openingBalanceDays,
    openingBalanceEntries,
    truncateOpeningBalances,
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

import Hedger.Domain.Fingerprint (PrefixChain)
import Hedger.Domain.Internal.NetWorth (assetLiabilityPeriodicReport, gainPeriodicReport)
import Hedger.Domain.Internal.NetWorthCheckpoint (
    OpeningBalances,
    QueryKey (..),
    ResumedBalanceSheet (..),
    assetLiabilityResumed,
    emptyOpeningBalances,
    openingBalanceCount,
    openingBalanceDays,
    openingBalanceEntries,
    truncateOpeningBalances,
 )
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
import Hedger.Domain.Period (Granularity, ReportPeriod (..), lastMaybe)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), complete, requireComplete)

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
    figuresFrom
        snapshot
        matched
        granularity
        commodity
        positiveReq
        negativeReq
        positiveDepth
        negativeDepth
        (assetLiabilityPeriodicReport snapshot matched granularity commodity)

{- | 'computeNetWorth' with the consolidated balance-sheet report resumed from
checkpointed opening balances, returning the opening balances the sweep observed.

Only the balance-sheet report is resumed. The gain report is a different balance
calculation — with @Historical@ accumulation @CalcGain@ consults the balance
calculation inside hledger's own after-sum valuation — so it cannot share a pass
with the balance report and is not checkpointed here. Everything downstream of
the two reports is the same code as 'computeNetWorth' runs.
-}
computeNetWorthResumed ::
    Snapshot ->
    [Hledger.Transaction] ->
    QueryKey ->
    PrefixChain ->
    OpeningBalances ->
    Granularity ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Int ->
    Int ->
    (NetWorthFigures, OpeningBalances)
computeNetWorthResumed
    snapshot
    matched
    query
    chain
    openings
    granularity
    commodity
    positiveReq
    negativeReq
    positiveDepth
    negativeDepth =
        ( figuresFrom
            snapshot
            matched
            granularity
            commodity
            positiveReq
            negativeReq
            positiveDepth
            negativeDepth
            resumed.report
        , resumed.openings
        )
      where
        resumed = assetLiabilityResumed snapshot matched query chain openings granularity commodity

figuresFrom ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Int ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport ->
    NetWorthFigures
figuresFrom
    snapshot
    matched
    granularity
    commodity
    positiveReq
    negativeReq
    positiveDepth
    negativeDepth
    combinedReport =
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
        assetSide = reportSide snapshot.journal [Hledger.Asset, Hledger.Cash] combinedReport
        liabilitySide = reportSide snapshot.journal [Hledger.Liability] combinedReport
        -- CalcGain is a different balance calculation, so it cannot share the report above.
        gainSide = wholeReport (gainPeriodicReport snapshot matched granularity commodity)
        reportPeriods = reportPeriodsFrom granularity combinedReport
        dataPeriods = periodsWithData granularity matched reportPeriods
        dataPeriodKeys = Set.fromList (fmap (.key) dataPeriods)
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
                (maybe False ((`Set.member` dataPeriodKeys) . (.period)) . (.value))
                (performanceFrom commodity reportPeriods allPoints assetTotals liabilityTotals gainSide)
        performanceMissing = reportMissing commodity gainSide
        historyMissing = reportMissing commodity assetSide <> reportMissing commodity liabilitySide
        assetsCurrent = strictLatest assetTotals
        liabilitiesCurrent = strictLatest liabilityTotals
        currentMissing = assetsCurrent.missing <> liabilitiesCurrent.missing

{- | The KPI value of a periodic column series.

The report journal drops price directives after the latest journal date, so
@AtEnd@ of the final interval resolves the same price as @AtDate@ of that date;
the final column is already the latest balance. What separates it from a KPI is
only the completeness rule: a KPI is withheld unless every commodity converted,
whereas the timeline reports a partial sum alongside its missing set.
-}
strictLatest :: [Conversion Decimal] -> Conversion Decimal
strictLatest = maybe (complete 0) requireComplete . lastMaybe

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
