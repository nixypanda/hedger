{- | Cashflow: income and expense flows per period (the movement side of the
ledger, as opposed to net worth's stock).
-}
module Hedger.Domain.Cashflow (
    TimelinePoint (..),
    BreakdownItem (..),
    CashflowFigures (..),
    computeCashflow,
)
where

import Data.Decimal (Decimal)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.Cashflow (expensePeriodicReport, incomeGainPeriodicReport)
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

data CashflowFigures = CashflowFigures
    { timeline :: [TimelinePoint]
    , available :: Bool
    , totalCount :: Int
    , filteredCount :: Int
    , incomeBreakdownPeriod :: Text
    , incomeBreakdownItems :: [BreakdownItem]
    , expenseBreakdownPeriod :: Text
    , expenseBreakdownItems :: [BreakdownItem]
    , missing :: Set Text
    }

computeCashflow ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Int ->
    Int ->
    CashflowFigures
computeCashflow snapshot matched granularity commodity positiveReq negativeReq positiveDepth negativeDepth =
    CashflowFigures
        { timeline = points
        , available = available
        , totalCount = length snapshot.transactions
        , filteredCount = length matched
        , incomeBreakdownPeriod = fst incomeBreakdown
        , incomeBreakdownItems = snd incomeBreakdown
        , expenseBreakdownPeriod = fst expenseBreakdown
        , expenseBreakdownItems = snd expenseBreakdown
        , missing = missing
        }
  where
    incomeReport = incomeGainPeriodicReport snapshot matched granularity commodity positiveDepth
    expenseReport = expensePeriodicReport snapshot matched granularity commodity negativeDepth
    reportPeriods = reportPeriodsFrom granularity incomeReport
    dataPeriods = periodsWithData granularity matched reportPeriods
    dataPeriodKeys = Set.fromList (fmap (.key) dataPeriods)
    incomeTotals = fmap (fmap negate) (reportTotals commodity incomeReport)
    expenseTotals = reportTotals commodity expenseReport
    missing = reportMissing commodity incomeReport <> reportMissing commodity expenseReport
    available = Set.null missing
    points =
        if available
            then
                filter
                    ((`Set.member` dataPeriodKeys) . (.period))
                    (zipWith3 cashflowPointFrom reportPeriods incomeTotals expenseTotals)
            else []
    positivePeriod = selectedPeriod positiveReq dataPeriods
    negativePeriod = selectedPeriod negativeReq dataPeriods
    incomeBreakdown =
        if available
            then breakdownItemsFor commodity positiveDepth positivePeriod reportPeriods incomeReport
            else (maybe "" (.key) positivePeriod, [])
    expenseBreakdown =
        if available
            then breakdownItemsFor commodity negativeDepth negativePeriod reportPeriods expenseReport
            else (maybe "" (.key) negativePeriod, [])

cashflowPointFrom :: ReportPeriod -> Conversion Decimal -> Conversion Decimal -> TimelinePoint
cashflowPointFrom period incomeResult expenseResult =
    let income = fromMaybe 0 incomeResult.value
        expenses = fromMaybe 0 expenseResult.value
     in TimelinePoint
            { period = period.key
            , startDate = period.start
            , positive = income
            , negative = expenses
            , net = income - expenses
            , missing = incomeResult.missing <> expenseResult.missing
            }
