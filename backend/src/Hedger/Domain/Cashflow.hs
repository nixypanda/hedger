{- | Cashflow: income and expense flows per period (the movement side of the
ledger, as opposed to net worth's stock).
-}
module Hedger.Domain.Cashflow (
    TimelinePoint (..),
    BreakdownItem (..),
    CashflowFigures (..),
    computeCashflow,
    computeCashflowResumed,

    -- * The checkpoint cache 'computeCashflowResumed' takes and returns
    CashflowColumns,
    emptyColumns,
    columnCount,
    columnMonths,
    truncateColumns,
)
where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (PrefixChain)
import Hedger.Domain.Internal.Cashflow (cashflowReportPeriods, incomeExpensePeriodicReport)
import Hedger.Domain.Internal.CashflowCheckpoint (
    CashflowColumn (..),
    CashflowColumns,
    ColumnSide (..),
    cashflowColumnsResumed,
    collapseToDepth,
    columnCount,
    columnMonths,
    emptyColumn,
    emptyColumns,
    monthEndsIn,
    sumColumns,
    truncateColumns,
 )
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    TimelinePoint (..),
    breakdownItemsFor,
    periodsWithData,
    reportMissing,
    reportPeriodsFrom,
    reportSide,
    reportTotals,
    selectedPeriod,
 )
import Hedger.Domain.Period (Granularity, ReportPeriod (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed)

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
    combinedReport = incomeExpensePeriodicReport snapshot matched granularity commodity
    incomeSide = reportSide snapshot.journal [Hledger.Revenue, Hledger.Gain] combinedReport
    expenseSide = reportSide snapshot.journal [Hledger.Expense] combinedReport
    reportPeriods = reportPeriodsFrom granularity combinedReport
    dataPeriods = periodsWithData granularity matched reportPeriods
    dataPeriodKeys = Set.fromList (fmap (.key) dataPeriods)
    incomeTotals = fmap (fmap negate) (reportTotals commodity incomeSide)
    expenseTotals = reportTotals commodity expenseSide
    missing = reportMissing commodity incomeSide <> reportMissing commodity expenseSide
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
            then breakdownItemsFor commodity positiveDepth positivePeriod reportPeriods incomeSide
            else (maybe "" (.key) positivePeriod, [])
    expenseBreakdown =
        if available
            then breakdownItemsFor commodity negativeDepth negativePeriod reportPeriods expenseSide
            else (maybe "" (.key) negativePeriod, [])

{- | 'computeCashflow' assembled from checkpointed monthly columns.

Observationally equal to 'computeCashflow' for the same snapshot, matched
transactions, granularity, commodity, requested periods and depths — which the
spec asserts field by field, keeping 'computeCashflow' as the oracle.

The request's periods are derived first, then decomposed into the calendar months
they are sums of, and only the months not already cached are computed. A monthly
request needs its own month per column; a yearly request needs the twelve months
of each year and adds them, which is exact because @PerPeriod@ accumulation
values every posting at its own date. Depth is applied at the very end, to
unclipped rows, so the cache is shared across depths.

The returned map is the input map extended with the months computed here.
-}
computeCashflowResumed ::
    Snapshot ->
    [Hledger.Transaction] ->
    PrefixChain ->
    CashflowColumns ->
    -- | Canonical text of the query that produced the matched transactions.
    Text ->
    Granularity ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Int ->
    Int ->
    (CashflowFigures, CashflowColumns)
computeCashflowResumed
    snapshot
    matched
    chain
    cached
    query
    granularity
    commodity
    positiveReq
    negativeReq
    positiveDepth
    negativeDepth =
        ( CashflowFigures
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
        , updated
        )
      where
        reportPeriods = cashflowReportPeriods snapshot matched granularity
        periodMonths = [monthEndsIn period.start period.end | period <- reportPeriods]
        (monthColumns, updated) =
            cashflowColumnsResumed snapshot matched chain cached query commodity (concat periodMonths)
        periodColumns = fmap (sumColumns commodity) (regroup periodMonths monthColumns)
        dataPeriods = periodsWithData granularity matched reportPeriods
        dataPeriodKeys = Set.fromList (fmap (.key) dataPeriods)
        incomeTotals =
            [fmap negate (conversionFromMixed commodity column.income.total) | column <- periodColumns]
        expenseTotals = [conversionFromMixed commodity column.expense.total | column <- periodColumns]
        missing = Set.unions [column.income.missing <> column.expense.missing | column <- periodColumns]
        available = Set.null missing
        points =
            if available
                then
                    filter
                        ((`Set.member` dataPeriodKeys) . (.period))
                        (zipWith3 cashflowPointFrom reportPeriods incomeTotals expenseTotals)
                else []
        -- Mirrors 'breakdownItemsFor', which falls back to the first column when
        -- the selected period is not among the report's own periods. Unreachable,
        -- because the selection is made from a subset of them.
        columnFor period =
            fromMaybe
                (case periodColumns of first : _ -> first; [] -> emptyColumn)
                (lookup period.key (zip (fmap (.key) reportPeriods) periodColumns))
        breakdownFor requested depth side =
            case selectedPeriod requested dataPeriods of
                Nothing -> ("", [])
                Just period ->
                    if available
                        then (period.key, collapseToDepth commodity depth (side (columnFor period)))
                        else (period.key, [])
        incomeBreakdown = breakdownFor positiveReq positiveDepth (.income)
        expenseBreakdown = breakdownFor negativeReq negativeDepth (.expense)

{- | Redistribute a flat list back into the shape it was flattened from, so the
months a period was decomposed into line up again with that period.
-}
regroup :: [[key]] -> [value] -> [[value]]
regroup [] _ = []
regroup (shape : shapes) values =
    let (taken, rest) = List.splitAt (length shape) values
     in taken : regroup shapes rest

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
