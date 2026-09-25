{- | Cashflow: income and expense flows per period (the movement side of the
ledger, as opposed to net worth's stock).
-}
module Hedger.Domain.Cashflow (
    TimelinePoint (..),
    BreakdownItem (..),
    CashflowFigures (..),
    computeCashflow,
) where

import Data.Decimal (Decimal)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Internal.Cashflow (incomeExpensePeriodicReport)
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
import Hedger.Domain.Period (Granularity, ReportPeriod, reportPeriodKey, reportPeriodStart)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionMissing, conversionValue)

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
    DisplayDepth ->
    DisplayDepth ->
    CashflowFigures
computeCashflow
    snapshot
    matched
    granularity
    commodity
    positiveReq
    negativeReq
    positiveDepth
    negativeDepth =
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
        dataPeriodKeys = Set.fromList (fmap reportPeriodKey dataPeriods)
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
                then
                    breakdownItemsFor
                        commodity
                        positiveDepth
                        positivePeriod
                        reportPeriods
                        incomeSide
                else (maybe "" reportPeriodKey positivePeriod, [])
        expenseBreakdown =
            if available
                then
                    breakdownItemsFor
                        commodity
                        negativeDepth
                        negativePeriod
                        reportPeriods
                        expenseSide
                else (maybe "" reportPeriodKey negativePeriod, [])

cashflowPointFrom :: ReportPeriod -> Conversion Decimal -> Conversion Decimal -> TimelinePoint
cashflowPointFrom period incomeResult expenseResult =
    let income = fromMaybe 0 (conversionValue incomeResult)
        expenses = fromMaybe 0 (conversionValue expenseResult)
     in TimelinePoint
            { period = reportPeriodKey period
            , startDate = reportPeriodStart period
            , positive = income
            , negative = expenses
            , net = income - expenses
            , missing = conversionMissing incomeResult <> conversionMissing expenseResult
            }
