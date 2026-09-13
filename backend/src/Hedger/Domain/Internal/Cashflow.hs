{- | Cashflow-specific report construction. Keeping these report definitions
here lets the calculation and its component benchmarks exercise the same
hledger options.
-}
module Hedger.Domain.Internal.Cashflow (
    incomeExpensePeriodicReport,
    incomeGainPeriodicReport,
    expensePeriodicReport,
)
where

import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (periodicBalanceReport)
import Hedger.Domain.Period (Granularity, ReportWindow)
import Hedger.Domain.Snapshot (Snapshot)

{- | Both cashflow sides in a single report; 'Hedger.Domain.Internal.PeriodReport.reportSide'
splits it back apart. Deliberately unclipped, which that function's docs require.
-}
incomeExpensePeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    ReportWindow ->
    Text ->
    HledgerMultiBalance.MultiBalanceReport
incomeExpensePeriodicReport snapshot transactions granularity window commodity =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        window
        commodity
        [Hledger.Revenue, Hledger.Gain, Hledger.Expense]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        Nothing

incomeGainPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    ReportWindow ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
incomeGainPeriodicReport snapshot transactions granularity window commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        window
        commodity
        [Hledger.Revenue, Hledger.Gain]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        (Just depth)

expensePeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    ReportWindow ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
expensePeriodicReport snapshot transactions granularity window commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        window
        commodity
        [Hledger.Expense]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        (Just depth)
