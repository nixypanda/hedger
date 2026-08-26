{- | Cashflow-specific report construction. Keeping these report definitions
here lets the calculation and its component benchmarks exercise the same
hledger options.
-}
module Hedger.Domain.Internal.Cashflow (
    incomeGainPeriodicReport,
    expensePeriodicReport,
)
where

import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (periodicBalanceReport)
import Hedger.Domain.Period (Granularity)
import Hedger.Domain.Snapshot (Snapshot)

incomeGainPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
incomeGainPeriodicReport snapshot transactions granularity commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        [Hledger.Revenue, Hledger.Gain]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        depth

expensePeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
expensePeriodicReport snapshot transactions granularity commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        [Hledger.Expense]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        depth
