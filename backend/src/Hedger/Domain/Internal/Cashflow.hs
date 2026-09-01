{- | Cashflow-specific report construction. Keeping these report definitions
here lets the calculation and its component benchmarks exercise the same
hledger options.
-}
module Hedger.Domain.Internal.Cashflow (
    cashflowAccountTypes,
    cashflowReportPeriods,
    incomeExpensePeriodicReport,
    incomeGainPeriodicReport,
    expensePeriodicReport,
)
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (addDays)

import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (periodicBalanceReport, reportWindow)
import Hedger.Domain.Period (Granularity, ReportPeriod, epoch, periodsFromDateSpan)
import Hedger.Domain.Snapshot (Snapshot (..))

-- | The account types that make up a cashflow report, both sides together.
cashflowAccountTypes :: [Hledger.AccountType]
cashflowAccountTypes = [Hledger.Revenue, Hledger.Gain, Hledger.Expense]

{- | Both cashflow sides in a single report; 'Hedger.Domain.Internal.PeriodReport.reportSide'
splits it back apart. Deliberately unclipped, which that function's docs require.
-}
incomeExpensePeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    HledgerMultiBalance.MultiBalanceReport
incomeExpensePeriodicReport snapshot transactions granularity commodity =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        cashflowAccountTypes
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        Nothing

{- | The periods 'incomeExpensePeriodicReport' will produce, without building it.

Needed by the checkpointed path, which has to know which periods a request covers
before it can decide which of them it already holds. The window itself comes from
the same 'reportWindow' the report uses, so the two cannot drift: only the report
spec handed to it is rebuilt here, and 'reportWindow' reads just the query, the
reference day and the granularity out of it. Everything else
'periodicBalanceReport' puts in its own spec — the depth, the emptiness flag, the
balance calculation and accumulation — is invisible to window selection.
-}
cashflowReportPeriods :: Snapshot -> [Hledger.Transaction] -> Granularity -> [ReportPeriod]
cashflowReportPeriods snapshot transactions granularity =
    periodsFromDateSpan granularity (HledgerPeriod.periodAsDateSpan window)
  where
    (window, _) = reportWindow granularity flowJournal windowSpec
    flowJournal = snapshot.journal{Hledger.jtxns = transactions}
    windowSpec =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsDay = fromMaybe epoch snapshot.latestJournalDate
            , HledgerReportOptions._rsQuery = windowQuery
            }
    windowQuery =
        HledgerQuery.And
            ( HledgerQuery.Type cashflowAccountTypes
                : case snapshot.latestJournalDate of
                    Just latest ->
                        [ HledgerQuery.Date
                            (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 latest)))
                        ]
                    Nothing -> []
            )

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
        (Just depth)

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
        (Just depth)
