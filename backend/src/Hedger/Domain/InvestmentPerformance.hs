module Hedger.Domain.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    InvestmentWarning (..),
    WarningKind (..),
    SummaryResult (..),
    InvestmentPerformanceContext,

    -- * Performance calculation
    prepareInvestmentPerformance,
    summaryAtPrepared,
    summariesAtPrepared,
    summaryAt,
    investmentSummaryAt,

    -- * Ledger policies and derivations
    validateScope,
    isInvestmentAccount,
    investmentAccountsAt,
    investmentDataTransactions,
    immediateChildren,
    childrenOf,
    reportPeriods,
    externalCashFlowsAt,
)
where

import Hedger.Domain.Internal.InvestmentPerformance
