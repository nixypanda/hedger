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
    summariesAtScopesPrepared,
    summaryAt,
    investmentSummaryAt,

    -- * Checkpointed performance calculation
    ScopeCache (..),
    ScopeCheckpoints,
    SolvedRates,
    emptyScopeCache,
    emptyCheckpoints,
    emptySolvedRates,
    truncateScopeCache,
    summariesAtResumed,
    summariesAtScopesResumed,

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

import Hedger.Domain.Internal.InvestmentCheckpoint (
    ScopeCache (..),
    ScopeCheckpoints,
    SolvedRates,
    emptyCheckpoints,
    emptyScopeCache,
    emptySolvedRates,
    summariesAtResumed,
    summariesAtScopesResumed,
    truncateScopeCache,
 )
import Hedger.Domain.Internal.InvestmentPerformance
