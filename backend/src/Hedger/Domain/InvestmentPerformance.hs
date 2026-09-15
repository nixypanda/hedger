module Hedger.Domain.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    AssetClassAllocation (..),
    InvestmentWarning (..),
    WarningKind (..),
    SummaryResult (..),
    InvestmentPerformanceContext,
    InvestmentScopeError (..),

    -- * Performance calculation
    prepareInvestmentPerformance,
    summaryAtPrepared,
    summariesAtPrepared,
    summariesAtScopesPrepared,
    assetClassAllocations,
    allocationPercentage,
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
