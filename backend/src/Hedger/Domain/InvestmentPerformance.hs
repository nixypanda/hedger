module Hedger.Domain.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary,
    Completeness,
    SummaryResult,
    summaryMarketValue,
    summaryPurchaseCost,
    summaryGainLoss,
    summaryXirr,
    summaryAttributedIncome,
    summaryRealisedGain,
    summaryTotalProfit,
    summaryWarnings,
    summaryCompleteness,
    InvestmentWarning (..),
    WarningKind (..),
    InvestmentPerformanceContext,

    -- * Performance calculation
    prepareInvestmentPerformance,
    summaryAtPrepared,
    summariesAtPrepared,
    summariesAtScopesPrepared,
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
) where

import Hedger.Domain.Internal.InvestmentPerformance
