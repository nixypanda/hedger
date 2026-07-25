module Hedger.Domain.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    InvestmentWarning (..),
    WarningKind (..),
    SummaryResult (..),

    -- * Performance calculation
    summaryAt,
    investmentSummaryAt,

    -- * Ledger policies and derivations
    validateScope,
    isInvestmentAccount,
    investmentDataTransactions,
    immediateChildren,
    reportPeriods,
)
where

import Hedger.Domain.Internal.InvestmentPerformance
