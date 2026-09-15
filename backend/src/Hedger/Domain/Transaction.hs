module Hedger.Domain.Transaction (
    -- * Types
    TransactionClassification (..),

    -- * Domain queries
    classificationFor,
    isOpening,
)
where

import Hedger.Domain.Internal.Transaction
