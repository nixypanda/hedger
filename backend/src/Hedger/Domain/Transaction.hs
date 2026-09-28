module Hedger.Domain.Transaction (
    -- * Types
    TransactionClassification (..),

    -- * Core API
    newestFirst,

    -- * Domain queries
    classificationFor,
    isOpening,
) where

import Hedger.Domain.Internal.Transaction
