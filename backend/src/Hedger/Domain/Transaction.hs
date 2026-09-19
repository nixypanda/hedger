module Hedger.Domain.Transaction (
    -- * Types
    TransactionClassification (..),
    TransactionPresentation (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    isOpening,
) where

import Hedger.Domain.Internal.Transaction
