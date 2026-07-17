module Hedger.Domain.Transaction (
    -- * Types
    CommodityQuantity (..),
    TransactionClassification (..),
    TransactionPresentation (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    isOpening,
    fromAccountsFor,
    toAccountsFor,
    dominantToAccountFor,
)
where

import Hedger.Domain.Internal.Transaction
