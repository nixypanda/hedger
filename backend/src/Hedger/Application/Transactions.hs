module Hedger.Application.Transactions (
    TransactionList (..),
    transactionList,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (AppM, SnapshotContext (..), requireSnapshot)
import Hedger.Application.Common (
    AccountTypeClassifier,
    SnapshotMetadata,
    accountTypeClassifier,
    metadataFor,
 )
import Hedger.Application.TransactionQuery (TransactionPageRequest (..))
import Hedger.Application.TransactionQuery qualified as TransactionQuery
import Hedger.Domain.Snapshot (Snapshot (..))

data TransactionList = TransactionList
    { metadata :: SnapshotMetadata
    , transactions :: [Hledger.Transaction]
    , nextCursor :: Maybe Text
    , classifyAccount :: AccountTypeClassifier
    }

transactionList :: TransactionPageRequest -> AppM TransactionList
transactionList request = do
    snapshotContext <- requireSnapshot
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
    page <- either throwError pure (TransactionQuery.transactionPage snapshot request)
    pure
        TransactionList
            { metadata = responseMetadata
            , transactions = page.transactions
            , nextCursor = page.nextCursor
            , classifyAccount = accountTypeClassifier snapshot.journal
            }
