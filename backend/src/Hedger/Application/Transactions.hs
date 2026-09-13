module Hedger.Application.Transactions (
    TransactionItem (..),
    TransactionList (..),
    transactionItem,
    transactionList,
)
where

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
import Hedger.Domain.Transaction (TransactionClassification)
import Hedger.Domain.Transaction qualified as Transaction

data TransactionItem = TransactionItem
    { transaction :: Hledger.Transaction
    , classification :: TransactionClassification
    }
    deriving stock (Show)

data TransactionList = TransactionList
    { metadata :: SnapshotMetadata
    , transactions :: [TransactionItem]
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
            , transactions = transactionItem <$> page.transactions
            , nextCursor = page.nextCursor
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

transactionItem :: Hledger.Transaction -> TransactionItem
transactionItem transaction =
    TransactionItem
        { transaction
        , classification = Transaction.classificationFor transaction
        }
