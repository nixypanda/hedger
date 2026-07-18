module Hedger.Application.Transactions (
    TransactionList (..),
    transactionList,
)
where

import Control.Monad.Except (throwError)
import Data.Text (Text)

import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (AppM, requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Application.TransactionQuery (TransactionPageRequest (..))
import Hedger.Application.TransactionQuery qualified as TransactionQuery

data TransactionList = TransactionList
    { metadata :: SnapshotMetadata
    , transactions :: [Hledger.Transaction]
    , nextCursor :: Maybe Text
    }

transactionList :: TransactionPageRequest -> AppM TransactionList
transactionList request = do
    snapshot <- requireSnapshot
    page <- either throwError pure (TransactionQuery.transactionPage snapshot request)
    responseMetadata <- metadataFor snapshot
    pure
        TransactionList
            { metadata = responseMetadata
            , transactions = page.transactions
            , nextCursor = page.nextCursor
            }
