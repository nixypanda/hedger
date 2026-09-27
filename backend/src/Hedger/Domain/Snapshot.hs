module Hedger.Domain.Snapshot (
    Snapshot (journal, transactions, accountTree, latestJournalDate, revision),
    mkSnapshot,
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.Account qualified as HledgerAccount
import Hledger.Data.Types qualified as Hledger

data Snapshot = Snapshot
    { journal :: Hledger.Journal
    , transactions :: [Hledger.Transaction]
    , accountTree :: Hledger.Account Hledger.BalanceData
    , latestJournalDate :: Maybe Day
    , revision :: Text
    }

-- | Derive every journal-dependent view when the snapshot is constructed.
mkSnapshot :: Hledger.Journal -> Text -> Snapshot
mkSnapshot source revision =
    Snapshot
        { journal = source
        , transactions = records
        , accountTree = HledgerAccount.accountFromPostings postingDate postings
        , latestJournalDate = maximumMaybe (Hledger.tdate <$> records)
        , revision = revision
        }
  where
    records = source.jtxns
    postings = concatMap Hledger.tpostings records
    postingDate posting = posting.pdate <|> (Hledger.tdate <$> posting.ptransaction)

maximumMaybe :: (Ord value) => [value] -> Maybe value
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)

instance Show Snapshot where
    show snapshot = "Snapshot{revision=" <> show snapshot.revision <> "}"
