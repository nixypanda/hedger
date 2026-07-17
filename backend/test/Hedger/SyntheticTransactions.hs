module Hedger.SyntheticTransactions (
    mkSnapshot,
) where

import Hedger.Domain.Snapshot (Snapshot (..))
import Hledger.Data.Journal (nulljournal)
import Hledger.Data.Types qualified as Hledger

mkSnapshot :: [Hledger.Transaction] -> Snapshot
mkSnapshot txns =
    Snapshot
        { journal = nulljournal
        , transactions = txns
        , latestJournalDate = maximumMaybe (Hledger.tdate <$> txns)
        , revision = "md5:synthetic"
        }

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just (maximum xs)
