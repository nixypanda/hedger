module Hedger.SyntheticTransactions (
    mkSnapshot,
) where

import Hledger.Data.Journal (nulljournal)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Snapshot (Snapshot)
import Hedger.Domain.Snapshot qualified as Snapshot

mkSnapshot :: [Hledger.Transaction] -> Snapshot
mkSnapshot txns =
    Snapshot.mkSnapshot nulljournal{Hledger.jtxns = txns} "md5:synthetic"
