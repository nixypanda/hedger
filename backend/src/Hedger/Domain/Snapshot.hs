module Hedger.Domain.Snapshot (
    Snapshot (..),
) where

import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.Types qualified as Hledger

data Snapshot = Snapshot
    { journal :: Hledger.Journal
    , transactions :: [Hledger.Transaction]
    , accountTree :: Hledger.Account Hledger.BalanceData
    , latestJournalDate :: Maybe Day
    , revision :: Text
    }

instance Show Snapshot where
    show snapshot = "Snapshot{revision=" <> show snapshot.revision <> "}"
