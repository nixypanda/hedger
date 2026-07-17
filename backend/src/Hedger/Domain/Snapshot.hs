module Hedger.Domain.Snapshot (
    Snapshot (..),
)
where

import Data.Text (Text)
import Data.Time (Day)
import Hledger.Data.Types qualified as Hledger

data Snapshot = Snapshot
    { journal :: Hledger.Journal
    , transactions :: [Hledger.Transaction]
    , latestJournalDate :: Maybe Day
    , revision :: Text
    }
    deriving (Eq)

instance Show Snapshot where
    show snapshot = "Snapshot{revision=" <> show snapshot.revision <> "}"
