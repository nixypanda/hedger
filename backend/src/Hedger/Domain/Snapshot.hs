module Hedger.Domain.Snapshot (
    Snapshot (..),
    SnapshotLoadError (..),
    SnapshotStore,
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    reloadSnapshot,
)
where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, tryJust)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Hledger.Data.Types qualified as Hledger
import Hledger.Read qualified as Hledger
import System.Directory qualified as Directory

data Snapshot = Snapshot
    { journal :: Hledger.Journal
    , transactions :: [Hledger.Transaction]
    , latestJournalDate :: Maybe Day
    }
    deriving (Eq, Show)

newtype SnapshotLoadError = JournalReadFailed Text
    deriving (Eq, Show)

data SnapshotState = SnapshotState
    { currentSnapshot :: Maybe Snapshot
    , lastReloadError :: Maybe SnapshotLoadError
    }

newtype SnapshotStore = SnapshotStore (IORef SnapshotState)

newSnapshotStore :: IO SnapshotStore
newSnapshotStore = SnapshotStore <$> newIORef SnapshotState{currentSnapshot = Nothing, lastReloadError = Nothing}

readCurrentSnapshot :: SnapshotStore -> IO (Maybe Snapshot)
readCurrentSnapshot (SnapshotStore stateReference) = (.currentSnapshot) <$> readIORef stateReference

readLastReloadError :: SnapshotStore -> IO (Maybe SnapshotLoadError)
readLastReloadError (SnapshotStore stateReference) = (.lastReloadError) <$> readIORef stateReference

{- | Load and validate a replacement before publishing it. A failed reload only
records the error, leaving readers on the previous immutable snapshot.
-}
reloadSnapshot :: SnapshotStore -> Text -> IO (Either SnapshotLoadError Snapshot)
reloadSnapshot (SnapshotStore stateReference) path = do
    result <- loadSnapshot path
    atomicModifyIORef' stateReference $ \state ->
        case result of
            Left loadError -> (state{lastReloadError = Just loadError}, ())
            Right snapshot -> (SnapshotState{currentSnapshot = Just snapshot, lastReloadError = Nothing}, ())
    pure result

loadSnapshot :: Text -> IO (Either SnapshotLoadError Snapshot)
loadSnapshot configuredPath = do
    readResult <- readJournal configuredPath
    pure $ do
        journalResult <- either (Left . JournalReadFailed . Text.pack . displayException) Right readResult
        loadedJournal <- either (Left . JournalReadFailed . Text.pack) Right journalResult
        let records = loadedJournal.jtxns
        pure
            Snapshot
                { journal = loadedJournal
                , transactions = records
                , latestJournalDate = maximumMaybe (Hledger.tdate <$> records)
                }

readJournal :: Text -> IO (Either SomeException (Either String Hledger.Journal))
readJournal configuredPath = tryJust synchronousException $ do
    absolutePath <- Directory.makeAbsolute (Text.unpack configuredPath)
    Hledger.runExceptT (Hledger.readJournalFile Hledger.definputopts absolutePath)

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> Nothing
        Nothing -> Just exception

maximumMaybe :: (Ord value) => [value] -> Maybe value
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)
