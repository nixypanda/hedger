module Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    SnapshotStoreRead (..),
    SnapshotStore,
    snapshotJournalPath,
    awaitRevisionChange,
    currentRevision,
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    readSnapshotStore,
    reloadSnapshot,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
 )
import Control.Exception (
    SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    tryJust,
 )
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory qualified as Directory

import Hledger.Data.Types qualified as Hledger
import Hledger.Read qualified as Hledger

import Hedger.Domain.Snapshot (Snapshot (..), mkSnapshot)

-- Store model
newtype SnapshotLoadError
    = JournalReadFailed Text
    deriving stock (Eq, Show)

data SnapshotStoreRead
    = SnapshotNotLoaded
    | SnapshotLoadFailed SnapshotLoadError
    | SnapshotFresh Snapshot
    | SnapshotStale Snapshot SnapshotLoadError

{- | The store state is kept in one transactional variable so a snapshot and
its published revision cannot be observed independently.
-}
data SnapshotStore = SnapshotStore
    { storeState :: TVar SnapshotStoreRead
    , journalPath :: Text
    }

-- Store operations
newSnapshotStore :: Text -> IO SnapshotStore
newSnapshotStore path = SnapshotStore <$> newTVarIO SnapshotNotLoaded <*> pure path

snapshotJournalPath :: SnapshotStore -> Text
snapshotJournalPath = (.journalPath)

readCurrentSnapshot :: SnapshotStore -> IO (Maybe Snapshot)
readCurrentSnapshot SnapshotStore{..} = do
    state <- readTVarIO storeState
    pure $
        case state of
            SnapshotNotLoaded -> Nothing
            SnapshotLoadFailed _ -> Nothing
            SnapshotFresh snapshot -> Just snapshot
            SnapshotStale snapshot _ -> Just snapshot

readLastReloadError :: SnapshotStore -> IO (Maybe SnapshotLoadError)
readLastReloadError SnapshotStore{..} = do
    state <- readTVarIO storeState
    pure $
        case state of
            SnapshotNotLoaded -> Nothing
            SnapshotLoadFailed loadError -> Just loadError
            SnapshotFresh _ -> Nothing
            SnapshotStale _ loadError -> Just loadError

readSnapshotStore :: SnapshotStore -> IO SnapshotStoreRead
readSnapshotStore SnapshotStore{..} = readTVarIO storeState

reloadSnapshot :: SnapshotStore -> IO (Either SnapshotLoadError Snapshot)
reloadSnapshot SnapshotStore{..} = do
    result <- loadSnapshot journalPath
    atomically (modifyTVar' storeState (storeReloadResult result))
    pure result

-- | The revision currently published, or @""@ before the first successful load.
currentRevision :: SnapshotStore -> IO Text
currentRevision SnapshotStore{..} = storeRevision <$> readTVarIO storeState

-- Blocks until a revision different from @seen@ is published, so a caller can
-- react to reloads without polling.
awaitRevisionChange :: SnapshotStore -> Text -> IO Text
awaitRevisionChange SnapshotStore{..} seen =
    atomically $ do
        revision <- storeRevision <$> readTVar storeState
        check (revision /= seen)
        pure revision

-- State projections and transitions
storeReloadResult :: Either SnapshotLoadError Snapshot -> SnapshotStoreRead -> SnapshotStoreRead
storeReloadResult result state =
    case result of
        Right snapshot -> SnapshotFresh snapshot
        Left loadError ->
            case state of
                SnapshotNotLoaded -> SnapshotLoadFailed loadError
                SnapshotLoadFailed _ -> SnapshotLoadFailed loadError
                SnapshotFresh snapshot -> SnapshotStale snapshot loadError
                SnapshotStale snapshot _ -> SnapshotStale snapshot loadError

storeRevision :: SnapshotStoreRead -> Text
storeRevision state =
    case state of
        SnapshotNotLoaded -> ""
        SnapshotLoadFailed _ -> ""
        SnapshotFresh snapshot -> snapshot.revision
        SnapshotStale snapshot _ -> snapshot.revision

-- Snapshot loading
loadSnapshot :: Text -> IO (Either SnapshotLoadError Snapshot)
loadSnapshot configuredPath = do
    readResult <- readJournal configuredPath
    pure $ do
        journalResult <-
            either (Left . JournalReadFailed . Text.pack . displayException) Right readResult
        loadedJournal <- either (Left . JournalReadFailed . Text.pack) Right journalResult
        pure (mkSnapshot loadedJournal (journalRevision loadedJournal))

readJournal :: Text -> IO (Either SomeException (Either String Hledger.Journal))
readJournal configuredPath =
    tryJust synchronousException $ do
        absolutePath <- Directory.makeAbsolute (Text.unpack configuredPath)
        Hledger.runExceptT (Hledger.readJournalFile Hledger.definputopts absolutePath)

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> Nothing
        Nothing -> Just exception

-- Revision hashing
journalRevision :: Hledger.Journal -> Text
journalRevision journal = "md5:" <> hexEncode (MD5.hash revisionInput)
  where
    revisionInput =
        TextEncoding.encodeUtf8
            ( Text.concat
                ( "read-options:definputopts-v1\n"
                    : (lengthDelimited . snd <$> journal.jfiles)
                )
            )

lengthDelimited :: Text -> Text
lengthDelimited value = Text.pack (show (Text.length value)) <> ":" <> value

hexEncode :: ByteString.ByteString -> Text
hexEncode = Text.pack . concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte =
    case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits
