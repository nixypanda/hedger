module Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    SnapshotStoreRead (..),
    SnapshotStore,
    awaitRevisionChange,
    currentRevision,
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    readSnapshotStore,
    reloadSnapshot,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, tryJust)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory qualified as Directory

import Hledger.Data.Account qualified as HledgerAccount
import Hledger.Data.Types qualified as Hledger
import Hledger.Read qualified as Hledger

import Hedger.Domain.Snapshot (Snapshot (..))

newtype SnapshotLoadError = JournalReadFailed Text
    deriving stock (Eq, Show)

data SnapshotStoreRead
    = SnapshotNotLoaded
    | SnapshotLoadFailed SnapshotLoadError
    | SnapshotFresh Snapshot
    | SnapshotStale Snapshot SnapshotLoadError

{- | Updates to the @IORef@ state and the @TVar@ revision are serialized by
@updateLock@. A successful snapshot is stored before its revision is published,
so revision subscribers cannot wake before the corresponding snapshot is ready.
-}
data SnapshotStore = SnapshotStore (IORef SnapshotStoreRead) (TVar Text) (MVar ())

newSnapshotStore :: IO SnapshotStore
newSnapshotStore =
    SnapshotStore
        <$> newIORef SnapshotNotLoaded
        <*> newTVarIO ""
        <*> newMVar ()

readCurrentSnapshot :: SnapshotStore -> IO (Maybe Snapshot)
readCurrentSnapshot (SnapshotStore stateReference _ _) = do
    state <- readIORef stateReference
    pure $ case state of
        SnapshotNotLoaded -> Nothing
        SnapshotLoadFailed _ -> Nothing
        SnapshotFresh snapshot -> Just snapshot
        SnapshotStale snapshot _ -> Just snapshot

readLastReloadError :: SnapshotStore -> IO (Maybe SnapshotLoadError)
readLastReloadError (SnapshotStore stateReference _ _) = do
    state <- readIORef stateReference
    pure $ case state of
        SnapshotNotLoaded -> Nothing
        SnapshotLoadFailed loadError -> Just loadError
        SnapshotFresh _ -> Nothing
        SnapshotStale _ loadError -> Just loadError

readSnapshotStore :: SnapshotStore -> IO SnapshotStoreRead
readSnapshotStore (SnapshotStore stateReference _ _) = readIORef stateReference

reloadSnapshot :: SnapshotStore -> Text -> IO (Either SnapshotLoadError Snapshot)
reloadSnapshot (SnapshotStore stateReference revisionVar updateLock) path = do
    result <- loadSnapshot path
    withMVar updateLock $ \_ -> do
        atomicModifyIORef' stateReference $ \state ->
            case result of
                Left loadError ->
                    ( case state of
                        SnapshotNotLoaded -> SnapshotLoadFailed loadError
                        SnapshotLoadFailed _ -> SnapshotLoadFailed loadError
                        SnapshotFresh snapshot -> SnapshotStale snapshot loadError
                        SnapshotStale snapshot _ -> SnapshotStale snapshot loadError
                    , ()
                    )
                Right snapshot -> (SnapshotFresh snapshot, ())
        case result of
            Right snapshot -> atomically (writeTVar revisionVar snapshot.revision)
            Left _ -> pure ()
    pure result

-- | The revision currently published, or @""@ before the first successful load.
currentRevision :: SnapshotStore -> IO Text
currentRevision (SnapshotStore _ revisionVar _) = readTVarIO revisionVar

-- Blocks until a revision different from @seen@ is published, so a caller can
-- react to reloads without polling.
awaitRevisionChange :: SnapshotStore -> Text -> IO Text
awaitRevisionChange (SnapshotStore _ revisionVar _) seen =
    atomically $ do
        revision <- readTVar revisionVar
        check (revision /= seen)
        pure revision

loadSnapshot :: Text -> IO (Either SnapshotLoadError Snapshot)
loadSnapshot configuredPath = do
    readResult <- readJournal configuredPath
    pure $ do
        journalResult <- either (Left . JournalReadFailed . Text.pack . displayException) Right readResult
        loadedJournal <- either (Left . JournalReadFailed . Text.pack) Right journalResult
        let records = loadedJournal.jtxns
            postings = concatMap Hledger.tpostings records
        pure
            Snapshot
                { journal = loadedJournal
                , transactions = records
                , accountTree = HledgerAccount.accountFromPostings postingDate postings
                , latestJournalDate = maximumMaybe (Hledger.tdate <$> records)
                , revision = journalRevision loadedJournal
                }
  where
    postingDate posting = posting.pdate <|> (Hledger.tdate <$> posting.ptransaction)

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

journalRevision :: Hledger.Journal -> Text
journalRevision journal =
    "md5:" <> hexEncode (MD5.hash revisionInput)
  where
    revisionInput =
        TextEncoding.encodeUtf8
            (Text.concat ("read-options:definputopts-v1\n" : (lengthDelimited . snd <$> journal.jfiles)))

lengthDelimited :: Text -> Text
lengthDelimited value = Text.pack (show (Text.length value)) <> ":" <> value

hexEncode :: ByteString.ByteString -> Text
hexEncode = Text.pack . concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte =
    case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits
