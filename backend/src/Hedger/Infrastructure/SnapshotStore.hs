module Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    SnapshotStoreRead (..),
    SnapshotStore,
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
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
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

data SnapshotState = SnapshotState
    { currentSnapshot :: Maybe Snapshot
    , lastReloadError :: Maybe SnapshotLoadError
    }

data SnapshotStoreRead = SnapshotStoreRead
    { snapshot :: Maybe Snapshot
    , reloadError :: Maybe SnapshotLoadError
    }

{- | The @IORef@ snapshot and the @TVar@ revision are only ever written together
while holding @updateLock@, so a reader never observes a revision published
for a different reload than the snapshot it accompanies.
-}
data SnapshotStore = SnapshotStore (IORef SnapshotState) (TVar Text) (MVar ())

newSnapshotStore :: IO SnapshotStore
newSnapshotStore =
    SnapshotStore
        <$> newIORef SnapshotState{currentSnapshot = Nothing, lastReloadError = Nothing}
        <*> newTVarIO ""
        <*> newMVar ()

readCurrentSnapshot :: SnapshotStore -> IO (Maybe Snapshot)
readCurrentSnapshot (SnapshotStore stateReference _ _) = (.currentSnapshot) <$> readIORef stateReference

readLastReloadError :: SnapshotStore -> IO (Maybe SnapshotLoadError)
readLastReloadError (SnapshotStore stateReference _ _) = (.lastReloadError) <$> readIORef stateReference

readSnapshotStore :: SnapshotStore -> IO SnapshotStoreRead
readSnapshotStore (SnapshotStore stateReference _ _) = do
    state <- readIORef stateReference
    pure
        SnapshotStoreRead
            { snapshot = state.currentSnapshot
            , reloadError = state.lastReloadError
            }

reloadSnapshot :: SnapshotStore -> Text -> IO (Either SnapshotLoadError Snapshot)
reloadSnapshot (SnapshotStore stateReference revisionVar updateLock) path = do
    result <- loadSnapshot path
    withMVar updateLock $ \_ -> do
        atomicModifyIORef' stateReference $ \state ->
            case result of
                Left loadError -> (state{lastReloadError = Just loadError}, ())
                Right snapshot ->
                    (SnapshotState{currentSnapshot = Just snapshot, lastReloadError = Nothing}, ())
        case result of
            Right snapshot -> atomically (writeTVar revisionVar snapshot.revision)
            Left _ -> pure ()
    pure result

{- | The revision currently published, or @""@ before the first successful load.

Nothing blocks on this. Clients follow the /derived/ revision instead
("Hedger.Application.Events"), because being told to refetch the moment the
journal parses points every one of them at analytics that have not been rebuilt
yet.
-}
currentRevision :: SnapshotStore -> IO Text
currentRevision (SnapshotStore _ revisionVar _) = readTVarIO revisionVar

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
