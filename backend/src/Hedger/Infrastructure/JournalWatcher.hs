{- | Automatic journal reloading.

The watcher listens for filesystem events (via @fsnotify@: FSEvents on macOS,
inotify on Linux) on every file that made up the current snapshot — the root
journal plus any files it @include@s — and asks the 'SnapshotStore' to reload
when one of them changes. It is event-driven, so an idle server does no work.

Three cases need care and are handled here:

  * Editors often save atomically (write a temporary file, then rename over the
    original). The membership check is on the final path, and any event that
    replaces a watched file wakes the reloader.

  * @include@ sets grow and shrink between reloads. After every reload the watch
    set is re-derived from the fresh snapshot and the watched directories are
    reconciled, so newly included files start being watched and removed ones
    stop.

  * A single save can emit several events. A short debounce coalesces a burst
    into one reload.

@fsnotify@ watches directories, not individual files, so we watch each distinct
parent directory and filter events down to the exact set of journal files.
-}
module Hedger.Infrastructure.JournalWatcher (
    watchTargets,
    withJournalWatcher,
)
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Exception (finally)
import Control.Monad (forever, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.FSNotify (
    Event,
    WatchManager,
    eventPath,
    watchDir,
    withManager,
 )
import System.FilePath (takeDirectory)

import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )

import Hledger.Data.Types qualified as Hledger

{- | How long to wait after the first event of a burst before reloading, so a
single save that emits several events triggers just one reload.
-}
debounceMicroseconds :: Int
debounceMicroseconds = 100_000

withJournalWatcher :: SnapshotStore -> Text -> IO a -> IO a
withJournalWatcher store journalPath action =
    withManager $ \manager -> do
        watchedDirs <- newIORef Map.empty
        targets <- newIORef Set.empty
        wakeReloader <- newTVarIO False
        let requestReload = atomically (writeTVar wakeReloader True)
        reconcileWatches manager store journalPath watchedDirs targets requestReload
        reloader <-
            forkIO (runReloader store journalPath manager watchedDirs targets requestReload wakeReloader)
        action `finally` killThread reloader

runReloader ::
    SnapshotStore ->
    Text ->
    WatchManager ->
    IORef (Map FilePath (IO ())) ->
    IORef (Set FilePath) ->
    IO () ->
    TVar Bool ->
    IO ()
runReloader store journalPath manager watchedDirs targets requestReload wakeReloader =
    forever $ do
        atomically (readTVar wakeReloader >>= check)
        threadDelay debounceMicroseconds
        -- Clear the flag so events coalesced during the debounce window are
        -- covered by the reload below; any that arrive afterwards set it again
        -- and drive a fresh cycle, so no change is missed.
        atomically (writeTVar wakeReloader False)
        void (reloadSnapshot store journalPath)
        reconcileWatches manager store journalPath watchedDirs targets requestReload

reconcileWatches ::
    WatchManager ->
    SnapshotStore ->
    Text ->
    IORef (Map FilePath (IO ())) ->
    IORef (Set FilePath) ->
    IO () ->
    IO ()
reconcileWatches manager store journalPath watchedDirs targets requestReload = do
    files <- watchTargets store journalPath
    writeIORef targets (Set.fromList files)
    let desiredDirs = Set.fromList (takeDirectory <$> files)
    existing <- readIORef watchedDirs
    let toStop = Map.withoutKeys existing desiredDirs
        keptDirs = Map.restrictKeys existing desiredDirs
        toStart = Set.difference desiredDirs (Map.keysSet existing)
    sequence_ (Map.elems toStop)
    started <-
        traverse
            (\dir -> (,) dir <$> watchDir manager dir (const True) (handleEvent targets requestReload))
            (Set.toList toStart)
    writeIORef watchedDirs (Map.union keptDirs (Map.fromList started))

handleEvent :: IORef (Set FilePath) -> IO () -> Event -> IO ()
handleEvent targets requestReload event = do
    watched <- readIORef targets
    when (eventPath event `Set.member` watched) requestReload

-- The root path is always included so a journal that currently fails to parse
-- is still watched and picked up once it parses again.
watchTargets :: SnapshotStore -> Text -> IO [FilePath]
watchTargets store journalPath = do
    snapshot <- readCurrentSnapshot store
    let included = maybe [] (fmap fst . (.jfiles) . (.journal)) snapshot
    root <- canonicalize (Text.unpack journalPath)
    absoluteIncluded <- traverse canonicalize included
    pure (nub (root : absoluteIncluded))

{- | Resolve to a canonical path so event paths (which the OS reports resolved)
compare equal to our target set even when the journal lives under a symlink.
-}
canonicalize :: FilePath -> IO FilePath
canonicalize = Directory.canonicalizePath
