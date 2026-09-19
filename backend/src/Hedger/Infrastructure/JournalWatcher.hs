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
    WatchState (..),
    guardReloaderStep,
    watchTargets,
    withJournalWatcher,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Exception (
    SomeAsyncException,
    SomeException,
    displayException,
    finally,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory qualified as Directory
import System.FSNotify (Event, WatchManager, eventPath, watchDir, withManager)
import System.FilePath (takeDirectory)

import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    SnapshotStore (..),
    readCurrentSnapshot,
    reloadSnapshot,
 )

import Hledger.Data.Types qualified as Hledger

data WatchState = WatchState
    { watchedDirs :: IORef (Map FilePath (IO ()))
    , watchTargets :: IORef (Set FilePath)
    }

{- | How long to wait after the first event of a burst before reloading, so a
single save that emits several events triggers just one reload.
-}
debounceMicroseconds :: Int
debounceMicroseconds = 100_000

withJournalWatcher :: SnapshotStore -> IO a -> IO a
withJournalWatcher store action =
    withManager $ \manager -> do
        watchedDirs' <- newIORef Map.empty
        watchTargets' <- newIORef Set.empty
        let ws = WatchState watchedDirs' watchTargets'
        wakeReloader <- newTVarIO False
        let requestReload = atomically (writeTVar wakeReloader True)
        reconcileWatches manager store ws requestReload
        reloader <- forkIO (runReloader store manager ws requestReload wakeReloader)
        action `finally` killThread reloader

{- | Runs 'reloadCycle' forever, guarding each iteration with 'guardReloaderStep'
so a synchronous exception (e.g. a watched path vanishing mid-reconcile during
an atomic-rename storm) is logged and the loop keeps going instead of the
reloader thread dying silently.
-}
runReloader :: SnapshotStore -> WatchManager -> WatchState -> IO () -> TVar Bool -> IO ()
runReloader store manager ws requestReload wakeReloader = forever (guardReloaderStep step)
  where
    step = reloadCycle store manager ws requestReload wakeReloader

reloadCycle :: SnapshotStore -> WatchManager -> WatchState -> IO () -> TVar Bool -> IO ()
reloadCycle store manager ws requestReload wakeReloader = do
    atomically (readTVar wakeReloader >>= check)
    threadDelay debounceMicroseconds
    -- Clear the flag so events coalesced during the debounce window are
    -- covered by the reload below; any that arrive afterwards set it again
    -- and drive a fresh cycle, so no change is missed.
    atomically (writeTVar wakeReloader False)
    result <- reloadSnapshot store
    Text.putStrLn (reloadMessage result)
    reconcileWatches manager store ws requestReload

{- | Runs an action, catching and logging a synchronous exception so the
caller (the reloader's @forever@ loop) can continue instead of dying.
Asynchronous exceptions — notably the @ThreadKilled@ that 'withJournalWatcher'
delivers via @killThread@ to stop the watcher — are rethrown untouched so the
reloader thread stays killable.
-}
guardReloaderStep :: IO () -> IO ()
guardReloaderStep action = do
    outcome <- try action
    case outcome of
        Right () -> pure ()
        Left exception
            | isAsynchronous exception -> throwIO exception
            | otherwise ->
                Text.putStrLn
                    ( "Journal watcher reconcile failed; retrying: "
                        <> Text.pack (displayException exception)
                    )

isAsynchronous :: SomeException -> Bool
isAsynchronous exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> True
        Nothing -> False

reloadMessage :: Either SnapshotLoadError Snapshot -> Text
reloadMessage result =
    case result of
        Right snapshot ->
            "Reloaded journal after a file change (revision "
                <> snapshot.revision
                <> ")."
        Left (JournalReadFailed problem) ->
            "Journal reload after a file change failed; keeping the previous snapshot: " <> problem

reconcileWatches :: WatchManager -> SnapshotStore -> WatchState -> IO () -> IO ()
reconcileWatches manager store ws requestReload = do
    files <- watchTargets store
    writeIORef ws.watchTargets (Set.fromList files)
    let desiredDirs = Set.fromList (takeDirectory <$> files)
    existing <- readIORef ws.watchedDirs
    let toStop = Map.withoutKeys existing desiredDirs
        keptDirs = Map.restrictKeys existing desiredDirs
        toStart = Set.difference desiredDirs (Map.keysSet existing)
    sequence_ (Map.elems toStop)
    started <-
        traverse
            (\dir -> (,) dir <$> watchDir manager dir (const True) (handleEvent ws requestReload))
            (Set.toList toStart)
    writeIORef ws.watchedDirs (Map.union keptDirs (Map.fromList started))

handleEvent :: WatchState -> IO () -> Event -> IO ()
handleEvent ws requestReload event = do
    watched <- readIORef ws.watchTargets
    when (eventPath event `Set.member` watched) requestReload

-- The root path is always included so a journal that currently fails to parse
-- is still watched and picked up once it parses again.
watchTargets :: SnapshotStore -> IO [FilePath]
watchTargets SnapshotStore{..} = do
    snapshot <- readCurrentSnapshot SnapshotStore{..}
    let included = maybe [] (fmap fst . (.jfiles) . (.journal)) snapshot
    root <- canonicalize (Text.unpack journalPath)
    absoluteIncluded <- traverse canonicalize included
    pure (nub (root : absoluteIncluded))

{- | Resolve to a canonical path so event paths (which the OS reports resolved)
compare equal to our target set even when the journal lives under a symlink.
-}
canonicalize :: FilePath -> IO FilePath
canonicalize = Directory.canonicalizePath
