{- | Automatic journal reloading.

The watcher listens for filesystem events (via @fsnotify@: FSEvents on macOS,
inotify on Linux) on every file that made up the current snapshot — the root
journal plus any files it @include@s — and asks the 'SnapshotStore' to reload
when one of them changes. While a journal directory is absent, it checks for
that directory so the ledger can recover when it is provisioned.

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
    withJournalWatcherNamed,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (race)
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
import Control.Monad (forever, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
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
    SnapshotStore,
    currentGeneration,
    readCurrentSnapshot,
    reloadSnapshot,
    snapshotJournalPath,
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
    withJournalWatcherNamed (snapshotJournalPath store) store action

withJournalWatcherNamed :: Text -> SnapshotStore -> IO a -> IO a
withJournalWatcherNamed label store action =
    withManager $ \manager -> do
        watchedDirs' <- newIORef Map.empty
        watchTargets' <- newIORef Set.empty
        let ws = WatchState watchedDirs' watchTargets'
        wakeReloader <- newTVarIO False
        let requestReload = atomically (writeTVar wakeReloader True)
        reconcileWatches manager store ws requestReload
        reloader <- forkIO (runReloader label store manager ws requestReload wakeReloader)
        action `finally` killThread reloader

{- | Runs 'reloadCycle' forever, guarding each iteration with 'guardReloaderStep'
so a synchronous exception (e.g. a watched path vanishing mid-reconcile during
an atomic-rename storm) is logged and the loop keeps going instead of the
reloader thread dying silently.
-}
runReloader :: Text -> SnapshotStore -> WatchManager -> WatchState -> IO () -> TVar Bool -> IO ()
runReloader label store manager ws requestReload wakeReloader = forever (guardReloaderStep step)
  where
    step = reloadCycle label store manager ws requestReload wakeReloader

reloadCycle :: Text -> SnapshotStore -> WatchManager -> WatchState -> IO () -> TVar Bool -> IO ()
reloadCycle label store manager ws requestReload wakeReloader = do
    waitForReload store wakeReloader
    threadDelay debounceMicroseconds
    -- Clear the flag so events coalesced during the debounce window are
    -- covered by the reload below; any that arrive afterwards set it again
    -- and drive a fresh cycle, so no change is missed.
    atomically (writeTVar wakeReloader False)
    before <- currentGeneration store
    result <- reloadSnapshot store
    after <- currentGeneration store
    when (after /= before) $
        Text.putStrLn ("Ledger " <> label <> ": " <> reloadMessage result)
    reconcileWatches manager store ws requestReload

waitForReload :: SnapshotStore -> TVar Bool -> IO ()
waitForReload store wakeReloader = do
    let parent = takeDirectory (Text.unpack (snapshotJournalPath store))
        awaitEvent = atomically (readTVar wakeReloader >>= check)
    parentExists <- Directory.doesDirectoryExist parent
    if parentExists
        then awaitEvent
        else void (race awaitEvent (waitForDirectory parent))

waitForDirectory :: FilePath -> IO ()
waitForDirectory parent = do
    exists <- Directory.doesDirectoryExist parent
    if exists
        then pure ()
        else threadDelay 1_000_000 >> waitForDirectory parent

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
    plans <- traverse watchDirectoryFor files
    let missingDirs = concatMap snd plans
        missingParents = [parent | (parent, missing) <- plans, not (null missing)]
        desiredDirs = Set.fromList (fst <$> plans)
    writeIORef ws.watchTargets (Set.fromList (files ++ missingDirs ++ missingParents))
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
    -- A journal can appear between the failed reload and watch installation.
    -- Recheck once after changing directories, so that creation cannot be lost.
    when (not (Set.null toStart)) $ do
        snapshot <- readCurrentSnapshot store
        rootExists <- Directory.doesFileExist (Text.unpack (snapshotJournalPath store))
        when (isNothing snapshot && rootExists) requestReload

-- A missing journal may sit below a directory that has not been created yet.
-- Watch the nearest existing ancestor and wake when each missing directory
-- appears, then reconcile down toward the journal.
watchDirectoryFor :: FilePath -> IO (FilePath, [FilePath])
watchDirectoryFor file = go (takeDirectory file) []
  where
    go candidate missing = do
        exists <- Directory.doesDirectoryExist candidate
        if exists
            then pure (candidate, missing)
            else go (takeDirectory candidate) (candidate : missing)

handleEvent :: WatchState -> IO () -> Event -> IO ()
handleEvent ws requestReload event = do
    watched <- readIORef ws.watchTargets
    when (eventPath event `Set.member` watched) requestReload

-- The root path is always included so a journal that currently fails to parse
-- is still watched and picked up once it parses again.
watchTargets :: SnapshotStore -> IO [FilePath]
watchTargets store = do
    snapshot <- readCurrentSnapshot store
    let included = maybe [] (fmap fst . (.jfiles) . (.journal)) snapshot
    root <- canonicalize (Text.unpack (snapshotJournalPath store))
    absoluteIncluded <- traverse canonicalize included
    pure (nub (root : absoluteIncluded))

{- | Resolve to a canonical path so event paths (which the OS reports resolved)
compare equal to our target set even when the journal lives under a symlink.
-}
canonicalize :: FilePath -> IO FilePath
canonicalize = Directory.canonicalizePath
