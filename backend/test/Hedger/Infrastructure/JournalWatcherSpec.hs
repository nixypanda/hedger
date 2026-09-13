module Hedger.Infrastructure.JournalWatcherSpec (journalWatcherSpec) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, throwIO, try)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.IO.Error (userError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.JournalWatcher (guardReloaderStep, watchTargets, withJournalWatcher)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )

journalWatcherSpec :: TestTree
journalWatcherSpec =
    testGroup
        "journal watcher"
        [ testCase "watches only the root journal when it includes nothing" watchesRootOnly
        , testCase "watches every file the journal includes" watchesIncludedFiles
        , testCase "reloads the snapshot when the root journal changes" reloadsOnChange
        , testCase "reloads the snapshot when an included file changes" reloadsOnIncludedChange
        , testCase
            "guardReloaderStep logs and swallows a synchronous exception"
            guardReloaderStepSwallowsSynchronousException
        , testCase
            "guardReloaderStep rethrows an asynchronous exception"
            guardReloaderStepRethrowsAsynchronousException
        ]

watchesRootOnly :: IO ()
watchesRootOnly =
    withJournalDir "root-only" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore (Text.pack path)
        _ <- reloadSnapshot store
        targets <- watchTargets store
        assertEqual "only the root journal should be watched" 1 (length targets)

watchesIncludedFiles :: IO ()
watchesIncludedFiles =
    withJournalDir "includes" $ \dir -> do
        let mainPath = dir </> "main.journal"
            includedPath = dir </> "extra.journal"
        writeFile includedPath (secondTransaction <> "\n")
        writeFile mainPath (openingBalance <> "\ninclude extra.journal\n")
        store <- newSnapshotStore (Text.pack mainPath)
        _ <- reloadSnapshot store
        targets <- watchTargets store
        assertEqual "the root and the included file should both be watched" 2 (length targets)

reloadsOnChange :: IO ()
reloadsOnChange =
    withJournalDir "reload" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore (Text.pack path)
        _ <- reloadSnapshot store
        revisionBefore <- currentRevision store
        withJournalWatcher store $ do
            waitForWatchInstall
            appendFile path ("\n" <> secondTransaction <> "\n")
            changed <- waitForRevisionChange store revisionBefore
            assertBool "the watcher should reload after the journal changes" changed

reloadsOnIncludedChange :: IO ()
reloadsOnIncludedChange =
    withJournalDir "include-reload" $ \dir -> do
        let mainPath = dir </> "main.journal"
            includedPath = dir </> "extra.journal"
        writeFile includedPath (secondTransaction <> "\n")
        writeFile mainPath (openingBalance <> "\ninclude extra.journal\n")
        store <- newSnapshotStore (Text.pack mainPath)
        _ <- reloadSnapshot store
        revisionBefore <- currentRevision store
        withJournalWatcher store $ do
            waitForWatchInstall
            appendFile includedPath ("\n" <> thirdTransaction <> "\n")
            changed <- waitForRevisionChange store revisionBefore
            assertBool "the watcher should reload after an included file changes" changed

{- | Regression test for the reloader dying silently on a transient error.

@runReloader@ used to run its whole body (reconcile included) inside a bare
@forever@ with no exception handler: a watched file or directory vanishing
mid-reconcile (an editor's atomic-rename) could throw, killing the reloader
thread outright and leaving the server serving a stale snapshot forever with
no further warning.

The fix wraps each iteration in 'guardReloaderStep', the exact guard
'runReloader' uses in production, so this test exercises the guard directly
rather than trying to force a genuine filesystem race (which would make the
test flaky by construction). A synchronous exception should be caught,
logged, and swallowed so the loop can continue.
-}
guardReloaderStepSwallowsSynchronousException :: IO ()
guardReloaderStepSwallowsSynchronousException = do
    outcome <- try (guardReloaderStep (throwIO (userError "boom"))) :: IO (Either SomeException ())
    assertBool "a synchronous exception should be caught and logged, not rethrown" (isRight outcome)

{- | The watcher is stopped via @killThread@, which delivers 'ThreadKilled' (an
asynchronous exception). 'guardReloaderStep' must let it propagate, or the
reloader thread becomes unkillable and 'withJournalWatcher' can no longer
tear it down.
-}
guardReloaderStepRethrowsAsynchronousException :: IO ()
guardReloaderStepRethrowsAsynchronousException = do
    outcome <- try (guardReloaderStep (throwIO ThreadKilled)) :: IO (Either AsyncException ())
    case outcome of
        Left ThreadKilled -> pure ()
        Left other -> assertFailure ("expected ThreadKilled, got: " <> show other)
        Right () -> assertFailure "an asynchronous exception should not be swallowed"

currentRevision :: SnapshotStore -> IO (Maybe Text)
currentRevision store = fmap (.revision) <$> readCurrentSnapshot store

-- Wait for the watch to register so the edit below does not race its install.
waitForWatchInstall :: IO ()
waitForWatchInstall = threadDelay 300_000

-- Bounded so a missing event fails the test rather than hanging, and generous
-- enough to absorb filesystem-event latency (notably FSEvents coalescing on macOS).
waitForRevisionChange :: SnapshotStore -> Maybe Text -> IO Bool
waitForRevisionChange store before = go (200 :: Int)
  where
    go 0 = pure False
    go remaining = do
        current <- currentRevision store
        if current /= before
            then pure True
            else threadDelay 50_000 >> go (remaining - 1)

withJournalDir :: String -> (FilePath -> IO a) -> IO a
withJournalDir label action =
    bracket acquire Directory.removeDirectoryRecursive action
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-journal-watcher-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir

openingBalance :: String
openingBalance =
    unlines
        [ "2024-01-01 opening balance"
        , "    assets:cash        100 INR"
        , "    equity:opening    -100 INR"
        ]

secondTransaction :: String
secondTransaction =
    unlines
        [ "2024-02-01 groceries"
        , "    expenses:food       20 INR"
        , "    assets:cash        -20 INR"
        ]

thirdTransaction :: String
thirdTransaction =
    unlines
        [ "2024-03-01 salary"
        , "    assets:cash        500 INR"
        , "    income:salary     -500 INR"
        ]
