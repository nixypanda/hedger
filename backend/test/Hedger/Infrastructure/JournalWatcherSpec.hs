module Hedger.Infrastructure.JournalWatcherSpec (journalWatcherSpec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.JournalWatcher (watchTargets, withJournalWatcher)
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
        ]

watchesRootOnly :: IO ()
watchesRootOnly =
    withJournalDir "root-only" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack path)
        targets <- watchTargets store (Text.pack path)
        assertEqual "only the root journal should be watched" 1 (length targets)

watchesIncludedFiles :: IO ()
watchesIncludedFiles =
    withJournalDir "includes" $ \dir -> do
        let mainPath = dir </> "main.journal"
            includedPath = dir </> "extra.journal"
        writeFile includedPath (secondTransaction <> "\n")
        writeFile mainPath (openingBalance <> "\ninclude extra.journal\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack mainPath)
        targets <- watchTargets store (Text.pack mainPath)
        assertEqual "the root and the included file should both be watched" 2 (length targets)

reloadsOnChange :: IO ()
reloadsOnChange =
    withJournalDir "reload" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack path)
        revisionBefore <- currentRevision store
        withJournalWatcher store (Text.pack path) $ do
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
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack mainPath)
        revisionBefore <- currentRevision store
        withJournalWatcher store (Text.pack mainPath) $ do
            waitForWatchInstall
            appendFile includedPath ("\n" <> thirdTransaction <> "\n")
            changed <- waitForRevisionChange store revisionBefore
            assertBool "the watcher should reload after an included file changes" changed

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
