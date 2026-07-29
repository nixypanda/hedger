module Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, when)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (
    currentRevision,
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    reloadSnapshot,
 )
import Hedger.TestHelpers (loadedStore)
import Hledger.Data.Account qualified as HledgerAccount
import Hledger.Data.Types qualified as Hledger

snapshotStoreSpec :: TestTree
snapshotStoreSpec =
    testGroup
        "journal snapshots"
        [ testGroup
            "fixture readiness"
            [ testCase "recognizes the shared journal as usable" $ do
                assertEqual "" True =<< snapshotAvailable "test/fixtures/shared.journal"
                assertEqual "" False =<< snapshotAvailable "test/fixtures/does-not-exist.journal"
            ]
        , testCase "parses the shared journal with every transaction and posting" parsesSharedJournal
        , testCase "rejects malformed journals instead of treating readable files as ready" $ do
            result <- loadSnapshot "test/fixtures/malformed.journal"
            assertBool "malformed journal should fail" (isLeft result)
        , testCase "keeps the last good immutable snapshot when a reload fails" keepsSnapshotOnFailedReload
        , testCase
            "keeps the published revision matching the returned snapshot under concurrent reloads"
            concurrentReloadsKeepRevisionInSyncWithSnapshot
        ]

parsesSharedJournal :: IO ()
parsesSharedJournal = do
    result <- loadSnapshot "test/fixtures/shared.journal"
    assertBool "snapshot load failed" (isRight result)
    case result of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            assertEqual "" 13 (length snapshot.transactions)
            assertEqual "" 27 (sum (length . Hledger.tpostings <$> snapshot.transactions))
            assertEqual "" (Just (fromGregorian 2024 4 2)) snapshot.latestJournalDate
            assertBool "revision should start with md5:" (Text.isPrefixOf "md5:" snapshot.revision)
            assertEqual
                "account tree should contain hledger's root and posting-derived accounts"
                ["root", "assets", "assets:cash", "assets:cash:checking"]
                (take 4 (Hledger.aname <$> HledgerAccount.flattenAccounts snapshot.accountTree))

keepsSnapshotOnFailedReload :: IO ()
keepsSnapshotOnFailedReload = do
    store <- loadedStore
    snapshotBefore <- readCurrentSnapshot store
    reloadResult <- reloadSnapshot store (Text.pack "test/fixtures/does-not-exist.journal")
    assertBool "reload should fail" (isLeft reloadResult)
    snapshotAfter <- readCurrentSnapshot store
    assertEqual
        ""
        (fmap (length . (.transactions)) snapshotBefore)
        (fmap (length . (.transactions)) snapshotAfter)
    reloadError <- readLastReloadError store
    assertBool "should have a reload error" (isJust reloadError)

-- Stress-tests the invariant that the revision published via `currentRevision`
-- always agrees with the revision of the snapshot `readCurrentSnapshot` returns.
-- `reloadSnapshot` publishes the snapshot and the revision together under a
-- single lock, so concurrent reloads of journals with different revisions can
-- never leave the two disagreeing once they have all settled.
concurrentReloadsKeepRevisionInSyncWithSnapshot :: IO ()
concurrentReloadsKeepRevisionInSyncWithSnapshot =
    withRaceJournals "concurrent-reload" $ \pathA pathB -> do
        store <- newSnapshotStore
        forM_ [1 .. rounds] $ \_ -> do
            done <- newEmptyMVar
            forM_ [1 .. tasksPerRound] $ \index ->
                forkIO $ do
                    _ <- reloadSnapshot store (if even (index :: Int) then pathA else pathB)
                    putMVar done ()
            replicateM_ tasksPerRound (takeMVar done)
        snapshotAfter <- readCurrentSnapshot store
        revisionAfter <- currentRevision store
        case snapshotAfter of
            Nothing -> assertFailure "expected a snapshot after concurrent reloads"
            Just snapshot ->
                assertEqual
                    "published revision should match the returned snapshot's revision"
                    snapshot.revision
                    revisionAfter
  where
    rounds = 3 :: Int
    tasksPerRound = 20 :: Int

-- Writes two minimal, differently-valued journals to a scratch directory so
-- concurrent reloads of them are guaranteed to observe different revisions.
withRaceJournals :: String -> (Text.Text -> Text.Text -> IO a) -> IO a
withRaceJournals label action =
    bracket acquire Directory.removeDirectoryRecursive $ \dir -> do
        let pathA = dir </> "race-a.journal"
            pathB = dir </> "race-b.journal"
        writeFile pathA raceJournalA
        writeFile pathB raceJournalB
        action (Text.pack pathA) (Text.pack pathB)
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-snapshot-store-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir

raceJournalA :: String
raceJournalA =
    unlines
        [ "2024-01-01 race fixture a"
        , "    assets:cash        111 INR"
        , "    equity:opening    -111 INR"
        ]

raceJournalB :: String
raceJournalB =
    unlines
        [ "2024-01-01 race fixture b"
        , "    assets:cash        222 INR"
        , "    equity:opening    -222 INR"
        ]

snapshotAvailable :: FilePath -> IO Bool
snapshotAvailable path = do
    store <- newSnapshotStore
    result <- reloadSnapshot store (Text.pack path)
    case result of
        Left _ -> pure False
        Right _ -> pure True
