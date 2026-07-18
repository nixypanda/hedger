module Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec) where

import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    reloadSnapshot,
 )
import Hedger.TestHelpers (loadedStore)
import Hledger.Data.Account qualified as HledgerAccount
import Hledger.Data.Types qualified as Hledger
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

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

snapshotAvailable :: FilePath -> IO Bool
snapshotAvailable path = do
    store <- newSnapshotStore
    result <- reloadSnapshot store (Text.pack path)
    case result of
        Left _ -> pure False
        Right _ -> pure True
