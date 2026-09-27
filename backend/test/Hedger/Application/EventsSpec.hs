module Hedger.Application.EventsSpec (
    eventsSpec,
) where

import Data.ByteString qualified as ByteString
import Data.Maybe (isJust)
import Data.Text qualified as Text
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hedger.Application.AppM (Env (..))
import Hedger.Application.Events (awaitNextRevision, subscribeToRevisions)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (newSnapshotStore, reloadSnapshot)
import Hedger.TestHelpers (configuration, withTemporaryJournal)

eventsSpec :: TestTree
eventsSpec =
    testGroup
        "application events"
        [testCase "emits the next revision published after subscribing" emitsNextRevision]

emitsNextRevision :: IO ()
emitsNextRevision = withTemporaryJournal $ \path -> do
    store <- newSnapshotStore path
    _ <- reloadSnapshot store
    subscription <- subscribeToRevisions Env{config = configuration, snapshotStore = store}
    replacement <- ByteString.readFile "test/fixtures/investments.journal"
    ByteString.writeFile (Text.unpack path) replacement
    reloadResult <- reloadSnapshot store
    expectedRevision <-
        case reloadResult of
            Left _ -> fail "replacement snapshot load failed"
            Right snapshot -> pure snapshot.revision
    revision <- timeout 1000000 (awaitNextRevision subscription)
    assertBool "subscription should not remain blocked" (isJust revision)
    assertEqual "" (Just expectedRevision) revision
