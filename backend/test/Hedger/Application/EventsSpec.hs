module Hedger.Application.EventsSpec (
    eventsSpec,
) where

import Data.Maybe (isJust)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hedger.Application.AppM (Env (..))
import Hedger.Application.Events (awaitNextRevision, subscribeToRevisions)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore (..), newSnapshotStore, reloadSnapshot)
import Hedger.TestHelpers (configuration)

eventsSpec :: TestTree
eventsSpec =
    testGroup
        "application events"
        [testCase "emits the next revision published after subscribing" emitsNextRevision]

emitsNextRevision :: IO ()
emitsNextRevision = do
    store <- newSnapshotStore "test/fixtures/shared.journal"
    _ <- reloadSnapshot store
    subscription <- subscribeToRevisions Env{config = configuration, snapshotStore = store}
    let altStore =
            SnapshotStore
                { storeState = store.storeState
                , journalPath = "test/fixtures/investments.journal"
                }
    reloadResult <- reloadSnapshot altStore
    expectedRevision <-
        case reloadResult of
            Left _ -> fail "replacement snapshot load failed"
            Right snapshot -> pure snapshot.revision
    revision <- timeout 1000000 (awaitNextRevision subscription)
    assertBool "subscription should not remain blocked" (isJust revision)
    assertEqual "" (Just expectedRevision) revision
