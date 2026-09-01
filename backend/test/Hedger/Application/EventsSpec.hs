{- | What a subscriber is and is not told.

The negative case is the one that matters. Gating the stream on the derived
revision is invisible to every other test in the suite: with the gate removed the
stream still delivers a revision after every reload, just too early, and only a
test that reloads /without/ rebuilding can tell the difference.
-}
module Hedger.Application.EventsSpec (eventsSpec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (throwIO)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Hedger.Application.AppM (Env (..))
import Hedger.Application.Events (
    RevisionSubscription,
    awaitNextRevision,
    subscribeToRevisions,
 )
import Hedger.Domain.Fingerprint (PrefixChain)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches,
    RebuildOutcome (..),
    readDerivedRevision,
    rebuildDerived,
 )
import Hedger.Infrastructure.JournalWatcher (DerivedTargets (..), rebuildAfterReload)
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, reloadSnapshot)
import Hedger.TestHelpers (loadedStore, testEnv)

eventsSpec :: TestTree
eventsSpec =
    testGroup
        "application events"
        [ testCase
            "does not wake a subscriber when only the snapshot advances"
            snapshotAloneDoesNotWake
        , testCase
            "wakes a subscriber when the derived revision advances"
            derivedRevisionWakesSubscriber
        , testCase
            "wakes a subscriber with the journal revision when a rebuild fails"
            failedRebuildWakesWithoutMovingTheDerivedView
        ]

-- * Fixtures and helpers

replacementJournal :: Text
replacementJournal = "test/fixtures/investments.journal"

{- | Long enough that a wakeup which is going to arrive has arrived. The positive
cases wait far longer, so the two are not trading off against each other.
-}
quietMicroseconds :: Int
quietMicroseconds = 300_000

wokenMicroseconds :: Int
wokenMicroseconds = 5_000_000

nextWithin :: Int -> RevisionSubscription -> IO (Maybe Text)
nextWithin microseconds subscription = timeout microseconds (awaitNextRevision subscription)

targetsFor :: Env -> DerivedTargets
targetsFor environment =
    DerivedTargets
        { derivedState = environment.derivedState
        , derivedStore = environment.derivedStore
        , config = environment.config
        }

-- | Replace the snapshot, leaving the derived view for the caller to advance or not.
publishReplacementSnapshot :: SnapshotStore -> IO Snapshot
publishReplacementSnapshot store = do
    result <- reloadSnapshot store replacementJournal
    either (const (assertFailure "replacement snapshot load failed")) pure result

failingCompute :: IO (PrefixChain, DerivedCaches)
failingCompute = throwIO (userError "boom")

neverSuperseded :: Int -> IO Bool
neverSuperseded _ = pure False

expectFailed :: RebuildOutcome -> IO ()
expectFailed outcome = case outcome of
    RebuildFailed _ -> pure ()
    other -> assertFailure ("expected a failed rebuild, got " <> show other)

-- * Cases

{- | The stampede this gate exists to prevent: the snapshot is published the
instant it parses, and a client told to refetch then would arrive at analytics
that have not been rebuilt yet.
-}
snapshotAloneDoesNotWake :: IO ()
snapshotAloneDoesNotWake = do
    store <- loadedStore
    environment <- testEnv store
    subscription <- subscribeToRevisions environment
    _ <- publishReplacementSnapshot store
    woken <- nextWithin quietMicroseconds subscription
    assertEqual "subscriber should still be waiting" Nothing woken

derivedRevisionWakesSubscriber :: IO ()
derivedRevisionWakesSubscriber = do
    store <- loadedStore
    environment <- testEnv store
    subscription <- subscribeToRevisions environment
    snapshot <- publishReplacementSnapshot store
    wake <- newTVarIO False
    rebuildAfterReload (targetsFor environment) wake snapshot
    woken <- nextWithin wokenMicroseconds subscription
    assertEqual "announced revision" (Just snapshot.revision) woken

{- | The alternative to announcing a failure is a client that waits indefinitely
while the journal — and so the transaction list, the accounts and the register —
moves on without it. So it is told, and told the /journal's/ revision, which is
what it should refetch against; the derived view has not moved, so the analytics
responses go on reporting that they trail it.
-}
failedRebuildWakesWithoutMovingTheDerivedView :: IO ()
failedRebuildWakesWithoutMovingTheDerivedView = do
    store <- loadedStore
    environment <- testEnv store
    subscription <- subscribeToRevisions environment
    snapshot <- publishReplacementSnapshot store
    outcome <-
        rebuildDerived environment.derivedState snapshot.revision neverSuperseded failingCompute
    expectFailed outcome
    woken <- nextWithin wokenMicroseconds subscription
    assertEqual "announced revision" (Just snapshot.revision) woken
    assertEqual "derived view unmoved" "" =<< readDerivedRevision environment.derivedState
