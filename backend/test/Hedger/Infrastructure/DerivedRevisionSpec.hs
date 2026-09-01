module Hedger.Infrastructure.DerivedRevisionSpec (derivedRevisionSpec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (throwIO)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hledger.Data.Journal qualified as HledgerJournal

import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    PrefixChain (..),
    globalFingerprint,
    prefixChain,
 )
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches (..),
    DerivedNotice (..),
    DerivedState,
    DerivedView (..),
    RebuildMetrics (..),
    RebuildOutcome (..),
    awaitDerivedNotice,
    currentGeneration,
    emptyDerivedCaches,
    mergeDerivedCaches,
    newDerivedState,
    nextGeneration,
    publishDerived,
    publishDerivedWith,
    readDerivedChain,
    readDerivedNotice,
    readDerivedRevision,
    readDerivedView,
    readRebuildMetrics,
    rebuildDerived,
    truncateDerivedCaches,
 )
import Hedger.Infrastructure.JournalWatcher (DerivedTargets (..), rebuildAfterReload)
import Hedger.Infrastructure.SnapshotStore (readCurrentSnapshot)
import Hedger.TestHelpers (configuration, loadedStore, testDerivedTargets)

derivedRevisionSpec :: TestTree
derivedRevisionSpec =
    testGroup
        "derived revision"
        [ testGroup
            "publication"
            [ testCase "has no derived revision before the first build" noDerivedRevisionInitially
            , testCase "publishes a revision together with its chain" publishesRevisionWithChain
            , testCase "leaves the caches alone when publishing without a transform" publishKeepsCaches
            , testCase "applies the cache transform with the revision" publishAppliesTransform
            , testCase "never pairs a revision with another publication's caches" viewIsInternallyConsistent
            ]
        , testGroup
            "merging request checkpoints"
            [ testCase "applies a merge for the published revision" mergeAppliesOnMatchingRevision
            , testCase "discards a merge for a superseded revision" mergeDiscardsStaleRevision
            , testCase "leaves the generation counter alone" mergeDoesNotBumpGeneration
            ]
        , testGroup
            "truncation by chain diff"
            [ testCase "discards every cache when the global fingerprint changed" globalChangeEmptiesCaches
            , testCase "discards every cache when there is no previous chain" noPreviousChainEmptiesCaches
            , testCase "keeps every cache when the chain is identical" identicalPreservesCaches
            , testCase "truncates rather than empties on a divergence" divergenceTruncatesCaches
            , testCase
                "keeps nothing when the divergence is at the oldest month"
                divergenceAtTheFirstMonthKeepsNothing
            ]
        , testGroup
            "eager warm"
            [ testCase "leaves populated checkpoint counts after a rebuild" rebuildWarmsCheckpoints
            ]
        , testGroup
            "subscriber notices"
            [ testCase "advances the tick when a revision is published" publicationAdvancesTheTick
            , testCase "spends no tick republishing the same revision" republicationSpendsNoTick
            , testCase
                "announces a revision again once a failed rebuild for it succeeds"
                successAfterFailureIsAnnounced
            , testCase "says nothing for an abandoned rebuild" abandonedRebuildSaysNothing
            , testCase "returns immediately when the tick already moved" alreadyMovedDoesNotBlock
            ]
        , testGroup
            "rebuilds"
            [ testCase "completes and publishes when it is not superseded" completesRebuild
            , testCase "abandons a superseded rebuild without publishing" abandonsSupersededRebuild
            , testCase "retains the previous derived view when a rebuild fails" retainsViewOnFailure
            , testCase "counts completed, abandoned and failed rebuilds" countsRebuildOutcomes
            ]
        ]

-- * Fixtures and helpers

-- | An empty chain is enough to exercise publication; chain content is Fingerprint's concern.
chainFixture :: PrefixChain
chainFixture = prefixChain (globalFingerprint HledgerJournal.nulljournal configuration) [] []

{- | Mirrors the watcher's check: a rebuild loses when a newer one has claimed a
generation token.
-}
supersededByGeneration :: DerivedState -> Int -> IO Bool
supersededByGeneration state token = (/= token) <$> currentGeneration state

neverSuperseded :: Int -> IO Bool
neverSuperseded _ = pure False

computeChain :: IO (PrefixChain, DerivedCaches)
computeChain = pure (chainFixture, emptyDerivedCaches)

failingCompute :: IO (PrefixChain, DerivedCaches)
failingCompute = throwIO (userError "boom")

{- | Run the real reload rebuild over the shared fixture. This is the only way to
obtain genuinely populated checkpoint caches: every key carries a content hash
of the journal prefix behind it, so a hand-written cache would either be
rejected by its own validity check or amount to reimplementing the chain.
-}
warmCaches :: IO (PrefixChain, DerivedCaches)
warmCaches = do
    store <- loadedStore
    targets <- testDerivedTargets
    wake <- newTVarIO False
    snapshot <- readCurrentSnapshot store
    maybe (fail "the fixture journal did not load") (rebuildAfterReload targets wake) snapshot
    view <- readDerivedView targets.derivedState
    chain <- maybe (fail "the rebuild published no chain") pure view.chain
    pure (chain, view.caches)

{- | The retained entry counts, read through the metrics because that is the only
place they are exposed — which also means the metrics themselves are covered by
every assertion below.
-}
cacheCounts :: DerivedCaches -> IO (Int, Int, Int)
cacheCounts caches = do
    state <- newDerivedState
    publishDerivedWith state "md5:counting" chainFixture (const caches)
    metricCounts <$> readRebuildMetrics state

metricCounts :: RebuildMetrics -> (Int, Int, Int)
metricCounts metrics =
    (metrics.scopeCheckpointCount, metrics.openingMonthCount, metrics.cashflowColumnCount)

emptyCounts :: (Int, Int, Int)
emptyCounts = (0, 0, 0)

allPositive :: (Int, Int, Int) -> Bool
allPositive (scopes, openings, columns) = scopes > 0 && openings > 0 && columns > 0

{- | Fewer entries in every family and strictly fewer where the fixture retains
more than one month: the shape that distinguishes a truncation from a discard.

The investment family is exempt from the strict part, and the exemption is not
slack in the test. Its retention policy is a dense tail plus every twelfth
month, so the shared fixture's fourteen months of history retain exactly one
scope checkpoint, and one entry either survives a divergence or does not. The
strict claim for that family is pinned by
'divergenceAtTheFirstMonthKeepsNothing' instead.
-}
truncatedNotEmptied :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
truncatedNotEmptied (fullScopes, fullOpenings, fullColumns) (scopes, openings, columns) =
    allPositive (scopes, openings, columns)
        && scopes <= fullScopes
        && openings < fullOpenings
        && columns < fullColumns

-- | The month the fixture's chain diverges at when its history is edited midway.
middleMonth :: PrefixChain -> Day
middleMonth chain = fst (chain.months !! (length chain.months `div` 2))

-- | The oldest month in the chain; nothing is retained from before it.
firstMonth :: PrefixChain -> Day
firstMonth chain = minimum (fmap fst chain.months)

-- * Publication

noDerivedRevisionInitially :: IO ()
noDerivedRevisionInitially = do
    state <- newDerivedState
    assertEqual "revision" "" =<< readDerivedRevision state
    assertBool "chain" . isNothing =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 0 metrics.rebuildCount
    assertBool "duration" (isNothing metrics.lastDuration)
    assertEqual "checkpoints" emptyCounts (metricCounts metrics)

publishesRevisionWithChain :: IO ()
publishesRevisionWithChain = do
    state <- newDerivedState
    publishDerived state "md5:one" chainFixture
    assertEqual "revision" "md5:one" =<< readDerivedRevision state
    assertBool "chain" . isJust =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "metrics revision" "md5:one" metrics.revision

{- | 'publishDerived' is the pre-checkpoint entry point and must stay a pure
revision swap, or a caller that only knows about revisions would silently drop
every checkpoint the caches hold.
-}
publishKeepsCaches :: IO ()
publishKeepsCaches = do
    (_, warmed) <- warmCaches
    populated <- cacheCounts warmed
    state <- newDerivedState
    publishDerivedWith state "md5:one" chainFixture (const warmed)
    publishDerived state "md5:two" chainFixture
    view <- readDerivedView state
    assertEqual "revision" "md5:two" view.revision
    assertEqual "caches survived" populated =<< cacheCounts view.caches

publishAppliesTransform :: IO ()
publishAppliesTransform = do
    (_, warmed) <- warmCaches
    populated <- cacheCounts warmed
    state <- newDerivedState
    publishDerivedWith state "md5:one" chainFixture (const warmed)
    view <- readDerivedView state
    assertEqual "revision" "md5:one" view.revision
    assertBool "chain" (isJust view.chain)
    assertBool "populated" (allPositive populated)
    assertEqual "caches" populated =<< cacheCounts view.caches

{- | The single-'IORef' invariant, observed from outside: a view read after a
publication carries that publication's revision and that publication's caches,
never one of each. Two publications with opposite cache contents make a
half-applied swap visible.
-}
viewIsInternallyConsistent :: IO ()
viewIsInternallyConsistent = do
    (_, warmed) <- warmCaches
    populated <- cacheCounts warmed
    state <- newDerivedState
    publishDerivedWith state "md5:warm" chainFixture (const warmed)
    warmView <- readDerivedView state
    assertEqual "warm revision" "md5:warm" warmView.revision
    assertEqual "warm caches" populated =<< cacheCounts warmView.caches
    publishDerivedWith state "md5:cold" chainFixture (const emptyDerivedCaches)
    coldView <- readDerivedView state
    assertEqual "cold revision" "md5:cold" coldView.revision
    assertEqual "cold caches" emptyCounts =<< cacheCounts coldView.caches

-- * Merging

mergeAppliesOnMatchingRevision :: IO ()
mergeAppliesOnMatchingRevision = do
    (_, warmed) <- warmCaches
    populated <- cacheCounts warmed
    state <- newDerivedState
    publishDerived state "md5:one" chainFixture
    merged <- mergeDerivedCaches state "md5:one" (const warmed)
    assertBool "merge kept" merged
    view <- readDerivedView state
    assertEqual "revision unchanged" "md5:one" view.revision
    assertEqual "caches merged" populated =<< cacheCounts view.caches

{- | The guard that stops a request from resurrecting checkpoints for a chain
that has since been truncated. Nothing else in the suite catches its removal:
with the revision comparison dropped, every other test here still passes,
because they all merge against the revision that is actually published.
-}
mergeDiscardsStaleRevision :: IO ()
mergeDiscardsStaleRevision = do
    (_, warmed) <- warmCaches
    state <- newDerivedState
    publishDerived state "md5:two" chainFixture
    merged <- mergeDerivedCaches state "md5:one" (const warmed)
    assertBool "merge discarded" (not merged)
    view <- readDerivedView state
    assertEqual "revision unchanged" "md5:two" view.revision
    assertEqual "caches untouched" emptyCounts =<< cacheCounts view.caches

{- | A merge is not a new derived view. Bumping the generation would make an
in-flight rebuild abandon itself over a request that agreed with it.
-}
mergeDoesNotBumpGeneration :: IO ()
mergeDoesNotBumpGeneration = do
    (_, warmed) <- warmCaches
    state <- newDerivedState
    publishDerived state "md5:one" chainFixture
    before <- currentGeneration state
    _ <- mergeDerivedCaches state "md5:one" (const warmed)
    _ <- mergeDerivedCaches state "md5:stale" (const emptyDerivedCaches)
    assertEqual "generation" before =<< currentGeneration state

-- * Truncation

{- | All three families must discard on 'GlobalChanged', and this is the only
thing holding them to it jointly. The month hashes are seeded from a sentinel
rather than from the global fingerprint, so they are byte-identical across a
commodity-style, account-type or config change: a family that fell back to
matching hashes would keep exactly the checkpoints 'GlobalChanged' exists to
discard, and would keep them silently.

Written after a mutation survived here. An explicit @GlobalChanged@ branch in
'truncateDerivedCaches' passed every test with the branch deleted, because the
delegation underneath already handled the case — so the branch was dead code
asserting nothing, and this test is where the requirement actually lives.
-}
globalChangeEmptiesCaches :: IO ()
globalChangeEmptiesCaches = do
    (chain, warmed) <- warmCaches
    assertBool "fixture is populated" . allPositive =<< cacheCounts warmed
    let retained = truncateDerivedCaches chain (Just GlobalChanged) warmed
    assertEqual "emptied" emptyCounts =<< cacheCounts retained

-- | Boot: there is no previous chain to compare against, so nothing may be kept.
noPreviousChainEmptiesCaches :: IO ()
noPreviousChainEmptiesCaches = do
    (chain, warmed) <- warmCaches
    let retained = truncateDerivedCaches chain Nothing warmed
    assertEqual "emptied" emptyCounts =<< cacheCounts retained

identicalPreservesCaches :: IO ()
identicalPreservesCaches = do
    (chain, warmed) <- warmCaches
    populated <- cacheCounts warmed
    assertBool "fixture is populated" (allPositive populated)
    let retained = truncateDerivedCaches chain (Just Identical) warmed
    assertEqual "preserved" populated =<< cacheCounts retained

{- | The point of the whole slice: a divergence partway through history keeps the
checkpoints before it. If this collapsed to a discard, an append would still cost
a full rebuild and the checkpoint modules would buy nothing.
-}
divergenceTruncatesCaches :: IO ()
divergenceTruncatesCaches = do
    (chain, warmed) <- warmCaches
    populated <- cacheCounts warmed
    assertBool "fixture is populated" (allPositive populated)
    let retained = truncateDerivedCaches chain (Just (DivergesFrom (middleMonth chain))) warmed
    counts <- cacheCounts retained
    assertBool
        ("truncated, not emptied: " <> show counts <> " of " <> show populated)
        (truncatedNotEmptied populated counts)

{- | The other end of the divergence range, and the one that pins every family
including the investment scope cache: a divergence at the oldest month in the
chain leaves nothing behind it to retain, so a truncation that quietly kept
entries would show up here as a non-zero count.
-}
divergenceAtTheFirstMonthKeepsNothing :: IO ()
divergenceAtTheFirstMonthKeepsNothing = do
    (chain, warmed) <- warmCaches
    assertBool "fixture is populated" . allPositive =<< cacheCounts warmed
    let retained = truncateDerivedCaches chain (Just (DivergesFrom (firstMonth chain))) warmed
    assertEqual "nothing retained" emptyCounts =<< cacheCounts retained

-- * Eager warm

{- | Without the warm the caches published by a rebuild are whatever survived
truncation, which at boot is nothing — so the first request after every edit
still pays full cost. Non-zero counts straight after a rebuild are what say the
warm ran, and that it ran inside the rebuild rather than being deferred into a
reader.
-}
rebuildWarmsCheckpoints :: IO ()
rebuildWarmsCheckpoints = do
    store <- loadedStore
    targets <- testDerivedTargets
    wake <- newTVarIO False
    snapshot <- readCurrentSnapshot store
    maybe (fail "the fixture journal did not load") (rebuildAfterReload targets wake) snapshot
    metrics <- readRebuildMetrics targets.derivedState
    assertEqual "rebuilds" 1 metrics.rebuildCount
    assertBool
        ("warmed checkpoints, got " <> show (metricCounts metrics))
        (allPositive (metricCounts metrics))
    assertBool "opening entries" (metrics.openingEntryCount > 0)

-- * Subscriber notices

publicationAdvancesTheTick :: IO ()
publicationAdvancesTheTick = do
    state <- newDerivedState
    before <- readDerivedNotice state
    publishDerived state "md5:one" chainFixture
    notice <- noticeAfter state before.tick
    assertEqual "revision" (Just "md5:one") (fmap (.revision) notice)
    assertEqual "complete" (Just True) (fmap (.complete) notice)

{- | A @touch@, or a @SIGHUP@ with nothing edited, reloads and rebuilds to the
same revision. Without this deduplication every such reload would push a refetch
to every connected client, which is what the old snapshot-revision comparison
happened to avoid for free.
-}
republicationSpendsNoTick :: IO ()
republicationSpendsNoTick = do
    state <- newDerivedState
    publishDerived state "md5:one" chainFixture
    before <- readDerivedNotice state
    publishDerived state "md5:one" chainFixture
    assertEqual "no wakeup" Nothing =<< noticeAfter state before.tick

{- | The reason a notice carries 'complete' rather than a revision alone. A
rebuild for a revision can fail — which is announced, so nobody waits forever —
and then succeed on a retry. Deduplicating on the revision alone would decide the
second announcement was the same news and swallow it, leaving every client on
stale analytics with the good news never sent.
-}
successAfterFailureIsAnnounced :: IO ()
successAfterFailureIsAnnounced = do
    state <- newDerivedState
    _ <- rebuildDerived state "md5:one" neverSuperseded failingCompute
    failureNotice <- readDerivedNotice state
    assertEqual "failure announced" ("md5:one", False) (failureNotice.revision, failureNotice.complete)
    publishDerived state "md5:one" chainFixture
    recovered <- noticeAfter state failureNotice.tick
    assertEqual "recovery announced" (Just ("md5:one", True)) (pairOf <$> recovered)

{- | An abandoned rebuild has already been superseded by one that will itself
publish or fail, so announcing here would only send clients at a view about to be
replaced.
-}
abandonedRebuildSaysNothing :: IO ()
abandonedRebuildSaysNothing = do
    state <- newDerivedState
    before <- readDerivedNotice state
    let compute = nextGeneration state *> computeChain
    outcome <- rebuildDerived state "md5:one" (supersededByGeneration state) compute
    assertEqual "outcome" RebuildAbandoned outcome
    assertEqual "no wakeup" Nothing =<< noticeAfter state before.tick

{- | The no-missed-wakeup property from the subscriber's side: an advance that
lands between taking a baseline and waiting on it leaves the wait returning
without blocking, rather than blocking until the next one.
-}
alreadyMovedDoesNotBlock :: IO ()
alreadyMovedDoesNotBlock = do
    state <- newDerivedState
    before <- readDerivedNotice state
    publishDerived state "md5:one" chainFixture
    publishDerived state "md5:two" chainFixture
    notice <- noticeAfter state before.tick
    assertEqual "latest revision" (Just "md5:two") (fmap (.revision) notice)

pairOf :: DerivedNotice -> (Text, Bool)
pairOf notice = (notice.revision, notice.complete)

{- | Whether a notice past @seen@ arrives promptly. 'Nothing' is the "still
waiting" answer, so a case asserting silence and a case asserting a wakeup differ
only in what they expect.
-}
noticeAfter :: DerivedState -> Int -> IO (Maybe DerivedNotice)
noticeAfter state seen = timeout 300_000 (awaitDerivedNotice state seen)

-- * Rebuilds

completesRebuild :: IO ()
completesRebuild = do
    state <- newDerivedState
    outcome <- rebuildDerived state "md5:new" neverSuperseded computeChain
    assertEqual "outcome" RebuildCompleted outcome
    assertEqual "revision" "md5:new" =<< readDerivedRevision state
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 1 metrics.rebuildCount
    assertBool "duration" (isJust metrics.lastDuration)

abandonsSupersededRebuild :: IO ()
abandonsSupersededRebuild = do
    state <- newDerivedState
    let compute = nextGeneration state *> computeChain
    outcome <- rebuildDerived state "md5:new" (supersededByGeneration state) compute
    assertEqual "outcome" RebuildAbandoned outcome
    assertEqual "revision" "" =<< readDerivedRevision state
    assertBool "chain" . isNothing =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "abandoned" 1 metrics.abandonedCount
    assertEqual "rebuilds" 0 metrics.rebuildCount

{- | A rebuild that throws must leave the whole previous view in place, caches
included: a half-built cache is indistinguishable from a complete one to a
reader.
-}
retainsViewOnFailure :: IO ()
retainsViewOnFailure = do
    (_, warmed) <- warmCaches
    populated <- cacheCounts warmed
    state <- newDerivedState
    publishDerivedWith state "md5:old" chainFixture (const warmed)
    outcome <- rebuildDerived state "md5:new" neverSuperseded failingCompute
    assertBool "outcome" (isFailure outcome)
    view <- readDerivedView state
    assertEqual "revision" "md5:old" view.revision
    assertBool "chain" (isJust view.chain)
    assertEqual "caches" populated =<< cacheCounts view.caches
    metrics <- readRebuildMetrics state
    assertEqual "failures" 1 metrics.failureCount
    assertBool "failure message" (maybe False (Text.isInfixOf "boom") metrics.lastFailure)

countsRebuildOutcomes :: IO ()
countsRebuildOutcomes = do
    state <- newDerivedState
    _ <- rebuildDerived state "md5:one" neverSuperseded computeChain
    _ <-
        rebuildDerived
            state
            "md5:two"
            (supersededByGeneration state)
            (nextGeneration state *> computeChain)
    _ <- rebuildDerived state "md5:three" neverSuperseded failingCompute
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 1 metrics.rebuildCount
    assertEqual "abandoned" 1 metrics.abandonedCount
    assertEqual "failures" 1 metrics.failureCount
    assertEqual "revision" "md5:one" metrics.revision

isFailure :: RebuildOutcome -> Bool
isFailure outcome = case outcome of
    RebuildFailed _ -> True
    _ -> False
