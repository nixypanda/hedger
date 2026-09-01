{- | The derived-generation state: which journal revision the derived (cached
analytics) view was built from, the prefix chain it was built from, the
checkpoint caches keyed by that chain, and the counters that describe rebuild
activity.

The snapshot is published the instant it parses, so the derived view trails it.
Everything a reader needs to know about that gap lives here.

The checkpoint caches live here rather than in
'Hedger.Infrastructure.DerivedStore' because 'DerivedStore' is a memoizing API
with no put operation: a request that extends a checkpoint map has nowhere to
write it back. Whole responses still belong in the byte-budgeted store; the
checkpoint maps that feed them belong in the value the reload cycle publishes,
so a truncation and the revision it follows from can never be observed apart.
-}
module Hedger.Infrastructure.DerivedRevision (
    DerivedState,
    DerivedCaches (..),
    DerivedView (..),
    RebuildOutcome (..),
    RebuildMetrics (..),
    newDerivedState,
    emptyDerivedCaches,
    truncateDerivedCaches,
    mergeDerivedCaches,
    readDerivedRevision,
    readDerivedChain,
    readDerivedView,
    publishDerived,
    publishDerivedWith,
    nextGeneration,
    currentGeneration,
    rebuildDerived,
    readRebuildMetrics,
)
where

import Control.Exception (
    SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Hedger.Domain.Fingerprint (ChainDiff, PrefixChain)
import Hedger.Domain.Internal.CashflowCheckpoint (
    CashflowColumns,
    columnCount,
    emptyColumns,
    truncateColumns,
 )
import Hedger.Domain.Internal.InvestmentCheckpoint (
    ScopeCache (..),
    checkpointCount,
    emptyScopeCache,
    truncateScopeCache,
 )
import Hedger.Domain.Internal.NetWorthCheckpoint (
    OpeningBalances,
    emptyOpeningBalances,
    openingBalanceCount,
    openingBalanceEntries,
    truncateOpeningBalances,
 )

data RebuildOutcome
    = RebuildCompleted
    | RebuildAbandoned
    | RebuildFailed Text
    deriving stock (Eq, Show)

data RebuildMetrics = RebuildMetrics
    { revision :: !Text
    , generation :: !Int
    , rebuildCount :: !Int
    , lastDuration :: !(Maybe NominalDiffTime)
    , abandonedCount :: !Int
    , failureCount :: !Int
    , lastFailure :: !(Maybe Text)
    , scopeCheckpointCount :: !Int
    , openingMonthCount :: !Int
    , openingEntryCount :: !Int
    , cashflowColumnCount :: !Int
    }
    deriving stock (Eq, Show)

{- | The checkpoint maps the analytics families resume from, held together
because they are all keyed by the same prefix chain and are all truncated by the
same 'Hedger.Domain.Fingerprint.ChainDiff'.
-}
data DerivedCaches = DerivedCaches
    { scopes :: !ScopeCache
    , netWorth :: !OpeningBalances
    , cashflow :: !CashflowColumns
    }

emptyDerivedCaches :: DerivedCaches
emptyDerivedCaches =
    DerivedCaches
        { scopes = emptyScopeCache
        , netWorth = emptyOpeningBalances
        , cashflow = emptyColumns
        }

{- | Drop from every cache what a chain diff condemns.

A missing diff is the boot case: there is no previous chain to compare against,
so nothing may be kept.

Everything else is delegated, and each family is handed the 'ChainDiff' itself
rather than a @Maybe Day@ distilled from it. That matters more than it looks.
The natural distillation sends @GlobalChanged@ to @Nothing@, which reads
downstream as "no divergence" and keeps everything — and the chain's month
hashes cannot catch the mistake, because they are seeded from a sentinel rather
than from the global fingerprint and so stay byte-identical across a
commodity-style, an account-type or a config change. Passing the diff through
makes discarding on @GlobalChanged@ each family's own decision, which all three
take; @globalChangeEmptiesCaches@ is what holds them to it.
-}
truncateDerivedCaches :: PrefixChain -> Maybe ChainDiff -> DerivedCaches -> DerivedCaches
truncateDerivedCaches chain difference caches = case difference of
    Nothing -> emptyDerivedCaches
    Just diff ->
        DerivedCaches
            { scopes = truncateScopeCache chain diff caches.scopes
            , netWorth = truncateOpeningBalances chain diff caches.netWorth
            , cashflow = truncateColumns chain diff caches.cashflow
            }

{- | Everything a request needs from the derived state, read as one consistent
value so a handler cannot pair one rebuild's chain with another's checkpoints.
-}
data DerivedView = DerivedView
    { revision :: !Text
    , chain :: !(Maybe PrefixChain)
    , caches :: !DerivedCaches
    }

data DerivedStateValue = DerivedStateValue
    { publishedRevision :: !Text
    , publishedChain :: !(Maybe PrefixChain)
    , publishedCaches :: !DerivedCaches
    , generationCounter :: !Int
    , completed :: !Int
    , duration :: !(Maybe NominalDiffTime)
    , abandoned :: !Int
    , failures :: !Int
    , failure :: !(Maybe Text)
    }

{- | Concurrency invariant: the revision, its chain, its checkpoint caches, and
the generation counter live in a single 'IORef' and are only ever replaced
together by 'atomicModifyIORef'', so no reader can observe a revision paired
with the chain or the checkpoints of a different rebuild, and no rebuild can
publish on top of a token it did not take. Rebuilds need no lock because
'Hedger.Infrastructure.JournalWatcher' runs one reloader thread, so they are
serialized by construction.

Requests, unlike rebuilds, are concurrent, and they write: a resumed
calculation extends a checkpoint map and hands it back through
'mergeDerivedCaches'. That is safe without a lock for two reasons, and both are
needed.

  * Checkpoint maps are /monotone under a fixed chain/. A checkpoint is keyed by
    a content hash of the journal prefix behind it, so two requests that compute
    the same key necessarily compute the same value. Merging is therefore
    idempotent and order-independent, and a merge that loses a race merely
    discards work that the winner already did or that a later request will redo.
  * The revision guard is what keeps that argument inside its premise. Once the
    chain moves, checkpoints computed against the old one describe a prefix that
    may have been truncated away, and re-inserting them would resurrect
    silently stale history — the worst failure mode available to this design. A
    merge whose revision no longer matches the published one is dropped rather
    than reconciled.
-}
newtype DerivedState = DerivedState (IORef DerivedStateValue)

newDerivedState :: IO DerivedState
newDerivedState =
    DerivedState
        <$> newIORef
            DerivedStateValue
                { publishedRevision = ""
                , publishedChain = Nothing
                , publishedCaches = emptyDerivedCaches
                , generationCounter = 0
                , completed = 0
                , duration = Nothing
                , abandoned = 0
                , failures = 0
                , failure = Nothing
                }

-- | The derived revision currently published, or @""@ before the first successful rebuild.
readDerivedRevision :: DerivedState -> IO Text
readDerivedRevision (DerivedState stateReference) = (.publishedRevision) <$> readIORef stateReference

readDerivedChain :: DerivedState -> IO (Maybe PrefixChain)
readDerivedChain (DerivedState stateReference) = (.publishedChain) <$> readIORef stateReference

-- | The revision, chain and caches as of one read of the 'IORef'.
readDerivedView :: DerivedState -> IO DerivedView
readDerivedView (DerivedState stateReference) = do
    state <- readIORef stateReference
    pure
        DerivedView
            { revision = state.publishedRevision
            , chain = state.publishedChain
            , caches = state.publishedCaches
            }

{- | Swap in a complete derived view, leaving the checkpoint caches as they are.
Bumping the generation here is what makes a rebuild that was still running
against the previous view detect that it lost.
-}
publishDerived :: DerivedState -> Text -> PrefixChain -> IO ()
publishDerived state newRevision newChain = publishDerivedWith state newRevision newChain id

{- | Publish a revision, its chain and a transformation of the checkpoint caches
in one step, so a truncation can never be visible without the chain that
justifies it (or the reverse).

Callers pass @const alreadyForcedCaches@ rather than the work itself: the
transformation runs inside 'atomicModifyIORef'', which is the wrong place to
discover that a cache field was a thunk.
-}
publishDerivedWith :: DerivedState -> Text -> PrefixChain -> (DerivedCaches -> DerivedCaches) -> IO ()
publishDerivedWith (DerivedState stateReference) newRevision newChain transform =
    atomicModifyIORef' stateReference $ \state ->
        ( state
            { publishedRevision = newRevision
            , publishedChain = Just newChain
            , publishedCaches = transform state.publishedCaches
            , generationCounter = state.generationCounter + 1
            }
        , ()
        )

{- | Merge checkpoints a request produced back into the published caches, but
only while the revision they were computed against is still the published one.
Returns whether the merge was kept; see the invariant on 'DerivedState' for why
a stale merge must be discarded rather than reconciled.

The generation counter is deliberately not bumped. A merge adds no new derived
view, and bumping would make an in-flight rebuild abandon itself over a request
that agreed with it.
-}
mergeDerivedCaches :: DerivedState -> Text -> (DerivedCaches -> DerivedCaches) -> IO Bool
mergeDerivedCaches (DerivedState stateReference) expectedRevision transform =
    atomicModifyIORef' stateReference $ \state ->
        if state.publishedRevision == expectedRevision
            then (state{publishedCaches = transform state.publishedCaches}, True)
            else (state, False)

-- | Claim the next generation token; the claim itself supersedes any rebuild in flight.
nextGeneration :: DerivedState -> IO Int
nextGeneration (DerivedState stateReference) =
    atomicModifyIORef' stateReference $ \state ->
        let claimed = state.generationCounter + 1
         in (state{generationCounter = claimed}, claimed)

currentGeneration :: DerivedState -> IO Int
currentGeneration (DerivedState stateReference) = (.generationCounter) <$> readIORef stateReference

readRebuildMetrics :: DerivedState -> IO RebuildMetrics
readRebuildMetrics (DerivedState stateReference) = do
    state <- readIORef stateReference
    pure
        RebuildMetrics
            { revision = state.publishedRevision
            , generation = state.generationCounter
            , rebuildCount = state.completed
            , lastDuration = state.duration
            , abandonedCount = state.abandoned
            , failureCount = state.failures
            , lastFailure = state.failure
            , scopeCheckpointCount = checkpointCount state.publishedCaches.scopes.checkpoints
            , openingMonthCount = openingBalanceCount state.publishedCaches.netWorth
            , openingEntryCount = openingBalanceEntries state.publishedCaches.netWorth
            , cashflowColumnCount = columnCount state.publishedCaches.cashflow
            }

{- | Run one derived rebuild: take a generation token, compute, and publish only
if the token still describes the newest rebuild.

@superseded@ is consulted immediately before publishing rather than after
computing, because that is the last moment at which abandoning still costs
nothing. A synchronous exception from @compute@ leaves the previous derived view
in place: a partial derived view is indistinguishable from a correct one to a
reader, so it must never be published.
-}
rebuildDerived ::
    DerivedState ->
    -- | Whether the rebuild holding this generation token has been superseded.
    (Int -> IO Bool) ->
    {- | Computes the revision, chain and checkpoint caches to publish; may
    invalidate the response store as a side effect. The caches it returns must
    already be forced — see 'publishDerivedWith'.
    -}
    IO (Text, PrefixChain, DerivedCaches) ->
    IO RebuildOutcome
rebuildDerived state superseded compute = do
    token <- nextGeneration state
    startedAt <- getCurrentTime
    result <- try (attempt token)
    finishedAt <- getCurrentTime
    outcome <- either rethrowAsynchronous pure result
    recordOutcome state outcome (diffUTCTime finishedAt startedAt)
    pure outcome
  where
    attempt token = do
        (newRevision, newChain, newCaches) <- compute
        lost <- superseded token
        if lost
            then pure RebuildAbandoned
            else do
                publishDerivedWith state newRevision newChain (const newCaches)
                pure RebuildCompleted

rethrowAsynchronous :: SomeException -> IO RebuildOutcome
rethrowAsynchronous exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> throwIO exception
        Nothing -> pure (RebuildFailed (Text.pack (displayException exception)))

recordOutcome :: DerivedState -> RebuildOutcome -> NominalDiffTime -> IO ()
recordOutcome (DerivedState stateReference) outcome elapsed =
    atomicModifyIORef' stateReference $ \state ->
        (counted state{duration = Just elapsed}, ())
  where
    counted state = case outcome of
        RebuildCompleted -> state{completed = state.completed + 1}
        RebuildAbandoned -> state{abandoned = state.abandoned + 1}
        RebuildFailed problem ->
            state{failures = state.failures + 1, failure = Just problem}
