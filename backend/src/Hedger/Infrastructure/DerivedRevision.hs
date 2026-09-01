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
    DerivedNotice (..),
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
    readDerivedNotice,
    awaitDerivedNotice,
    publishDerived,
    publishDerivedWith,
    nextGeneration,
    currentGeneration,
    rebuildDerived,
    readRebuildMetrics,
)
where

import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVar,
    readTVar,
    readTVarIO,
 )
import Control.Exception (
    SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Hedger.Domain.Cashflow (
    CashflowColumns,
    columnCount,
    emptyColumns,
    truncateColumns,
 )
import Hedger.Domain.Fingerprint (ChainDiff, PrefixChain)

import Hedger.Domain.InvestmentPerformance (
    ScopeCache (..),
    checkpointCount,
    emptyScopeCache,
    truncateScopeCache,
 )
import Hedger.Domain.NetWorth (
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

{- | What a subscriber is told when the derived view moves.

'tick' is the wakeup signal and 'revision' is what to announce. The tick is a
monotone counter rather than the revision text because the same revision can
have to be announced twice for different reasons: a rebuild for it can fail
(announcing that the /journal/ reached it while the derived view did not) and
then succeed (announcing that the derived view caught up). Comparing revision
text alone would swallow the second announcement.

'complete' is what makes those two announcements distinguishable, and so is what
the deduplication below compares: a tick is spent only when the
@(revision, complete)@ pair actually changes, which keeps a content-free reload —
a @touch@, or a @SIGHUP@ with nothing edited — from pushing an event to every
connected client.
-}
data DerivedNotice = DerivedNotice
    { tick :: !Int
    , revision :: !Text
    , complete :: !Bool
    {- ^ Whether the derived view for 'revision' is published, as opposed to a
    rebuild for it having failed.
    -}
    }
    deriving stock (Eq, Show)

{- | Concurrency invariant: the revision, its chain, its checkpoint caches, and
the generation counter live in a single 'TVar' and are only ever replaced
together, so no reader can observe a revision paired with the chain or the
checkpoints of a different rebuild, and no rebuild can publish on top of a token
it did not take.

The notice is a second 'TVar', written in the /same/ transaction as the value it
announces. That is the whole reason this is STM and not an 'Data.IORef.IORef':
a subscriber woken by the notice must be unable to read a derived view older
than the one the notice describes, and one transaction over both variables is
the cheapest way to say so. Keeping the notice separate rather than folding it
into the value means a subscriber blocks on revision changes only, and is not
woken by every request that merges checkpoints back.

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
data DerivedState = DerivedState (TVar DerivedStateValue) (TVar DerivedNotice)

newDerivedState :: IO DerivedState
newDerivedState =
    atomically $
        DerivedState
            <$> newTVar
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
            <*> newTVar DerivedNotice{tick = 0, revision = "", complete = False}

-- | The derived revision currently published, or @""@ before the first successful rebuild.
readDerivedRevision :: DerivedState -> IO Text
readDerivedRevision (DerivedState stateVar _) = (.publishedRevision) <$> readTVarIO stateVar

readDerivedChain :: DerivedState -> IO (Maybe PrefixChain)
readDerivedChain (DerivedState stateVar _) = (.publishedChain) <$> readTVarIO stateVar

-- | The revision, chain and caches as of one read of the state variable.
readDerivedView :: DerivedState -> IO DerivedView
readDerivedView (DerivedState stateVar _) = do
    state <- readTVarIO stateVar
    pure
        DerivedView
            { revision = state.publishedRevision
            , chain = state.publishedChain
            , caches = state.publishedCaches
            }

-- | The notice as it currently stands, for a subscriber establishing its baseline.
readDerivedNotice :: DerivedState -> IO DerivedNotice
readDerivedNotice (DerivedState _ noticeVar) = readTVarIO noticeVar

{- | Block until the notice carries a tick other than @seen@.

A subscriber cannot miss a wakeup: it takes its baseline tick from
'readDerivedNotice' and every subsequent advance is a commit to the same
variable, so an advance that lands between the baseline read and the
'awaitDerivedNotice' call leaves the variable already differing and the
transaction returns without blocking.
-}
awaitDerivedNotice :: DerivedState -> Int -> IO DerivedNotice
awaitDerivedNotice (DerivedState _ noticeVar) seen =
    atomically $ do
        notice <- readTVar noticeVar
        check (notice.tick /= seen)
        pure notice

{- | Swap in a complete derived view, leaving the checkpoint caches as they are.
Bumping the generation here is what makes a rebuild that was still running
against the previous view detect that it lost.
-}
publishDerived :: DerivedState -> Text -> PrefixChain -> IO ()
publishDerived state newRevision newChain = publishDerivedWith state newRevision newChain id

{- | Publish a revision, its chain and a transformation of the checkpoint caches
in one step, so a truncation can never be visible without the chain that
justifies it (or the reverse). The notice advances in the same transaction, so a
subscriber woken by it always reads at least this view.

Callers pass @const alreadyForcedCaches@ rather than the work itself: the
transformation runs inside the transaction, which is the wrong place to discover
that a cache field was a thunk — and worse under STM than under an 'Data.IORef.IORef',
because a conflicting commit would evaluate it again.
-}
publishDerivedWith :: DerivedState -> Text -> PrefixChain -> (DerivedCaches -> DerivedCaches) -> IO ()
publishDerivedWith (DerivedState stateVar noticeVar) newRevision newChain transform =
    atomically $ do
        modifyTVar' stateVar $ \state ->
            state
                { publishedRevision = newRevision
                , publishedChain = Just newChain
                , publishedCaches = transform state.publishedCaches
                , generationCounter = state.generationCounter + 1
                }
        advanceNotice noticeVar newRevision True

{- | Move the notice on, unless it already says exactly this. See 'DerivedNotice'
for why the announcement is deduplicated on the pair rather than on the revision.
-}
advanceNotice :: TVar DerivedNotice -> Text -> Bool -> STM ()
advanceNotice noticeVar newRevision nowComplete =
    modifyTVar' noticeVar $ \notice ->
        if notice.revision == newRevision && notice.complete == nowComplete
            then notice
            else
                DerivedNotice
                    { tick = notice.tick + 1
                    , revision = newRevision
                    , complete = nowComplete
                    }

{- | Merge checkpoints a request produced back into the published caches, but
only while the revision they were computed against is still the published one.
Returns whether the merge was kept; see the invariant on 'DerivedState' for why
a stale merge must be discarded rather than reconciled.

The generation counter is deliberately not bumped. A merge adds no new derived
view, and bumping would make an in-flight rebuild abandon itself over a request
that agreed with it.
-}
mergeDerivedCaches :: DerivedState -> Text -> (DerivedCaches -> DerivedCaches) -> IO Bool
mergeDerivedCaches (DerivedState stateVar _) expectedRevision transform =
    atomically $ do
        state <- readTVar stateVar
        if state.publishedRevision == expectedRevision
            then do
                modifyTVar' stateVar $ \current ->
                    current{publishedCaches = transform current.publishedCaches}
                pure True
            else pure False

-- | Claim the next generation token; the claim itself supersedes any rebuild in flight.
nextGeneration :: DerivedState -> IO Int
nextGeneration (DerivedState stateVar _) =
    atomically $ do
        state <- readTVar stateVar
        let claimed = state.generationCounter + 1
        modifyTVar' stateVar (\current -> current{generationCounter = claimed})
        pure claimed

currentGeneration :: DerivedState -> IO Int
currentGeneration (DerivedState stateVar _) = (.generationCounter) <$> readTVarIO stateVar

readRebuildMetrics :: DerivedState -> IO RebuildMetrics
readRebuildMetrics (DerivedState stateVar _) = do
    state <- readTVarIO stateVar
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

{- | Run one derived rebuild for @target@: take a generation token, compute, and
publish only if the token still describes the newest rebuild.

@superseded@ is consulted immediately before publishing rather than after
computing, because that is the last moment at which abandoning still costs
nothing. A synchronous exception from @compute@ leaves the previous derived view
in place: a partial derived view is indistinguishable from a correct one to a
reader, so it must never be published.

Each of the three outcomes has to leave subscribers in a defensible state, and
they differ:

  * Completed publishes, which advances the notice as a completion.
  * Failed advances the notice as an incompletion, naming @target@. The derived
    view has /not/ moved, so this is not an invitation to expect fresh analytics;
    it is what stops a client from waiting indefinitely while the snapshot — and
    therefore transactions, accounts and the register — moves on without it. The
    @refreshing@/@stale@ metadata on the analytics responses continues to say
    that the numbers trail the journal.
  * Abandoned deliberately says nothing. A newer rebuild has already claimed the
    state and will itself complete or fail, so the wakeup arrives from there;
    announcing here would only push a client onto a view about to be replaced.
    The chain terminates because the reloader clears its wake flag before
    reloading, so the last rebuild of a burst is never superseded.
-}
rebuildDerived ::
    DerivedState ->
    -- | The revision this rebuild is for, known before @compute@ so a failure can name it.
    Text ->
    -- | Whether the rebuild holding this generation token has been superseded.
    (Int -> IO Bool) ->
    {- | Computes the chain and checkpoint caches to publish; may invalidate the
    response store as a side effect. The caches it returns must already be
    forced — see 'publishDerivedWith'.
    -}
    IO (PrefixChain, DerivedCaches) ->
    IO RebuildOutcome
rebuildDerived state target superseded compute = do
    token <- nextGeneration state
    startedAt <- getCurrentTime
    result <- try (attempt token)
    finishedAt <- getCurrentTime
    outcome <- either rethrowAsynchronous pure result
    recordOutcome state target outcome (diffUTCTime finishedAt startedAt)
    pure outcome
  where
    attempt token = do
        (newChain, newCaches) <- compute
        lost <- superseded token
        if lost
            then pure RebuildAbandoned
            else do
                publishDerivedWith state target newChain (const newCaches)
                pure RebuildCompleted

rethrowAsynchronous :: SomeException -> IO RebuildOutcome
rethrowAsynchronous exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> throwIO exception
        Nothing -> pure (RebuildFailed (Text.pack (displayException exception)))

recordOutcome :: DerivedState -> Text -> RebuildOutcome -> NominalDiffTime -> IO ()
recordOutcome (DerivedState stateVar noticeVar) target outcome elapsed =
    atomically $ do
        modifyTVar' stateVar (\state -> counted state{duration = Just elapsed})
        case outcome of
            RebuildFailed _ -> advanceNotice noticeVar target False
            RebuildCompleted -> pure ()
            RebuildAbandoned -> pure ()
  where
    counted state = case outcome of
        RebuildCompleted -> state{completed = state.completed + 1}
        RebuildAbandoned -> state{abandoned = state.abandoned + 1}
        RebuildFailed problem ->
            state{failures = state.failures + 1, failure = Just problem}
