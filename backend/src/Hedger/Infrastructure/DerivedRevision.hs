{- | The derived-generation state: which journal revision the derived (cached
analytics) view was built from, the prefix chain it was built from, and the
counters that describe rebuild activity.

The snapshot is published the instant it parses, so the derived view trails it.
Everything a reader needs to know about that gap lives here.
-}
module Hedger.Infrastructure.DerivedRevision (
    DerivedState,
    RebuildOutcome (..),
    RebuildMetrics (..),
    newDerivedState,
    readDerivedRevision,
    readDerivedChain,
    publishDerived,
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

import Hedger.Domain.Fingerprint (PrefixChain)

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
    }
    deriving stock (Eq, Show)

data DerivedStateValue = DerivedStateValue
    { publishedRevision :: !Text
    , publishedChain :: !(Maybe PrefixChain)
    , generationCounter :: !Int
    , completed :: !Int
    , duration :: !(Maybe NominalDiffTime)
    , abandoned :: !Int
    , failures :: !Int
    , failure :: !(Maybe Text)
    }

{- | Concurrency invariant: the revision, its chain, and the generation counter
live in a single 'IORef' and are only ever replaced together by
'atomicModifyIORef'', so no reader can observe a revision paired with the chain
of a different rebuild, and no rebuild can publish on top of a token it did not
take. This holds without a lock because 'Hedger.Infrastructure.JournalWatcher'
runs one reloader thread: rebuilds are serialized by construction, and the only
concurrent access is reading.
-}
newtype DerivedState = DerivedState (IORef DerivedStateValue)

newDerivedState :: IO DerivedState
newDerivedState =
    DerivedState
        <$> newIORef
            DerivedStateValue
                { publishedRevision = ""
                , publishedChain = Nothing
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

{- | Swap in a complete derived view. Bumping the generation here is what makes a
rebuild that was still running against the previous view detect that it lost.
-}
publishDerived :: DerivedState -> Text -> PrefixChain -> IO ()
publishDerived (DerivedState stateReference) newRevision newChain =
    atomicModifyIORef' stateReference $ \state ->
        ( state
            { publishedRevision = newRevision
            , publishedChain = Just newChain
            , generationCounter = state.generationCounter + 1
            }
        , ()
        )

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
    -- | Computes the revision and chain to publish; may invalidate caches as a side effect.
    IO (Text, PrefixChain) ->
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
        (newRevision, newChain) <- compute
        lost <- superseded token
        if lost
            then pure RebuildAbandoned
            else do
                publishDerived state newRevision newChain
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
