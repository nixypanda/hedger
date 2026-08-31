module Hedger.Infrastructure.DerivedStore (
    CacheKey (..),
    CacheMetrics (..),
    DerivedStore,
    clearStore,
    invalidateOtherRevisions,
    lookupOrCompute,
    newDerivedStore,
    readCacheMetrics,
)
where

import Control.Concurrent.STM (
    STM,
    TMVar,
    TVar,
    atomically,
    newEmptyTMVar,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVar,
    readTVarIO,
    writeTVar,
 )
import Control.Exception (SomeException, mask, throwIO, try)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

{- | Identifies one derived value: the report family that produced it, the
snapshot revision it was derived from, and a canonical rendering of the request
dimensions (query, commodity, granularity, depth, ...). Two requests share a
cache entry exactly when all three agree.
-}
data CacheKey = CacheKey
    { family :: !Text
    , revision :: !Text
    , dimensions :: !Text
    }
    deriving stock (Eq, Ord, Show)

data CacheMetrics = CacheMetrics
    { hits :: !Int
    , misses :: !Int
    , evictions :: !Int
    , waiters :: !Int
    -- | Completed entries only; a computation still in flight holds no bytes.
    , entries :: !Int
    , bytes :: !Int
    }
    deriving stock (Eq, Show)

data Counters = Counters
    { hitCount :: !Int
    , missCount :: !Int
    , evictionCount :: !Int
    , waiterCount :: !Int
    }

data ReadyEntry = ReadyEntry
    { value :: !Dynamic
    , sizeBytes :: !Int
    , insertedAt :: !Int
    }

type Barrier = TMVar (Either SomeException Dynamic)

data Entry
    = Ready !ReadyEntry
    | InFlight !Barrier

data StoreState = StoreState
    { entryMap :: !(Map CacheKey Entry)
    , heldBytes :: !Int
    , nextInsertion :: !Int
    , counters :: !Counters
    }

{- | All mutable state lives in one @TVar@, so every read-modify-write of the
entry map, the byte total and the counters happens in a single transaction: two
threads can never both decide they are the one to compute a key, and the byte
total can never disagree with the entries it sums.

That transaction never runs the caller's computation. A miss is claimed by
inserting an empty @TMVar@ barrier and committing; the winner then computes
outside any transaction while later callers block on @readTMVar@ of the barrier.
So a miss on one key never delays a miss on another.

A waiter cannot deadlock or miss a wakeup. The barrier's only writer is the
thread that claimed the key, it runs the computation under @mask@, and it fills
the barrier on both the success and the failure path — including when the
computation throws or the thread is killed — before the mask is lifted. Because
a filled @TMVar@ stays filled under @readTMVar@, every waiter observes the
result rather than one waiter consuming it, and a waiter that arrives after the
barrier is filled reads it immediately instead of blocking.

Values are held as @Dynamic@ so one store serves every report family. A value
of an unexpected type is treated as a miss, never an error, so a family that
changes its response type cannot poison a key.
-}
data DerivedStore = DerivedStore !Int !(TVar StoreState)

-- | Takes the byte budget of derived values the store may hold before evicting.
newDerivedStore :: Int -> IO DerivedStore
newDerivedStore budget = DerivedStore budget <$> newTVarIO emptyState

emptyState :: StoreState
emptyState =
    StoreState
        { entryMap = Map.empty
        , heldBytes = 0
        , nextInsertion = 0
        , counters = Counters{hitCount = 0, missCount = 0, evictionCount = 0, waiterCount = 0}
        }

data Claim value
    = Hit !value
    | Await !Barrier
    | Compute !Barrier

{- | Returns the cached value for @key@, or computes it. Concurrent callers that
miss the same key share one computation; the size hint is applied to the
computed value to charge it against the store's byte budget. An exception from
@compute@ reaches every caller waiting on that key and leaves no entry behind,
so a later call retries.
-}
lookupOrCompute
    :: forall value
     . (Typeable value)
    => DerivedStore
    -> CacheKey
    -> (value -> Int)
    -> IO value
    -> IO value
lookupOrCompute store key sizeHint compute = do
    claim <- atomically (claimKey store key (fromDynamic @value))
    case claim of
        Hit cached -> pure cached
        Compute barrier -> runClaim store key sizeHint compute barrier
        Await barrier -> do
            shared <- atomically (readTMVar barrier) >>= either throwIO pure
            case fromDynamic shared of
                Just cached -> pure cached
                -- The in-flight computation we waited on belonged to a different
                -- value type, so its result is not ours to return.
                Nothing -> do
                    stolen <- atomically (stealClaim store key)
                    runClaim store key sizeHint compute stolen

runClaim
    :: (Typeable value)
    => DerivedStore
    -> CacheKey
    -> (value -> Int)
    -> IO value
    -> Barrier
    -> IO value
runClaim store key sizeHint compute barrier = mask $ \restore -> do
    result <- try (restore compute)
    case result of
        Left failure -> do
            atomically $ do
                releaseClaim store key barrier
                putTMVar barrier (Left failure)
            throwIO (failure :: SomeException)
        Right computed -> do
            -- Forced here so a failing size hint cannot abort the publishing
            -- transaction and strand the barrier unfilled.
            let !sizeBytes = sizeHint computed
                dynamic = toDyn computed
            atomically $ do
                publishClaim store key barrier dynamic sizeBytes
                putTMVar barrier (Right dynamic)
            pure computed

claimKey :: DerivedStore -> CacheKey -> (Dynamic -> Maybe value) -> STM (Claim value)
claimKey (DerivedStore _ stateVar) key cast = do
    state <- readTVar stateVar
    case Map.lookup key state.entryMap of
        Just (Ready ready)
            | Just cached <- cast ready.value -> do
                writeTVar stateVar state{counters = bumpHit state.counters}
                pure (Hit cached)
        -- A completed entry of another value type counts as a miss and is
        -- replaced, so the two families keep sharing one store.
        Just (Ready ready) -> Compute <$> claimOver key (dropReady ready state) stateVar
        Just (InFlight barrier) -> do
            writeTVar stateVar state{counters = bumpWaiter state.counters}
            pure (Await barrier)
        Nothing -> Compute <$> claimOver key state stateVar

stealClaim :: DerivedStore -> CacheKey -> STM Barrier
stealClaim (DerivedStore _ stateVar) key = do
    state <- readTVar stateVar
    let cleared = case Map.lookup key state.entryMap of
            Just (Ready ready) -> dropReady ready state
            _ -> state
    claimOver key cleared stateVar

claimOver :: CacheKey -> StoreState -> TVar StoreState -> STM Barrier
claimOver key state stateVar = do
    barrier <- newEmptyTMVar
    writeTVar
        stateVar
        state
            { entryMap = Map.insert key (InFlight barrier) state.entryMap
            , counters = bumpMiss state.counters
            }
    pure barrier

dropReady :: ReadyEntry -> StoreState -> StoreState
dropReady ready state = state{heldBytes = state.heldBytes - ready.sizeBytes}

publishClaim :: DerivedStore -> CacheKey -> Barrier -> Dynamic -> Int -> STM ()
publishClaim (DerivedStore budget stateVar) key barrier computed sizeBytes = do
    state <- readTVar stateVar
    case Map.lookup key state.entryMap of
        -- An entry invalidated while its computation was in flight must not be
        -- resurrected, so publish only while our own claim is still registered.
        Just (InFlight owner)
            | owner == barrier ->
                writeTVar stateVar (evictToBudget budget key (insertReady key computed sizeBytes state))
        _ -> pure ()

releaseClaim :: DerivedStore -> CacheKey -> Barrier -> STM ()
releaseClaim (DerivedStore _ stateVar) key barrier = do
    state <- readTVar stateVar
    case Map.lookup key state.entryMap of
        Just (InFlight owner)
            | owner == barrier -> writeTVar stateVar state{entryMap = Map.delete key state.entryMap}
        _ -> pure ()

insertReady :: CacheKey -> Dynamic -> Int -> StoreState -> StoreState
insertReady key computed sizeBytes state =
    state
        { entryMap = Map.insert key (Ready ready) state.entryMap
        , heldBytes = state.heldBytes + sizeBytes
        , nextInsertion = state.nextInsertion + 1
        }
  where
    ready = ReadyEntry{value = computed, sizeBytes, insertedAt = state.nextInsertion}

{- | Evicts until the byte total fits the budget, taking entries of any revision
other than @current@'s before any entry of @current@'s revision, and the
oldest-inserted first within a revision. A single entry larger than the whole
budget is therefore evicted immediately after being published rather than
looping forever.
-}
evictToBudget :: Int -> CacheKey -> StoreState -> StoreState
evictToBudget budget current state = go (evictionOrder current state.entryMap) state
  where
    go victims fitting
        | fitting.heldBytes <= budget = fitting
        | otherwise = case victims of
            [] -> fitting
            victim : rest -> go rest (evictEntry victim fitting)

evictionOrder :: CacheKey -> Map CacheKey Entry -> [CacheKey]
evictionOrder current entryMap =
    fst <$> List.sortOn rank [(key, ready) | (key, Ready ready) <- Map.toList entryMap]
  where
    rank (key, ready) = (key.revision == current.revision, ready.insertedAt)

evictEntry :: CacheKey -> StoreState -> StoreState
evictEntry key state =
    case Map.lookup key state.entryMap of
        Just (Ready ready) ->
            (dropReady ready state)
                { entryMap = Map.delete key state.entryMap
                , counters = bumpEviction state.counters
                }
        _ -> state

-- | Drops every completed or in-flight entry derived from another revision.
invalidateOtherRevisions :: DerivedStore -> Text -> IO ()
invalidateOtherRevisions (DerivedStore _ stateVar) current =
    atomically $ do
        state <- readTVar stateVar
        let (kept, stale) = Map.partitionWithKey (\key _ -> key.revision == current) state.entryMap
        writeTVar
            stateVar
            state
                { entryMap = kept
                , heldBytes = state.heldBytes - sum (entryBytes <$> Map.elems stale)
                }

-- | Drops every entry, keeping the cumulative counters so metrics stay monotonic.
clearStore :: DerivedStore -> IO ()
clearStore (DerivedStore _ stateVar) =
    atomically $ do
        state <- readTVar stateVar
        writeTVar stateVar state{entryMap = Map.empty, heldBytes = 0}

entryBytes :: Entry -> Int
entryBytes (Ready ready) = ready.sizeBytes
entryBytes (InFlight _) = 0

readCacheMetrics :: DerivedStore -> IO CacheMetrics
readCacheMetrics (DerivedStore _ stateVar) = do
    state <- readTVarIO stateVar
    pure
        CacheMetrics
            { hits = state.counters.hitCount
            , misses = state.counters.missCount
            , evictions = state.counters.evictionCount
            , waiters = state.counters.waiterCount
            , entries = length [() | Ready _ <- Map.elems state.entryMap]
            , bytes = state.heldBytes
            }

bumpHit :: Counters -> Counters
bumpHit counters = counters{hitCount = counters.hitCount + 1}

bumpMiss :: Counters -> Counters
bumpMiss counters = counters{missCount = counters.missCount + 1}

bumpEviction :: Counters -> Counters
bumpEviction counters = counters{evictionCount = counters.evictionCount + 1}

bumpWaiter :: Counters -> Counters
bumpWaiter counters = counters{waiterCount = counters.waiterCount + 1}
