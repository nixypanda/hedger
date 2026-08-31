module Hedger.Infrastructure.DerivedStoreSpec (derivedStoreSpec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (forM_, replicateM, unless)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Infrastructure.DerivedStore (
    CacheKey (..),
    CacheMetrics (..),
    DerivedStore,
    clearStore,
    invalidateOtherRevisions,
    lookupOrCompute,
    newDerivedStore,
    readCacheMetrics,
 )

derivedStoreSpec :: TestTree
derivedStoreSpec =
    testGroup
        "derived value cache"
        [ testCase "computes on a miss and serves the second lookup from the cache" cachesAfterFirstMiss
        , testCase "runs one computation when many threads miss the same key" singleFlightsIdenticalMisses
        , testCase "does not serialize misses on different keys" concurrentMissesRunConcurrently
        , testCase "propagates a failing computation to every waiter and retries later" failedComputeRetries
        , testCase "keeps the held bytes within the budget" evictionRespectsBudget
        , testCase "evicts other revisions before the current one" evictionPrefersOtherRevisions
        , testCase "invalidation drops exactly the stale revisions" invalidationDropsStaleRevisions
        , testCase "treats an entry of another value type as a miss" mismatchedTypeCountsAsMiss
        , testCase "clearing drops every entry but keeps the counters" clearingDropsEntries
        , testCase "counts hits, misses, waiters and evictions" metricsFollowStoreActivity
        ]

newtype ComputationFailed = ComputationFailed Text
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

type Outcome = Either ComputationFailed Text

-- Long enough that a correct implementation never trips it, short enough that a
-- broken one fails the test instead of hanging the suite.
guardMicroseconds :: Int
guardMicroseconds = 10_000_000

keyAt :: Text -> Text -> CacheKey
keyAt revision dimensions = CacheKey{family = "net-worth", revision, dimensions}

fixedSize :: Int -> value -> Int
fixedSize size _ = size

-- | Records how many times the store actually ran a computation.
countedCompute :: IORef Int -> result -> IO result
countedCompute calls result = do
    atomicModifyIORef' calls (\count -> (count + 1, ()))
    pure result

withinGuard :: String -> IO a -> IO a
withinGuard label action = do
    outcome <- timeout guardMicroseconds action
    maybe (assertFailure (label <> " did not finish within the failure guard")) pure outcome

forkResult :: IO Text -> IO (MVar Outcome)
forkResult action = do
    slot <- newEmptyMVar
    _ <- forkIO (try action >>= putMVar slot)
    pure slot

collect :: String -> [MVar Outcome] -> IO [Outcome]
collect label slots = withinGuard label (traverse readMVar slots)

{- | Waits for the store to reach an observable state. A thread blocked inside
`lookupOrCompute` cannot signal from the test, so the metrics are the only way
to know the waiters have arrived; the delay bounds a broken store rather than
standing in for synchronization.
-}
awaitMetrics :: String -> DerivedStore -> (CacheMetrics -> Bool) -> IO ()
awaitMetrics label store predicate = withinGuard label poll
  where
    poll = do
        metrics <- readCacheMetrics store
        unless (predicate metrics) (threadDelay 200 >> poll)

cachesAfterFirstMiss :: IO ()
cachesAfterFirstMiss = do
    store <- newDerivedStore 1_000
    calls <- newIORef 0
    let key = keyAt "rev-1" "monthly"
        lookupKey :: IO Text
        lookupKey = lookupOrCompute store key (fixedSize 10) (countedCompute calls "computed")
    first <- lookupKey
    second <- lookupKey
    assertEqual "" "computed" first
    assertEqual "" "computed" second
    assertEqual "should compute once" 1 =<< readIORef calls

singleFlightsIdenticalMisses :: IO ()
singleFlightsIdenticalMisses = do
    store <- newDerivedStore 1_000
    calls <- newIORef 0
    gate <- newEmptyMVar
    let key = keyAt "rev-1" "monthly"
        compute = readMVar gate >> countedCompute calls "computed"
    slots <- replicateM threads (forkResult (lookupOrCompute store key (fixedSize 10) compute))
    awaitMetrics "all threads reaching the key" store $ \metrics ->
        metrics.misses == 1 && metrics.waiters == threads - 1
    putMVar gate ()
    collected <- collect "single-flight lookups" slots
    assertEqual "should compute once for all threads" 1 =<< readIORef calls
    assertEqual "" (replicate threads (Right "computed")) collected
  where
    threads = 8 :: Int

-- Each computation blocks until the other has started, so the test can only
-- finish if the store leaves no global lock held while a computation runs.
concurrentMissesRunConcurrently :: IO ()
concurrentMissesRunConcurrently = do
    store <- newDerivedStore 1_000
    startedA <- newEmptyMVar
    startedB <- newEmptyMVar
    let rendezvous mine theirs result = putMVar mine () >> readMVar theirs >> pure result
        lookupWith dimensions = lookupOrCompute store (keyAt "rev-1" dimensions) (fixedSize 10)
    slotA <- forkResult (lookupWith "a" (rendezvous startedA startedB "a"))
    slotB <- forkResult (lookupWith "b" (rendezvous startedB startedA "b"))
    collected <- collect "interleaved misses on different keys" [slotA, slotB]
    assertEqual "" [Right "a", Right "b"] collected

failedComputeRetries :: IO ()
failedComputeRetries = do
    store <- newDerivedStore 1_000
    gate <- newEmptyMVar
    let key = keyAt "rev-1" "monthly"
        failing = readMVar gate >> throwIO (ComputationFailed "boom")
        attempt = lookupOrCompute store key (fixedSize 10) failing
    claimant <- forkResult attempt
    awaitMetrics "the first thread claiming the key" store ((== 1) . (.misses))
    waiter <- forkResult attempt
    awaitMetrics "the second thread waiting on the key" store ((== 1) . (.waiters))
    putMVar gate ()
    collected <- collect "failing lookups" [claimant, waiter]
    assertBool "every caller should see the failure" (all isFailure collected)
    metrics <- readCacheMetrics store
    assertEqual "a failed computation should leave no entry" 0 metrics.entries
    assertEqual "" 0 metrics.bytes
    retried <- lookupOrCompute store key (fixedSize 10) (pure @IO @Text "recovered")
    assertEqual "" "recovered" retried

evictionRespectsBudget :: IO ()
evictionRespectsBudget = do
    store <- newDerivedStore 250
    forM_ ["a", "b", "c", "d"] $ \dimensions ->
        lookupOrCompute store (keyAt "rev-1" dimensions) (fixedSize 100) (pure @IO @Text dimensions)
    metrics <- readCacheMetrics store
    assertBool ("bytes held out of budget: " <> show metrics.bytes) (metrics.bytes <= 250)
    assertEqual "" 2 metrics.entries

evictionPrefersOtherRevisions :: IO ()
evictionPrefersOtherRevisions = do
    store <- newDerivedStore 250
    calls <- newIORef 0
    let stale = keyAt "rev-1"
        current = keyAt "rev-2"
        lookupCounted :: CacheKey -> IO Text
        lookupCounted key = lookupOrCompute store key (fixedSize 100) (countedCompute calls key.dimensions)
    forM_ [stale "a", stale "b", current "c"] lookupCounted
    assertEqual "three distinct keys should compute three times" 3 =<< readIORef calls
    -- Only the oldest other-revision entry needs to go to fit 300 bytes into 250.
    forM_ [stale "b", current "c"] lookupCounted
    assertEqual "surviving entries should hit" 3 =<< readIORef calls
    _ <- lookupCounted (stale "a")
    assertEqual "the oldest other-revision entry should have been evicted" 4 =<< readIORef calls

invalidationDropsStaleRevisions :: IO ()
invalidationDropsStaleRevisions = do
    store <- newDerivedStore 10_000
    calls <- newIORef 0
    let lookupCounted :: CacheKey -> IO Text
        lookupCounted key = lookupOrCompute store key (fixedSize 100) (countedCompute calls key.dimensions)
    forM_ [keyAt "rev-1" "a", keyAt "rev-1" "b", keyAt "rev-2" "a"] lookupCounted
    invalidateOtherRevisions store "rev-2"
    metrics <- readCacheMetrics store
    assertEqual "" 1 metrics.entries
    assertEqual "" 100 metrics.bytes
    _ <- lookupCounted (keyAt "rev-2" "a")
    assertEqual "the current revision should survive invalidation" 3 =<< readIORef calls
    forM_ [keyAt "rev-1" "a", keyAt "rev-1" "b"] lookupCounted
    assertEqual "stale entries should recompute" 5 =<< readIORef calls

mismatchedTypeCountsAsMiss :: IO ()
mismatchedTypeCountsAsMiss = do
    store <- newDerivedStore 1_000
    let key = keyAt "rev-1" "monthly"
    text <- lookupOrCompute store key (fixedSize 100) (pure @IO @Text "text")
    assertEqual "" "text" text
    number <- lookupOrCompute store key (fixedSize 10) (pure @IO @Int 7)
    assertEqual "a different value type should recompute" 7 number
    metrics <- readCacheMetrics store
    assertEqual "the replacement should not be counted as a hit" 0 metrics.hits
    assertEqual "" 2 metrics.misses
    assertEqual "" 1 metrics.entries
    assertEqual "the replaced entry's bytes should be released" 10 metrics.bytes

clearingDropsEntries :: IO ()
clearingDropsEntries = do
    store <- newDerivedStore 1_000
    calls <- newIORef 0
    let key = keyAt "rev-1" "monthly"
        lookupCounted :: IO Text
        lookupCounted = lookupOrCompute store key (fixedSize 100) (countedCompute calls "computed")
    _ <- lookupCounted
    _ <- lookupCounted
    clearStore store
    _ <- lookupCounted
    assertEqual "clearing should force a recomputation" 2 =<< readIORef calls
    metrics <- readCacheMetrics store
    assertEqual "counters should stay cumulative across a clear" 1 metrics.hits
    assertEqual "" 2 metrics.misses
    assertEqual "" 1 metrics.entries
    assertEqual "" 100 metrics.bytes

metricsFollowStoreActivity :: IO ()
metricsFollowStoreActivity = do
    store <- newDerivedStore 250
    gate <- newEmptyMVar
    let stale = keyAt "rev-1" "a"
        current = keyAt "rev-2" "a"
        gated = readMVar gate >> pure "current"
        attempt = lookupOrCompute store current (fixedSize 200) gated
    _ <- lookupOrCompute store stale (fixedSize 200) (pure @IO @Text "stale")
    _ <- lookupOrCompute store stale (fixedSize 200) (pure @IO @Text "stale")
    claimant <- forkResult attempt
    awaitMetrics "the computing thread claiming the key" store ((== 2) . (.misses))
    waiter <- forkResult attempt
    awaitMetrics "the waiting thread" store ((== 1) . (.waiters))
    putMVar gate ()
    _ <- collect "gated lookups" [claimant, waiter]
    metrics <- readCacheMetrics store
    assertEqual "" expectedMetrics metrics
  where
    expectedMetrics =
        CacheMetrics
            { hits = 1
            , misses = 2
            , evictions = 1
            , waiters = 1
            , entries = 1
            , bytes = 200
            }

isFailure :: Outcome -> Bool
isFailure (Left _) = True
isFailure (Right _) = False
