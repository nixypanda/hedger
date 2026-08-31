module Hedger.Infrastructure.DerivedRevisionSpec (derivedRevisionSpec) where

import Control.Exception (throwIO)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hledger.Data.Journal qualified as HledgerJournal

import Hedger.Domain.Fingerprint (PrefixChain, globalFingerprint, prefixChain)
import Hedger.Infrastructure.DerivedRevision (
    DerivedState,
    RebuildMetrics (..),
    RebuildOutcome (..),
    currentGeneration,
    newDerivedState,
    nextGeneration,
    publishDerived,
    readDerivedChain,
    readDerivedRevision,
    readRebuildMetrics,
    rebuildDerived,
 )
import Hedger.TestHelpers (configuration)

derivedRevisionSpec :: TestTree
derivedRevisionSpec =
    testGroup
        "derived revision"
        [ testGroup
            "publication"
            [ testCase "has no derived revision before the first build" noDerivedRevisionInitially
            , testCase "publishes a revision together with its chain" publishesRevisionWithChain
            ]
        , testGroup
            "rebuilds"
            [ testCase "completes and publishes when it is not superseded" completesRebuild
            , testCase "abandons a superseded rebuild without publishing" abandonsSupersededRebuild
            , testCase "retains the previous derived view when a rebuild fails" retainsViewOnFailure
            , testCase "counts completed, abandoned and failed rebuilds" countsRebuildOutcomes
            ]
        ]

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

computeChain :: Text -> IO (Text, PrefixChain)
computeChain revision = pure (revision, chainFixture)

failingCompute :: IO (Text, PrefixChain)
failingCompute = throwIO (userError "boom")

noDerivedRevisionInitially :: IO ()
noDerivedRevisionInitially = do
    state <- newDerivedState
    assertEqual "revision" "" =<< readDerivedRevision state
    assertBool "chain" . isNothing =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 0 metrics.rebuildCount
    assertBool "duration" (isNothing metrics.lastDuration)

publishesRevisionWithChain :: IO ()
publishesRevisionWithChain = do
    state <- newDerivedState
    publishDerived state "md5:one" chainFixture
    assertEqual "revision" "md5:one" =<< readDerivedRevision state
    assertBool "chain" . isJust =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "metrics revision" "md5:one" metrics.revision

completesRebuild :: IO ()
completesRebuild = do
    state <- newDerivedState
    outcome <- rebuildDerived state neverSuperseded (computeChain "md5:new")
    assertEqual "outcome" RebuildCompleted outcome
    assertEqual "revision" "md5:new" =<< readDerivedRevision state
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 1 metrics.rebuildCount
    assertBool "duration" (isJust metrics.lastDuration)

abandonsSupersededRebuild :: IO ()
abandonsSupersededRebuild = do
    state <- newDerivedState
    let compute = nextGeneration state *> computeChain "md5:new"
    outcome <- rebuildDerived state (supersededByGeneration state) compute
    assertEqual "outcome" RebuildAbandoned outcome
    assertEqual "revision" "" =<< readDerivedRevision state
    assertBool "chain" . isNothing =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "abandoned" 1 metrics.abandonedCount
    assertEqual "rebuilds" 0 metrics.rebuildCount

retainsViewOnFailure :: IO ()
retainsViewOnFailure = do
    state <- newDerivedState
    publishDerived state "md5:old" chainFixture
    outcome <- rebuildDerived state neverSuperseded failingCompute
    assertBool "outcome" (isFailure outcome)
    assertEqual "revision" "md5:old" =<< readDerivedRevision state
    assertBool "chain" . isJust =<< readDerivedChain state
    metrics <- readRebuildMetrics state
    assertEqual "failures" 1 metrics.failureCount
    assertBool "failure message" (maybe False (Text.isInfixOf "boom") metrics.lastFailure)

countsRebuildOutcomes :: IO ()
countsRebuildOutcomes = do
    state <- newDerivedState
    _ <- rebuildDerived state neverSuperseded (computeChain "md5:one")
    _ <- rebuildDerived state (supersededByGeneration state) (nextGeneration state *> computeChain "md5:two")
    _ <- rebuildDerived state neverSuperseded failingCompute
    metrics <- readRebuildMetrics state
    assertEqual "rebuilds" 1 metrics.rebuildCount
    assertEqual "abandoned" 1 metrics.abandonedCount
    assertEqual "failures" 1 metrics.failureCount
    assertEqual "revision" "md5:one" metrics.revision

isFailure :: RebuildOutcome -> Bool
isFailure outcome = case outcome of
    RebuildFailed _ -> True
    _ -> False
