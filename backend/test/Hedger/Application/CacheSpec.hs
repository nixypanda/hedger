module Hedger.Application.CacheSpec (cacheSpec) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hedger.Application.AppM (AppM, Env (..), runAppM)
import Hedger.Application.Cache (cachedResponse, dimensionKey)
import Hedger.Application.Common (SnapshotMetadata (..))
import Hedger.Domain.Error (AppError (InvalidDate))
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (globalFingerprint, prefixChain)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (DerivedState, newDerivedState, publishDerived)
import Hedger.Infrastructure.DerivedStore (newDerivedStore)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )
import Hedger.TestHelpers (configuration)

cacheSpec :: TestTree
cacheSpec =
    testGroup
        "Application.Cache"
        [ testCase "a hit reuses the body but recomputes metadata" hitReusesBodyWithFreshMetadata
        , testCase "a failing producer is not cached and is retried" failureIsNotCached
        , testCase "differing dimensions get separate entries" dimensionsSeparateEntries
        , testCase "a revision change invalidates prior entries" revisionChangeInvalidates
        ]

data Payload = Payload
    { body :: Text
    , observedFreshness :: Text
    , observedWarnings :: [Text]
    }
    deriving stock (Eq, Show)

sharedJournal :: Text
sharedJournal = "test/fixtures/shared.journal"

otherJournal :: Text
otherJournal = "test/fixtures/investments.journal"

missingJournal :: Text
missingJournal = "test/fixtures/no-such-file.journal"

withEnvironment :: (Env -> SnapshotStore -> IO a) -> IO a
withEnvironment run = do
    snapshotStore <- newSnapshotStore
    _ <- reloadSnapshot snapshotStore sharedJournal
    derivedStore <- newDerivedStore (1024 * 1024)
    derivedState <- newDerivedState
    -- Publish a derived revision matching the snapshot, so the baseline freshness is
    -- "fresh". A never-built derived state reports "refreshing", which is correct but
    -- would mask what this spec is about: metadata being recomputed on a cache hit.
    readCurrentSnapshot snapshotStore >>= traverse_ (publishFor derivedState)
    run Env{config = configuration, snapshotStore, derivedStore, derivedState} snapshotStore

publishFor :: DerivedState -> Snapshot -> IO ()
publishFor derivedState snapshot =
    publishDerived
        derivedState
        snapshot.revision
        ( prefixChain
            (globalFingerprint snapshot.journal configuration)
            snapshot.transactions
            snapshot.journal.jpricedirectives
        )

withMetadata :: SnapshotMetadata -> Payload -> Payload
withMetadata value payload =
    payload{observedFreshness = value.freshness, observedWarnings = value.warnings}

sizeHint :: Payload -> Int
sizeHint payload = Text.length payload.body

producing :: IORef Int -> Text -> AppM Payload
producing counter value = do
    liftIO (modifyIORef' counter (+ 1))
    pure Payload{body = value, observedFreshness = "unset", observedWarnings = []}

failing :: IORef Int -> AppM Payload
failing counter = do
    liftIO (modifyIORef' counter (+ 1))
    throwError InvalidDate

cached :: Env -> Text -> AppM Payload -> IO (Either AppError Payload)
cached environment dimensions = runAppM environment . cachedResponse "test" dimensions withMetadata sizeHint

hitReusesBodyWithFreshMetadata :: IO ()
hitReusesBodyWithFreshMetadata = withEnvironment $ \environment store -> do
    counter <- newIORef 0
    first <- cached environment "only" (producing counter "computed")
    assertEqual "fresh metadata on the miss" (Right ("computed", "fresh", [])) (payloadParts <$> first)
    -- A failed reload keeps the snapshot and its revision, so the cache still hits
    -- while the freshness and reload warnings the response must report have changed.
    _ <- reloadSnapshot store missingJournal
    second <- cached environment "only" (producing counter "recomputed")
    calls <- readIORef counter
    assertEqual "producer ran once" 1 calls
    assertEqual "cached body reused" (Right "computed") (bodyOf <$> second)
    assertEqual "stale freshness served" (Right "stale") (freshnessOf <$> second)
    assertBool "reload warning served" (either (const False) (not . null . observedWarningsOf) second)

failureIsNotCached :: IO ()
failureIsNotCached = withEnvironment $ \environment _ -> do
    counter <- newIORef 0
    first <- cached environment "only" (failing counter)
    assertEqual "error propagated" (Left InvalidDate) (bodyOf <$> first)
    second <- cached environment "only" (producing counter "computed")
    assertEqual "the retry recomputes" (Right "computed") (bodyOf <$> second)
    calls <- readIORef counter
    assertEqual "both producers ran" 2 calls

dimensionsSeparateEntries :: IO ()
dimensionsSeparateEntries = withEnvironment $ \environment _ -> do
    counter <- newIORef 0
    let dimensionsFor value = dimensionKey [Aeson.toJSON (value :: Text)]
    first <- cached environment (dimensionsFor "a") (producing counter "for-a")
    second <- cached environment (dimensionsFor "b") (producing counter "for-b")
    repeated <- cached environment (dimensionsFor "a") (producing counter "unused")
    calls <- readIORef counter
    assertEqual "one entry per dimension" 2 calls
    assertEqual "first dimension body" (Right "for-a") (bodyOf <$> first)
    assertEqual "second dimension body" (Right "for-b") (bodyOf <$> second)
    assertEqual "first dimension still cached" (Right "for-a") (bodyOf <$> repeated)

revisionChangeInvalidates :: IO ()
revisionChangeInvalidates = withEnvironment $ \environment store -> do
    counter <- newIORef 0
    _ <- cached environment "only" (producing counter "first-revision")
    _ <- reloadSnapshot store otherJournal
    afterChange <- cached environment "only" (producing counter "second-revision")
    assertEqual "a new revision misses" (Right "second-revision") (bodyOf <$> afterChange)
    _ <- reloadSnapshot store sharedJournal
    afterRevert <- cached environment "only" (producing counter "third-revision")
    assertEqual "the superseded entry was dropped" (Right "third-revision") (bodyOf <$> afterRevert)
    calls <- readIORef counter
    assertEqual "every revision recomputed" 3 calls

payloadParts :: Payload -> (Text, Text, [Text])
payloadParts payload = (payload.body, payload.observedFreshness, payload.observedWarnings)

bodyOf :: Payload -> Text
bodyOf payload = payload.body

freshnessOf :: Payload -> Text
freshnessOf payload = payload.observedFreshness

observedWarningsOf :: Payload -> [Text]
observedWarningsOf payload = payload.observedWarnings
