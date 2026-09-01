module Hedger.Infrastructure.JournalWatcherSpec (journalWatcherSpec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, throwIO, try)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (Day, fromGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.IO.Error (userError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Config (Config (..))
import Hedger.Domain.Fingerprint (PrefixChain)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches,
    DerivedView (..),
    publishDerivedWith,
    readDerivedView,
 )
import Hedger.Infrastructure.JournalWatcher (
    DerivedTargets (..),
    ShadowDimension (..),
    guardReloaderStep,
    rebuildAfterReload,
    shadowDivergences,
    verifyShadow,
    watchTargets,
    withJournalWatcher,
 )
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )
import Hedger.TestHelpers (testDerivedTargets)

journalWatcherSpec :: TestTree
journalWatcherSpec =
    testGroup
        "journal watcher"
        [ testCase "watches only the root journal when it includes nothing" watchesRootOnly
        , testCase "watches every file the journal includes" watchesIncludedFiles
        , testCase "reloads the snapshot when the root journal changes" reloadsOnChange
        , testCase "reloads the snapshot when an included file changes" reloadsOnIncludedChange
        , testCase
            "guardReloaderStep logs and swallows a synchronous exception"
            guardReloaderStepSwallowsSynchronousException
        , testCase
            "guardReloaderStep rethrows an asynchronous exception"
            guardReloaderStepRethrowsAsynchronousException
        , shadowVerificationTests
        ]

watchesRootOnly :: IO ()
watchesRootOnly =
    withJournalDir "root-only" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack path)
        targets <- watchTargets store (Text.pack path)
        assertEqual "only the root journal should be watched" 1 (length targets)

watchesIncludedFiles :: IO ()
watchesIncludedFiles =
    withJournalDir "includes" $ \dir -> do
        let mainPath = dir </> "main.journal"
            includedPath = dir </> "extra.journal"
        writeFile includedPath (secondTransaction <> "\n")
        writeFile mainPath (openingBalance <> "\ninclude extra.journal\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack mainPath)
        targets <- watchTargets store (Text.pack mainPath)
        assertEqual "the root and the included file should both be watched" 2 (length targets)

reloadsOnChange :: IO ()
reloadsOnChange =
    withJournalDir "reload" $ \dir -> do
        let path = dir </> "main.journal"
        writeFile path (openingBalance <> "\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack path)
        revisionBefore <- currentRevision store
        derived <- testDerivedTargets
        withJournalWatcher store derived (Text.pack path) $ do
            waitForWatchInstall
            appendFile path ("\n" <> secondTransaction <> "\n")
            changed <- waitForRevisionChange store revisionBefore
            assertBool "the watcher should reload after the journal changes" changed

reloadsOnIncludedChange :: IO ()
reloadsOnIncludedChange =
    withJournalDir "include-reload" $ \dir -> do
        let mainPath = dir </> "main.journal"
            includedPath = dir </> "extra.journal"
        writeFile includedPath (secondTransaction <> "\n")
        writeFile mainPath (openingBalance <> "\ninclude extra.journal\n")
        store <- newSnapshotStore
        _ <- reloadSnapshot store (Text.pack mainPath)
        revisionBefore <- currentRevision store
        derived <- testDerivedTargets
        withJournalWatcher store derived (Text.pack mainPath) $ do
            waitForWatchInstall
            appendFile includedPath ("\n" <> thirdTransaction <> "\n")
            changed <- waitForRevisionChange store revisionBefore
            assertBool "the watcher should reload after an included file changes" changed

{- | Regression test for the reloader dying silently on a transient error.

@runReloader@ used to run its whole body (reconcile included) inside a bare
@forever@ with no exception handler: a watched file or directory vanishing
mid-reconcile (an editor's atomic-rename) could throw, killing the reloader
thread outright and leaving the server serving a stale snapshot forever with
no further warning.

The fix wraps each iteration in 'guardReloaderStep', the exact guard
'runReloader' uses in production, so this test exercises the guard directly
rather than trying to force a genuine filesystem race (which would make the
test flaky by construction). A synchronous exception should be caught,
logged, and swallowed so the loop can continue.
-}
guardReloaderStepSwallowsSynchronousException :: IO ()
guardReloaderStepSwallowsSynchronousException = do
    outcome <- try (guardReloaderStep (throwIO (userError "boom"))) :: IO (Either SomeException ())
    assertBool "a synchronous exception should be caught and logged, not rethrown" (isRight outcome)

{- | The watcher is stopped via @killThread@, which delivers 'ThreadKilled' (an
asynchronous exception). 'guardReloaderStep' must let it propagate, or the
reloader thread becomes unkillable and 'withJournalWatcher' can no longer
tear it down.
-}
guardReloaderStepRethrowsAsynchronousException :: IO ()
guardReloaderStepRethrowsAsynchronousException = do
    outcome <- try (guardReloaderStep (throwIO ThreadKilled)) :: IO (Either AsyncException ())
    case outcome of
        Left ThreadKilled -> pure ()
        Left other -> assertFailure ("expected ThreadKilled, got: " <> show other)
        Right () -> assertFailure "an asynchronous exception should not be swallowed"

currentRevision :: SnapshotStore -> IO (Maybe Text)
currentRevision store = fmap (.revision) <$> readCurrentSnapshot store

-- Wait for the watch to register so the edit below does not race its install.
waitForWatchInstall :: IO ()
waitForWatchInstall = threadDelay 300_000

-- Bounded so a missing event fails the test rather than hanging, and generous
-- enough to absorb filesystem-event latency (notably FSEvents coalescing on macOS).
waitForRevisionChange :: SnapshotStore -> Maybe Text -> IO Bool
waitForRevisionChange store before = go (200 :: Int)
  where
    go 0 = pure False
    go remaining = do
        current <- currentRevision store
        if current /= before
            then pure True
            else threadDelay 50_000 >> go (remaining - 1)

withJournalDir :: String -> (FilePath -> IO a) -> IO a
withJournalDir label action =
    bracket acquire Directory.removeDirectoryRecursive action
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-journal-watcher-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir

openingBalance :: String
openingBalance =
    unlines
        [ "2024-01-01 opening balance"
        , "    assets:cash        100 INR"
        , "    equity:opening    -100 INR"
        ]

secondTransaction :: String
secondTransaction =
    unlines
        [ "2024-02-01 groceries"
        , "    expenses:food       20 INR"
        , "    assets:cash        -20 INR"
        ]

thirdTransaction :: String
thirdTransaction =
    unlines
        [ "2024-03-01 salary"
        , "    assets:cash        500 INR"
        , "    income:salary     -500 INR"
        ]

-- * Shadow verification

{- | Shadow verification is a detector, so it needs both a true negative and a
true positive. Without the negative it could report divergences unconditionally;
without the positive it could report none, which is what a broken comparison
looks like.

The positive is built the only way a wrongly kept checkpoint can be built without
forging a hash: hand the comparison a chain that has /not/ been recomputed for the
edited journal. Against that chain every checkpoint still hash-matches, so the
resumed path accepts all of them — precisely what would happen if truncation kept
a month it should have discarded. The edit is in the journal's first month, before
the 24-column window opens, so every later column depends on it through a
checkpoint.
-}
shadowVerificationTests :: TestTree
shadowVerificationTests =
    testGroup
        "shadow verification"
        [ testCase "finds nothing to report after a real rebuild" shadowAgreesAfterRebuild
        , testCase
            "detects a checkpoint kept for a chain the journal moved past"
            shadowDetectsAKeptCheckpoint
        , testCase "compares nothing while the flag is off" shadowSkipsEverythingWhenDisabled
        , testCase "reports the divergence when the flag is on" shadowReportsWhenEnabled
        , testCase
            "says nothing about a view from another revision"
            shadowIgnoresAnotherRevisionsView
        ]

shadowAgreesAfterRebuild :: IO ()
shadowAgreesAfterRebuild =
    withJournalDir "shadow-agrees" $ \dir -> do
        let path = dir </> "main.journal"
        TextIO.writeFile path (syntheticJournal "100.00" "1")
        (targets, snapshot) <- rebuiltFrom (Text.pack path)
        view <- readDerivedView targets.derivedState
        chain <- maybe (assertFailure "the rebuild published no chain") pure view.chain
        assertEqual
            "a freshly rebuilt view agrees with the unchecked path"
            []
            (shadowDivergences targets snapshot chain view.caches)

shadowDetectsAKeptCheckpoint :: IO ()
shadowDetectsAKeptCheckpoint = do
    scenario <- keptCheckpointScenario "shadow-detects"
    assertBool
        "a checkpoint kept across a historical edit must be reported"
        (not (null (divergencesOf scenario)))

{- | The cheapness requirement, made observable: the same state that
'shadowDetectsAKeptCheckpoint' shows to be divergent yields nothing at all with
the flag off, so the comparison provably did not run.
-}
shadowSkipsEverythingWhenDisabled :: IO ()
shadowSkipsEverythingWhenDisabled = do
    scenario <- keptCheckpointScenario "shadow-disabled"
    assertBool "the scenario must be divergent" (not (null (divergencesOf scenario)))
    publishScenario scenario
    reported <- verifyShadow scenario.targets scenario.snapshot
    assertEqual "nothing is compared with the flag off" [] reported

shadowReportsWhenEnabled :: IO ()
shadowReportsWhenEnabled = do
    scenario <- keptCheckpointScenario "shadow-enabled"
    let enabled =
            scenario
                { targets =
                    scenario.targets
                        { config = scenario.targets.config{shadowVerification = True}
                        }
                }
    publishScenario enabled
    reported <- verifyShadow enabled.targets enabled.snapshot
    assertBool ("expected a report, got " <> show reported) (not (null reported))

{- | The false-positive guard. A rebuild for a newer revision can land between
publication and this check, and comparing that view against the older snapshot
would report a divergence that is only the two describing different journals —
the loudest possible log line for no bug at all.
-}
shadowIgnoresAnotherRevisionsView :: IO ()
shadowIgnoresAnotherRevisionsView = do
    scenario <- keptCheckpointScenario "shadow-other-revision"
    let enabled =
            scenario
                { targets =
                    scenario.targets
                        { config = scenario.targets.config{shadowVerification = True}
                        }
                }
    assertBool "the scenario must be divergent" (not (null (divergencesOf enabled)))
    publishDerivedWith
        enabled.targets.derivedState
        "md5:some-other-revision"
        enabled.chain
        (const enabled.caches)
    reported <- verifyShadow enabled.targets enabled.snapshot
    assertEqual "a view from another revision is not compared" [] reported

{- | A journal rebuilt at one revision, then edited in its first month, with the
pre-edit chain and caches kept: the state a failed truncation would leave behind.
-}
data KeptCheckpoint = KeptCheckpoint
    { targets :: DerivedTargets
    , snapshot :: Snapshot
    , chain :: PrefixChain
    , caches :: DerivedCaches
    }

keptCheckpointScenario :: String -> IO KeptCheckpoint
keptCheckpointScenario label =
    withJournalDir label $ \dir -> do
        let path = dir </> "main.journal"
        TextIO.writeFile path (syntheticJournal "100.00" "1")
        (targets, before) <- rebuiltFrom (Text.pack path)
        stale <- readDerivedView targets.derivedState
        chain <- maybe (assertFailure "the rebuild published no chain") pure stale.chain
        TextIO.writeFile path (syntheticJournal "900.00" "3")
        store <- newSnapshotStore
        after <- loadSnapshotFrom store (Text.pack path)
        assertBool "the edit must change the revision" (after.revision /= before.revision)
        pure KeptCheckpoint{targets, snapshot = after, chain, caches = stale.caches}

divergencesOf :: KeptCheckpoint -> [ShadowDimension]
divergencesOf scenario =
    shadowDivergences scenario.targets scenario.snapshot scenario.chain scenario.caches

{- | Make the stale trio the published derived view under the edited journal's
revision, so 'verifyShadow' sees it rather than being turned away by its own
revision guard.
-}
publishScenario :: KeptCheckpoint -> IO ()
publishScenario scenario =
    publishDerivedWith
        scenario.targets.derivedState
        scenario.snapshot.revision
        scenario.chain
        (const scenario.caches)

rebuiltFrom :: Text -> IO (DerivedTargets, Snapshot)
rebuiltFrom path = do
    store <- newSnapshotStore
    snapshot <- loadSnapshotFrom store path
    targets <- testDerivedTargets
    wake <- newTVarIO False
    rebuildAfterReload targets wake snapshot
    pure (targets, snapshot)

loadSnapshotFrom :: SnapshotStore -> Text -> IO Snapshot
loadSnapshotFrom store path = do
    loaded <- reloadSnapshot store path
    case loaded of
        Left problem -> assertFailure ("snapshot load failed: " <> show problem)
        Right snapshot -> pure snapshot

{- | 26 consecutive months from January 2022, so the monthly window of 24 columns
opens after the journal starts and a resume has an earlier month to open from.
The first month's amounts are parameters, which is what makes an edit
/historical/ rather than an append.
-}
syntheticJournal :: Text -> Text -> Text
syntheticJournal firstDeposit firstUnits =
    Text.unlines (concatMap monthEntries [0 .. 25 :: Int])
  where
    monthEntries index =
        [ dayText (monthDay index 15) <> " deposit"
        , "    assets:bank:checking      " <> depositIn index <> " INR"
        , "    income:salary"
        , ""
        , dayText (monthDay index 15) <> " buy"
        , "    assets:investments:equity:xyz    " <> unitsIn index <> " XYZ @ 50.00 INR"
        , "    assets:bank:checking"
        , ""
        , dayText (monthDay index 31) <> " card purchase"
        , "    expenses:food             25.00 INR"
        , "    liabilities:card"
        , ""
        , "P " <> dayText (monthDay index 31) <> " XYZ 60.00 INR"
        , ""
        ]
    depositIn index = if index == 0 then firstDeposit else "100.00"
    unitsIn index = if index == 0 then firstUnits else "1"
    monthDay :: Int -> Int -> Day
    monthDay index dayOfMonth =
        fromGregorian (2022 + fromIntegral (index `div` 12)) (1 + (index `mod` 12)) dayOfMonth

dayText :: Day -> Text
dayText = Text.pack . show
