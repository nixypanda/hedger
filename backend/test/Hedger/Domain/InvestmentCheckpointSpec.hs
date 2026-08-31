{- | Checkpointed investment scope scans measured against the unchecked path.

The decisive test is 'resumedScopeScanTests': for every investment fixture, at
the root scope and at least one nested scope, a 'scopeScansResumed' seeded from a
populated checkpoint map must equal 'scopeScansAt' computed from scratch, field
by field. The remaining cases pin the properties that equality depends on — that
a checkpoint whose chain hash has moved is ignored, that the month-end boundary
neither double-counts nor skips, and that truncation and retention keep exactly
what they claim.
-}
module Hedger.Domain.InvestmentCheckpointSpec (investmentCheckpointSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (Day, fromGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    PrefixChain (..),
    diffChains,
    globalFingerprint,
    prefixChain,
 )
import Hedger.Domain.Internal.InvestmentCheckpoint (
    ScopeCache (..),
    checkpointCount,
    checkpointMonths,
    denseTailMonths,
    emptyCheckpoints,
    emptyScopeCache,
    retainedMonths,
    scopeScansResumed,
    solvedRateCount,
    sparseCheckpointInterval,
    summariesAtResumed,
    summariesAtScopesResumed,
    truncateCheckpoints,
 )
import Hedger.Domain.Internal.InvestmentPerformance (
    InvestmentPerformanceContext (..),
    ScopeScan (..),
    prepareInvestmentPerformance,
    scopeScansAt,
 )
import Hedger.Domain.Investment (
    AssetClass (Cash, Equity),
    AssetClassMapping (..),
    AssetClassMappings,
    InvestmentMapping (..),
    InvestmentMappings,
    PerformanceAccount (..),
    PerformanceRole (Fee, Interest, RealisedGain),
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetClassMappings,
    mkInvestmentMappings,
 )
import Hedger.Domain.InvestmentPerformance (
    InvestmentSummary (..),
    SummaryResult (..),
    investmentDataTransactions,
    reportPeriods,
    summariesAtPrepared,
    summariesAtScopesPrepared,
 )
import Hedger.Domain.Period (Granularity (Monthly, Yearly))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Xirr (CashFlow, cashFlowAmount, cashFlowDate)
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.TestHelpers (configuration)

investmentCheckpointSpec :: TestTree
investmentCheckpointSpec =
    testGroup
        "investment checkpoints"
        [ resumedScopeScanTests
        , resumedSummaryTests
        , resumedScopeSummaryTests
        , testCase
            "a cached solved rate equals an uncached solve"
            cachedRateMatchesUncachedSolve
        , testCase
            "a transaction dated on a month end is counted once, by its own month"
            monthEndBoundaryCountedOnce
        , testCase
            "a checkpoint whose chain hash has moved is ignored rather than trusted"
            staleChainHashIsIgnored
        , testCase
            "a global fingerprint change invalidates every checkpoint"
            globalChangeInvalidatesCheckpoints
        , testCase
            "truncation drops exactly the checkpoints at or after the divergence month"
            truncationDropsFromDivergence
        , testCase
            "retention keeps every twelfth month plus the dense tail"
            retentionKeepsClaimedMonths
        , testCase
            "retention keeps every month of a history shorter than the dense tail"
            retentionKeepsShortHistory
        ]

-- * The decisive equivalence: resumed sweeps against sweeps from scratch

{- | For every fixture, at the root scope and at least one nested scope, a
resumed sweep must equal 'scopeScansAt'. Three seedings are checked per case: an
empty map (which must degrade to a full fold), the map the sweep itself produced,
and a map populated only from the earlier half of history (which forces the
resume to replay a genuine tail rather than land on the requested day).
-}
resumedScopeScanTests :: TestTree
resumedScopeScanTests =
    testGroup
        "scopeScansResumed agrees with scopeScansAt"
        (concatMap (fixtureCases assertResumedScansMatch) scopeScanFixtures)

assertResumedScansMatch ::
    Text -> AssetClassMappings -> InvestmentMappings -> Maybe Text -> Granularity -> IO ()
assertResumedScansMatch path assetClasses mappings scope granularity =
    withFixture path $ \snapshot -> do
        let context = prepareInvestmentPerformance snapshot assetClasses mappings "INR"
            chain = chainOf snapshot.journal
            days = requestDays snapshot chain assetClasses scope granularity
            expected = scopeScansAt context scope days
            (cold, populated) = scopeScansResumed context chain emptyCheckpoints scope days
            (warm, _) = scopeScansResumed context chain populated scope days
            (partial, seeded) = scopeScansResumed context chain emptyCheckpoints scope (earlyHalf days)
            (resumedFromPartial, _) = scopeScansResumed context chain seeded scope days
        assertScansEqual "empty seed" expected cold
        assertBool
            "the sweep must checkpoint at least one month, or nothing is being tested"
            (checkpointCount populated > 0)
        assertScansEqual "seeded from its own checkpoints" expected warm
        assertEqual
            "the partial sweep emits one scan per requested day"
            (length (earlyHalf days))
            (length partial)
        assertScansEqual "seeded from earlier history only" expected resumedFromPartial

{- | Field by field rather than by 'Eq', so a failure names the field that moved.
Cash flows are compared in order: unlike the per-day oracle in
'Hedger.Domain.InvestmentPerformanceSpec', both sides here come from the same
chronological sweep, so the order is part of what must agree.
-}
assertScansEqual :: String -> [ScopeScan] -> [ScopeScan] -> IO ()
assertScansEqual label expected actual = do
    assertEqual (label <> ": same number of scans") (length expected) (length actual)
    sequence_
        [ assertScanEqual (label <> " scan " <> show index) left right
        | (index, left, right) <- zip3 [0 :: Int ..] expected actual
        ]

assertScanEqual :: String -> ScopeScan -> ScopeScan -> IO ()
assertScanEqual label expected actual = do
    assertEqual (label <> " accounts") expected.accounts actual.accounts
    assertEqual (label <> " mappingComplete") expected.mappingComplete actual.mappingComplete
    assertEqual (label <> " retained") expected.retained actual.retained
    assertEqual (label <> " attributed") expected.attributed actual.attributed
    assertEqual (label <> " cashFlows") (flowPairs <$> expected.cashFlows) (flowPairs <$> actual.cashFlows)

flowPairs :: [CashFlow] -> [(Day, Decimal)]
flowPairs = fmap (\flow -> (cashFlowDate flow, cashFlowAmount flow))

-- * The same equivalence through the summary entry points

{- | 'summariesAtResumed' against the retained 'summariesAtPrepared' oracle,
cold and then warm. The warm pass answers every annualised return the cold pass
solved from the rate cache, so agreement here is also the check that a cached
rate equals an uncached solve. Fixtures with no performance mappings never solve
a rate at all; 'cachedRateMatchesUncachedSolve' pins the case that does.
-}
resumedSummaryTests :: TestTree
resumedSummaryTests =
    testGroup
        "summariesAtResumed agrees with summariesAtPrepared"
        (concatMap (fixtureCases assertResumedSummariesMatch) scopeScanFixtures)

assertResumedSummariesMatch ::
    Text -> AssetClassMappings -> InvestmentMappings -> Maybe Text -> Granularity -> IO ()
assertResumedSummariesMatch path assetClasses mappings scope granularity =
    withFixture path $ \snapshot -> do
        let context = prepareInvestmentPerformance snapshot assetClasses mappings "INR"
            chain = chainOf snapshot.journal
            days = requestDays snapshot chain assetClasses scope granularity
            expected = summariesAtPrepared context scope granularity days
            (cold, warmedCache) = summariesAtResumed context chain emptyScopeCache scope granularity days
            (warm, _) = summariesAtResumed context chain warmedCache scope granularity days
        assertSummariesEqual "uncached" expected cold
        assertSummariesEqual "rates answered from the cache" expected warm

{- | Solved-rate caching, on a fixture that actually produces annualised returns.

The fixture-wide tests above compare against the oracle but cannot assert that
any rate was cached, because a fixture without performance mappings never solves
one. This case pins both halves: rates are cached, and every summary a cached
rate answered matches the uncached solve.
-}
cachedRateMatchesUncachedSolve :: IO ()
cachedRateMatchesUncachedSolve =
    withFixture "test/fixtures/investment-xirr.journal" $ \snapshot -> do
        let mappings = mappingsOrEmpty [xirrMapping]
            context = prepareInvestmentPerformance snapshot testAssetClasses mappings "INR"
            chain = chainOf snapshot.journal
            days = requestDays snapshot chain testAssetClasses Nothing Monthly
            expected = summariesAtPrepared context Nothing Monthly days
            (cold, warmedCache) = summariesAtResumed context chain emptyScopeCache Nothing Monthly days
            (warm, _) = summariesAtResumed context chain warmedCache Nothing Monthly days
        assertBool
            "the fixture must produce at least one annualised return"
            (any (\result -> result.summary.xirr /= Nothing) expected)
        assertBool "the cold pass must cache solved rates" (solvedRateCount warmedCache.rates > 0)
        assertSummariesEqual "uncached solve" expected cold
        assertSummariesEqual "cached rate" expected warm

{- | The drill-down entry point, over root, nested and duplicated scopes in one
request, against the retained 'summariesAtScopesPrepared' oracle.
-}
resumedScopeSummaryTests :: TestTree
resumedScopeSummaryTests =
    testGroup
        "summariesAtScopesResumed agrees with summariesAtScopesPrepared"
        [ testCase label (assertResumedScopeSummariesMatch path assetClasses mappings scopes)
        | (label, path, assetClasses, mappings, scopes) <- scopeScanFixtures
        ]

assertResumedScopeSummariesMatch ::
    Text -> AssetClassMappings -> InvestmentMappings -> [Maybe Text] -> IO ()
assertResumedScopeSummariesMatch path assetClasses mappings configuredScopes =
    withFixture path $ \snapshot -> do
        let latest = latestDayOf snapshot
            context = prepareInvestmentPerformance snapshot assetClasses mappings "INR"
            chain = chainOf snapshot.journal
            scopes = Nothing : configuredScopes <> take 1 configuredScopes
            expected = summariesAtScopesPrepared context latest scopes
            (cold, warmedCache) = summariesAtScopesResumed context chain emptyScopeCache latest scopes
            (warm, _) = summariesAtScopesResumed context chain warmedCache latest scopes
            (empty, _) = summariesAtScopesResumed context chain emptyScopeCache latest []
        assertEqual "scope order and duplicates" (fmap fst expected) (fmap fst cold)
        assertEqual "no results for no scopes" [] (fmap fst empty)
        assertSummariesEqual "uncached" (fmap snd expected) (fmap snd cold)
        assertSummariesEqual "rates answered from the cache" (fmap snd expected) (fmap snd warm)

assertSummariesEqual :: String -> [SummaryResult] -> [SummaryResult] -> IO ()
assertSummariesEqual label expected actual = do
    assertEqual (label <> ": same number of summaries") (length expected) (length actual)
    sequence_
        [ assertSummaryResultEqual (label <> " summary " <> show index) left right
        | (index, left, right) <- zip3 [0 :: Int ..] expected actual
        ]

assertSummaryResultEqual :: String -> SummaryResult -> SummaryResult -> IO ()
assertSummaryResultEqual label expected actual = do
    assertEqual (label <> " marketValue") expected.summary.marketValue actual.summary.marketValue
    assertEqual (label <> " purchaseCost") expected.summary.purchaseCost actual.summary.purchaseCost
    assertEqual (label <> " gainLoss") expected.summary.gainLoss actual.summary.gainLoss
    assertEqual (label <> " xirr") expected.summary.xirr actual.summary.xirr
    assertEqual
        (label <> " attributedIncome")
        expected.summary.attributedIncome
        actual.summary.attributedIncome
    assertEqual (label <> " realisedGain") expected.summary.realisedGain actual.summary.realisedGain
    assertEqual (label <> " totalProfit") expected.summary.totalProfit actual.summary.totalProfit
    assertEqual (label <> " completeness") expected.summary.completeness actual.summary.completeness
    assertEqual (label <> " warnings") expected.warnings actual.warnings

-- * The month-end boundary

{- | The trap this slice can most easily fall into. A transaction dated exactly
on a month end belongs to that month, so it must be inside the checkpoint taken
there and must not be replayed on resume.

Checked by counting cash flows rather than only by comparing against a scratch
fold: a double count and a skip would both show as a wrong flow count, and the
counts are asserted absolutely so a fold that lost the transaction from both
sides could not pass.
-}
monthEndBoundaryCountedOnce :: IO ()
monthEndBoundaryCountedOnce =
    withTemporaryJournal "boundary" boundaryJournal $ \snapshot -> do
        let context = prepareInvestmentPerformance snapshot boundaryAssetClasses boundaryMappings "INR"
            chain = chainOf snapshot.journal
            januaryEnd = fromGregorian 2023 1 31
            februaryEnd = fromGregorian 2023 2 28
            (januaryOnly, seeded) = scopeScansResumed context chain emptyCheckpoints Nothing [januaryEnd]
            (resumed, _) = scopeScansResumed context chain seeded Nothing [januaryEnd, februaryEnd]
        assertEqual
            "the chain must have an entry for both months"
            [januaryEnd, februaryEnd]
            (fst <$> chain.months)
        assertEqual
            "January must be checkpointed so the resume has something to reuse"
            [januaryEnd]
            (checkpointMonths Nothing "INR" seeded)
        assertEqual
            "the month-end transaction belongs to January"
            [Just 1]
            (fmap (length <$>) (fmap (.cashFlows) januaryOnly))
        assertEqual
            "resuming past January must neither replay nor skip its month-end transaction"
            [Just 1, Just 2]
            (fmap (length <$>) (fmap (.cashFlows) resumed))
        assertScansEqual
            "resumed across the boundary"
            (scopeScansAt context Nothing [januaryEnd, februaryEnd])
            resumed

{- | The first transaction is dated on January's month end, which is where the
checkpoint lands, so replaying or skipping it changes February's cash flows.
-}
boundaryJournal :: Text
boundaryJournal =
    Text.unlines
        [ "2023-01-31 buy on the month end"
        , "    assets:investments:equity:xyz    10 XYZ @ 100.00 INR"
        , "    assets:bank:checking          -1000.00 INR"
        , "2023-02-15 buy mid month"
        , "    assets:investments:equity:xyz     5 XYZ @ 110.00 INR"
        , "    assets:bank:checking           -550.00 INR"
        , "P 2023-02-28 XYZ 120.00 INR"
        ]

boundaryAssetClasses :: AssetClassMappings
boundaryAssetClasses =
    assetClassesOrEmpty [AssetClassMapping Equity ["assets:investments"] Nothing]

boundaryMappings :: InvestmentMappings
boundaryMappings = mappingsOrEmpty [InvestmentMapping "assets:investments:equity" []]

-- * Rejecting checkpoints that must not be reused

{- | The failure mode with no other symptom. Checkpoints are populated from one
revision of a journal, then a transaction is inserted into the /first/ month, so
every stored hash is stale. Trusting any of them would silently omit the inserted
transaction from every later month.
-}
staleChainHashIsIgnored :: IO ()
staleChainHashIsIgnored =
    withJournalPath "stale" $ \path -> do
        before <- snapshotAt path boundaryJournal
        after <- snapshotAt path historicalInsertJournal
        let assetClasses = boundaryAssetClasses
            beforeContext = prepareInvestmentPerformance before assetClasses boundaryMappings "INR"
            afterContext = prepareInvestmentPerformance after assetClasses boundaryMappings "INR"
            beforeChain = chainOf before.journal
            afterChain = chainOf after.journal
            days = [fromGregorian 2023 1 31, fromGregorian 2023 2 28]
            (_, stale) = scopeScansResumed beforeContext beforeChain emptyCheckpoints Nothing days
            fromScratch = scopeScansAt afterContext Nothing days
            (withStaleSeed, _) = scopeScansResumed afterContext afterChain stale Nothing days
        assertBool "the earlier revision must have produced checkpoints" (checkpointCount stale > 0)
        assertEqual
            "the insertion must diverge at the first month"
            (DivergesFrom (fromGregorian 2023 1 31))
            (diffChains beforeChain afterChain)
        assertEqual
            "every stale checkpoint must be truncated away"
            0
            (checkpointCount (truncateCheckpoints afterChain Identical stale))
        assertScansEqual "stale checkpoints must be ignored, not trusted" fromScratch withStaleSeed
        assertBool
            "the inserted transaction must actually move the numbers, or this proves nothing"
            (fmap (.cashFlows) fromScratch /= fmap (.cashFlows) (scopeScansAt beforeContext Nothing days))

-- | An extra transaction in January, so January's hash and every later hash move.
historicalInsertJournal :: Text
historicalInsertJournal =
    Text.unlines
        [ "2023-01-10 an earlier buy"
        , "    assets:investments:equity:xyz     2 XYZ @ 90.00 INR"
        , "    assets:bank:checking           -180.00 INR"
        ]
        <> boundaryJournal

{- | A commodity declaration changes no transaction, so every month hash is
unchanged and only the global fingerprint moves. Checkpoints must still all go:
a display-style change is not date-scoped and can restate arbitrarily old output.
-}
globalChangeInvalidatesCheckpoints :: IO ()
globalChangeInvalidatesCheckpoints =
    withJournalPath "global" $ \path -> do
        before <- snapshotAt path boundaryJournal
        after <- snapshotAt path ("commodity 1000.000 INR\n\n" <> boundaryJournal)
        let context = prepareInvestmentPerformance before boundaryAssetClasses boundaryMappings "INR"
            beforeChain = chainOf before.journal
            afterChain = chainOf after.journal
            days = [fromGregorian 2023 1 31, fromGregorian 2023 2 28]
            (_, stored) = scopeScansResumed context beforeChain emptyCheckpoints Nothing days
        assertEqual
            "the declaration must change only the global fingerprint"
            GlobalChanged
            (diffChains beforeChain afterChain)
        assertEqual
            "month hashes alone cannot see a global change"
            (fmap snd beforeChain.months)
            (fmap snd afterChain.months)
        assertBool "the earlier revision must have produced checkpoints" (checkpointCount stored > 0)
        assertEqual
            "a global change must invalidate every checkpoint"
            0
            (checkpointCount (truncateCheckpoints afterChain Identical stored))
        assertEqual
            "GlobalChanged must invalidate every checkpoint against its own chain"
            0
            (checkpointCount (truncateCheckpoints beforeChain GlobalChanged stored))

{- | 'DivergesFrom' keeps the months strictly before the divergence and drops the
divergent month itself, because that month's own content is what changed.
-}
truncationDropsFromDivergence :: IO ()
truncationDropsFromDivergence =
    withTemporaryJournal "truncate" threeMonthJournal $ \snapshot -> do
        let context = prepareInvestmentPerformance snapshot boundaryAssetClasses boundaryMappings "INR"
            chain = chainOf snapshot.journal
            monthEnds = fst <$> chain.months
            (_, stored) = scopeScansResumed context chain emptyCheckpoints Nothing monthEnds
        assertEqual "three populated months" 3 (length monthEnds)
        assertEqual "all three are checkpointed" monthEnds (checkpointMonths Nothing "INR" stored)
        sequence_
            [ assertEqual
                ("divergence at " <> show divergence <> " keeps the earlier months")
                (filter (< divergence) monthEnds)
                (checkpointMonths Nothing "INR" (truncateCheckpoints chain (DivergesFrom divergence) stored))
            | divergence <- monthEnds
            ]
        assertEqual
            "an identical chain keeps every checkpoint"
            monthEnds
            (checkpointMonths Nothing "INR" (truncateCheckpoints chain Identical stored))

threeMonthJournal :: Text
threeMonthJournal =
    boundaryJournal
        <> Text.unlines
            [ "2023-03-20 buy again"
            , "    assets:investments:equity:xyz     3 XYZ @ 130.00 INR"
            , "    assets:bank:checking           -390.00 INR"
            ]

-- * Retention

{- | The policy is "every twelfth month plus the last 'denseTailMonths'", counted
from the oldest month so that the choice of retained months does not shift as
history grows at the near end.
-}
retentionKeepsClaimedMonths :: IO ()
retentionKeepsClaimedMonths = do
    let total = 40
        months = distinctMonths total
        retained = retainedMonths months
        denseTail = drop (total - denseTailMonths) months
        sparseHead = take (total - denseTailMonths) months
    assertEqual
        "the whole dense tail is retained, without gaps"
        denseTail
        (dropWhile (`notElem` denseTail) retained)
    assertEqual
        "before the dense tail, one month in twelve counting from the oldest"
        [day | (index, day) <- zip [0 :: Int ..] sparseHead, index `mod` sparseCheckpointInterval == 0]
        (filter (`elem` sparseHead) retained)
    assertEqual
        "40 months retain the dense tail of 24 plus months 0 and 12 of the 16-month head"
        26
        (length retained)
    assertEqual "retained months stay in chain order" (filter (`elem` retained) months) retained

retentionKeepsShortHistory :: IO ()
retentionKeepsShortHistory = do
    let months = distinctMonths denseTailMonths
    assertEqual
        "a history no longer than the dense tail is retained entirely"
        months
        (retainedMonths months)
    assertEqual "an empty chain retains nothing" [] (retainedMonths [])

-- | Distinct, increasing stand-ins for month ends; only their positions matter.
distinctMonths :: Int -> [Day]
distinctMonths count = take count [fromGregorian 2020 1 1 ..]

-- * Fixtures and shared helpers

{- | The requested days a real report would ask for, plus the chain's month ends.

The month ends matter: a fixture's report periods can land entirely mid-month, in
which case nothing is checkpointed and an equivalence test passes vacuously.
Including them guarantees the resumed path is exercised while still covering the
mid-month days that a real request produces.
-}
requestDays :: Snapshot -> PrefixChain -> AssetClassMappings -> Maybe Text -> Granularity -> [Day]
requestDays snapshot chain assetClasses scope granularity =
    List.sort (List.nub (reportDays <> (fst <$> chain.months)))
  where
    transactions = investmentDataTransactions snapshot assetClasses scope
    reportDays = [end | (_, _, end) <- reportPeriods granularity transactions (latestDayOf snapshot)]

-- | Enough of the day list to leave a tail for the resume to replay.
earlyHalf :: [Day] -> [Day]
earlyHalf days = take (max 1 (length days `div` 2)) days

latestDayOf :: Snapshot -> Day
latestDayOf snapshot =
    fromMaybe (error "fixture has no journal date") snapshot.latestJournalDate

chainOf :: Hledger.Journal -> PrefixChain
chainOf journal =
    prefixChain (globalFingerprint journal configuration) journal.jtxns journal.jpricedirectives

withFixture :: Text -> (Snapshot -> IO a) -> IO a
withFixture path action = do
    loaded <- loadSnapshot path
    case loaded of
        Left problem -> assertFailure (show problem)
        Right snapshot -> action snapshot

snapshotAt :: FilePath -> Text -> IO Snapshot
snapshotAt path contents = do
    TextIO.writeFile path contents
    loaded <- loadSnapshot (Text.pack path)
    case loaded of
        Left problem -> assertFailure ("journal load failed: " <> show problem)
        Right snapshot -> pure snapshot

withTemporaryJournal :: String -> Text -> (Snapshot -> IO a) -> IO a
withTemporaryJournal label contents action =
    withJournalPath label (\path -> snapshotAt path contents >>= action)

{- | One directory per case, and one path within it, so the global fingerprint's
file-set component is held constant across a case's revisions.
-}
withJournalPath :: String -> (FilePath -> IO a) -> IO a
withJournalPath label action =
    bracket acquire Directory.removeDirectoryRecursive (\dir -> action (dir </> "main.journal"))
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-investment-checkpoint-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir

fixtureCases ::
    (Text -> AssetClassMappings -> InvestmentMappings -> Maybe Text -> Granularity -> IO ()) ->
    (String, Text, AssetClassMappings, InvestmentMappings, [Maybe Text]) ->
    [TestTree]
fixtureCases assertion (label, path, assetClasses, mappings, scopes) =
    [ testCase
        (label <> " " <> show granularity <> " scope=" <> show scope)
        (assertion path assetClasses mappings scope granularity)
    | granularity <- [Monthly, Yearly]
    , scope <- scopes
    ]

{- | (label, journal path, asset classes, investment mappings, scopes to check),
mirroring 'Hedger.Domain.InvestmentPerformanceSpec''s matrix so the checkpointed
path is measured over exactly the cases the unchecked path is. Every fixture is
covered at the root scope plus at least one narrower scope.
-}
scopeScanFixtures :: [(String, Text, AssetClassMappings, InvestmentMappings, [Maybe Text])]
scopeScanFixtures =
    [
        ( "investments.journal"
        , "test/fixtures/investments.journal"
        , testAssetClasses
        , mappingsOrEmpty [fdMapping, equityMapping]
        , [Nothing, Just "assets:investments:equity:xyz"]
        )
    ,
        ( "investment-descendant.journal"
        , "test/fixtures/investment-descendant.journal"
        , testAssetClasses
        , mappingsOrEmpty [parentFdMapping]
        , [Nothing, Just "assets:investments:fd:hdfc"]
        )
    ,
        ( "investment-foreign-cash.journal"
        , "test/fixtures/investment-foreign-cash.journal"
        , testAssetClasses
        , mappingsOrEmpty [InvestmentMapping "assets:cash:brokerage" []]
        , [Nothing, Just "assets:cash:brokerage"]
        )
    ,
        ( "investment-xirr.journal"
        , "test/fixtures/investment-xirr.journal"
        , testAssetClasses
        , mappingsOrEmpty [xirrMapping]
        , [Nothing, Just "assets:investments:fund:b"]
        )
    ,
        ( "investment-ambiguous-xirr.journal"
        , "test/fixtures/investment-ambiguous-xirr.journal"
        , testAssetClasses
        , mappingsOrEmpty [ambiguousMapping]
        , [Nothing]
        )
    ,
        ( "investment-gapped-price.journal"
        , "test/fixtures/investment-gapped-price.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-never-priced.journal"
        , "test/fixtures/investment-never-priced.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-gain-fee-roles.journal"
        , "test/fixtures/investment-gain-fee-roles.journal"
        , edgeCaseAssetClasses
        , gainFeeRoleMappings
        , [Nothing, Just "assets:investments:equity"]
        )
    ,
        ( "investment-ambiguous-xirr.journal (wide asset classes)"
        , "test/fixtures/investment-ambiguous-xirr.journal"
        , edgeCaseAssetClasses
        , mappingsOrEmpty [ambiguousMapping]
        , [Nothing]
        )
    ,
        ( "investment-dust-commodity.journal"
        , "test/fixtures/investment-dust-commodity.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-nested-mapping.journal"
        , "test/fixtures/investment-nested-mapping.journal"
        , edgeCaseAssetClasses
        , nestedMappings
        , [Nothing, Just "assets:investments", Just "assets:investments:equity:xyz"]
        )
    ]

testAssetClasses :: AssetClassMappings
testAssetClasses =
    assetClassesOrEmpty
        [ AssetClassMapping Equity ["assets:investments"] Nothing
        , AssetClassMapping Cash ["assets:cash:brokerage"] Nothing
        ]

-- | Wide enough to cover every account touched by the edge-case journals.
edgeCaseAssetClasses :: AssetClassMappings
edgeCaseAssetClasses =
    assetClassesOrEmpty
        [ AssetClassMapping Equity ["assets:investments"] Nothing
        , AssetClassMapping Cash ["assets:cash"] Nothing
        ]

fdMapping :: InvestmentMapping
fdMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:fd:hdfc"
        , performanceAccounts = [PerformanceAccount "income:investments:fd:interest" Interest]
        }

parentFdMapping :: InvestmentMapping
parentFdMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:fd"
        , performanceAccounts = [PerformanceAccount "income:investments:fd:interest" Interest]
        }

equityMapping :: InvestmentMapping
equityMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:equity:xyz"
        , performanceAccounts = [PerformanceAccount "income:investments:xyz:gains" RealisedGain]
        }

xirrMapping :: InvestmentMapping
xirrMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:fund"
        , performanceAccounts =
            [ PerformanceAccount "income:investments:fund:dividend" Interest
            , PerformanceAccount "expenses:investments:fund:fee" Fee
            ]
        }

ambiguousMapping :: InvestmentMapping
ambiguousMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:ambiguous"
        , performanceAccounts = []
        }

gainFeeRoleMappings :: InvestmentMappings
gainFeeRoleMappings =
    mappingsOrEmpty
        [ InvestmentMapping
            { assetAccount = "assets:investments:equity"
            , performanceAccounts =
                [ PerformanceAccount "income:investments:xyz:gains" RealisedGain
                , PerformanceAccount "expenses:investments:fee" Fee
                ]
            }
        ]

{- | Parent and child both map to the same asset class, so a scope nested inside
either exercises the scope-narrowing predicates together.
-}
nestedMappings :: InvestmentMappings
nestedMappings =
    mappingsOrEmpty
        [ InvestmentMapping
            { assetAccount = "assets:investments"
            , performanceAccounts = [PerformanceAccount "income:investments:dividend" Interest]
            }
        ]

mappingsOrEmpty :: [InvestmentMapping] -> InvestmentMappings
mappingsOrEmpty mappings = case mkInvestmentMappings mappings of
    Left _ -> emptyInvestmentMappings
    Right value -> value

assetClassesOrEmpty :: [AssetClassMapping] -> AssetClassMappings
assetClassesOrEmpty mappings = case mkAssetClassMappings mappings of
    Left _ -> emptyAssetClassMappings
    Right value -> value
