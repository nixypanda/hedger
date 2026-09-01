{- | The analytics use cases reading the checkpoint caches.

The decisive tests are the sweeps: over every fixture, both granularities and
both commodities, a request that resumes from published checkpoints must project
exactly the figures the unchecked path projects — cold, and again warm from the
checkpoints the cold request contributed back. A sweep case also asserts that the
cold request /did/ contribute something, because a resume that quietly fell back
to the unchecked path would otherwise agree vacuously.

The remaining cases pin the guards, each of which can fail without any sweep case
noticing:

* An empty derived view must fall back. There is no chain to resume against
  before the first rebuild completes, and a request arriving then still has to
  answer.
* A derived view whose revision is not the snapshot's must fall back. Its
  checkpoints were keyed against a different chain, so a month that /did/ change
  still hash-matches that chain and would be reused. This is the guard with no
  other symptom, and 'historicalEditWithStaleDerivedView' is the only thing in
  the suite that catches its removal.
* A query's cache key must carry the reference date it was resolved against, not
  its text alone: @date:thismonth@ denotes a different span once an append moves
  @latestJournalDate@, and it can restate arbitrarily old columns that the month
  chain would not flag.
-}
module Hedger.Application.CheckpointsSpec (checkpointsSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Decimal (Decimal)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (Day, fromGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Types qualified as Hledger

import Hedger.Application.Analytics (
    AnalyticsRequest (..),
    CashflowReport (..),
    CategoryBreakdown (..),
    ConversionWarning (..),
    Kpi (..),
    NetWorthReport (..),
    cashflow,
    netWorth,
 )
import Hedger.Application.AppM (AppM, Env (..), runAppM)
import Hedger.Application.Investments (
    AssetClassAllocation (..),
    InvestmentBreadcrumb (..),
    InvestmentChild (..),
    InvestmentPoint (..),
    InvestmentsReport (..),
    InvestmentsRequest (..),
    investments,
 )
import Hedger.Domain.Fingerprint (PrefixChain, globalFingerprint, prefixChain)
import Hedger.Domain.Internal.CashflowCheckpoint (columnCount, columnMonths)
import Hedger.Domain.Internal.InvestmentCheckpoint (checkpointCount)
import Hedger.Domain.Internal.NetWorthCheckpoint (
    QueryKey (..),
    openingBalanceCount,
    openingBalanceDays,
 )
import Hedger.Domain.Internal.PeriodReport (BreakdownItem (..), TimelinePoint (..))
import Hedger.Domain.InvestmentPerformance (
    InvestmentSummary (..),
    ScopeCache (..),
 )
import Hedger.Domain.NetWorth (PerformancePoint (..))
import Hedger.Domain.Period (Granularity (..), epoch)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches (..),
    DerivedView (..),
    emptyDerivedCaches,
    newDerivedState,
    publishDerivedWith,
    readDerivedView,
 )
import Hedger.Infrastructure.DerivedStore (newDerivedStore)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )
import Hedger.TestHelpers (configuration)

checkpointsSpec :: TestTree
checkpointsSpec =
    testGroup
        "Application checkpoint resume"
        [ resumedRequestTests
        , testCase
            "a resumed request contributes its checkpoints back"
            resumedRequestContributesCheckpoints
        , testCase
            "an empty derived view falls back and still answers"
            emptyDerivedViewFallsBack
        , testCase
            "a derived view from another revision is not resumed from"
            historicalEditWithStaleDerivedView
        , testCase
            "a relative-date query is keyed by its reference date"
            relativeDateQueryIsKeyedByReferenceDate
        ]

-- * The sweep

{- | Every fixture, both granularities, both commodities, all three use cases.

One case covers three families rather than one so the sweep stays affordable: a
family that diverges names itself in the assertion label.
-}
resumedRequestTests :: TestTree
resumedRequestTests =
    testGroup
        "resumed requests agree with the unchecked path"
        [ testCase caseLabel (source caseLabel (assertResumedRequestsAgree granularity commodity))
        | (label, source) <- sweepSources
        , granularity <- [Monthly, Yearly]
        , commodity <- ["INR", "USD"]
        , let caseLabel = label <> " " <> show granularity <> " " <> Text.unpack commodity
        ]

assertResumedRequestsAgree :: Granularity -> Text -> Text -> IO ()
assertResumedRequestsAgree granularity commodity path = do
    unchecked <- withEnvironment path (\environment _ -> reportShapes environment granularity commodity)
    (cold, warm) <- withEnvironment path $ \environment snapshot -> do
        publishChainFor environment snapshot
        cold <- reportShapes environment granularity commodity
        warm <- reportShapes environment granularity commodity
        pure (cold, warm)
    assertEqual "the cold resumed request must match the unchecked path" unchecked cold
    assertEqual "the warm resumed request must match the unchecked path" unchecked warm

{- | The checkpoints held by a derived view.

The sweep cannot assert these: most of the file fixtures span too few months for
any retention policy to keep a checkpoint at all, so a sweep assertion would
either be vacuous or wrong depending on the fixture.
'resumedRequestContributesCheckpoints' asserts them on the one journal long
enough to retain something.
-}
data RetainedCounts = RetainedCounts
    { openings :: Int
    , columns :: Int
    , scopeCheckpoints :: Int
    }
    deriving stock (Eq, Show)

retainedCounts :: Env -> IO RetainedCounts
retainedCounts environment = do
    derivedView <- readDerivedView environment.derivedState
    pure
        RetainedCounts
            { openings = openingBalanceCount derivedView.caches.netWorth
            , columns = columnCount derivedView.caches.cashflow
            , scopeCheckpoints = checkpointCount derivedView.caches.scopes.checkpoints
            }

assertRetainedSomething :: RetainedCounts -> IO ()
assertRetainedSomething retained = do
    assertBool "net worth contributed opening balances back" (retained.openings > 0)
    assertBool "cashflow contributed columns back" (retained.columns > 0)
    assertBool "investments contributed scope checkpoints back" (retained.scopeCheckpoints > 0)

{- | The write-back itself, on a journal long enough to retain checkpoints.

Without this the sweep would pass just as happily against a resume whose updated
caches were never contributed back: the figures would be right every time and
nothing would ever be reused.
-}
resumedRequestContributesCheckpoints :: IO ()
resumedRequestContributesCheckpoints =
    withTemporaryJournal "contributes" (syntheticJournal 26 "100.00" "1") $ \path ->
        withEnvironment path $ \environment snapshot -> do
            publishChainFor environment snapshot
            _ <- reportShapes environment Monthly "INR"
            cold <- retainedCounts environment
            assertRetainedSomething cold
            _ <- reportShapes environment Monthly "INR"
            warm <- retainedCounts environment
            assertEqual "a repeated request adds nothing and removes nothing" cold warm

-- * The guards

{- | A request arriving before the first rebuild has no chain to resume against
and must answer from the unchecked path.
-}
emptyDerivedViewFallsBack :: IO ()
emptyDerivedViewFallsBack =
    withTemporaryJournal "empty-derived-view" (syntheticJournal 26 "100.00" "1") $ \path -> do
        expected <- withEnvironment path (\environment _ -> reportShapes environment Monthly "INR")
        withEnvironment path $ \environment _ -> do
            derivedView <- readDerivedView environment.derivedState
            assertBool "the derived view under test must have no chain" (isNothing derivedView.chain)
            actual <- reportShapes environment Monthly "INR"
            assertEqual "an unpublished derived view still answers correctly" expected actual
            retained <- retainedCounts environment
            assertEqual
                "nothing is contributed back when there is no chain to key against"
                RetainedCounts{openings = 0, columns = 0, scopeCheckpoints = 0}
                retained

{- | The decisive test for the revision guard.

A journal is edited in its /first/ month — before the 24-month window opens, so
every later column, cashflow year and scope scan depends on that month through a
checkpoint. The derived view is deliberately left describing the journal as it
was: the caches it holds were keyed against that older chain, and against that
older chain they all still hash-match. Resuming would therefore accept every one
of them and serve the journal that no longer exists. Only the revision check
rejects them.
-}
historicalEditWithStaleDerivedView :: IO ()
historicalEditWithStaleDerivedView =
    withJournalPath "stale-derived-view" $ \path -> do
        writeJournal path (syntheticJournal 26 "100.00" "1")
        expected <- afterHistoricalEdit
        store <- newSnapshotStore
        environment <- environmentFor store
        before <- loadInto store (Text.pack path)
        publishChainFor environment before
        _ <- reportShapes environment Monthly "INR"
        retained <- retainedCounts environment
        assertRetainedSomething retained
        writeJournal path (syntheticJournal 26 "900.00" "3")
        after <- loadInto store (Text.pack path)
        assertBool "the edit must change the snapshot revision" (after.revision /= before.revision)
        actual <- reportShapes environment Monthly "INR"
        assertEqual "the edited journal is reported, not the checkpointed one" expected actual

-- | The same edited journal read with no derived view at all.
afterHistoricalEdit :: IO (String, String, String)
afterHistoricalEdit =
    withTemporaryJournal "stale-derived-view-reference" (syntheticJournal 26 "900.00" "3") $ \path ->
        withEnvironment path (\environment _ -> reportShapes environment Monthly "INR")

{- | @date:thismonth@ resolves against @latestJournalDate@, so its text alone
does not fix its meaning. Net worth's key carries the reference date as a field;
cashflow's carries it inside the canonical text, which is why the raw text finds
nothing.
-}
relativeDateQueryIsKeyedByReferenceDate :: IO ()
relativeDateQueryIsKeyedByReferenceDate =
    withTemporaryJournal "reference-date" (syntheticJournal 26 "100.00" "1") $ \path ->
        withEnvironment path $ \environment snapshot -> do
            publishChainFor environment snapshot
            let request = (analyticsRequest Monthly "INR"){query = Just relativeQuery}
            _ <- runOrFail environment (netWorth request)
            _ <- runOrFail environment (cashflow request)
            caches <- (.caches) <$> readDerivedView environment.derivedState
            let reference = fromMaybe epoch snapshot.latestJournalDate
                keyAt referenceDay = QueryKey{text = relativeQuery, reference = referenceDay}
            assertBool
                "opening balances are keyed at the query's own reference date"
                (not (null (openingBalanceDays (keyAt reference) "INR" caches.netWorth)))
            assertEqual
                "no opening balance is keyed at a different reference date"
                []
                (openingBalanceDays (keyAt epoch) "INR" caches.netWorth)
            assertBool "cashflow cached columns for the query" (columnCount caches.cashflow > 0)
            assertEqual
                "no cashflow column is keyed by the query text alone"
                []
                (columnMonths relativeQuery "INR" caches.cashflow)

relativeQuery :: Text
relativeQuery = "date:thismonth"

-- * Running the use cases

{- | The three use cases' figures, projected to a comparable shape.

The reports themselves are neither 'Eq' nor 'Show': they carry
'Hedger.Application.Common.SnapshotMetadata', whose freshness legitimately
differs between a published and an unpublished derived view, and an account
classifier function. Every /calculated/ field is projected here.
-}
reportShapes :: Env -> Granularity -> Text -> IO (String, String, String)
reportShapes environment granularity commodity = do
    netWorthReport <- runOrFail environment (netWorth (analyticsRequest granularity commodity))
    cashflowReport <- runOrFail environment (cashflow (analyticsRequest granularity commodity))
    investmentsReport <- runOrFail environment (investments (investmentsRequest granularity commodity))
    pure
        ( netWorthShape netWorthReport
        , cashflowShape cashflowReport
        , investmentsShape investmentsReport
        )

runOrFail :: Env -> AppM value -> IO value
runOrFail environment action = do
    result <- runAppM environment action
    either (\problem -> assertFailure ("use case failed: " <> show problem)) pure result

analyticsRequest :: Granularity -> Text -> AnalyticsRequest
analyticsRequest granularity commodity =
    AnalyticsRequest
        { query = Nothing
        , granularity = granularity
        , displayCommodity = Just commodity
        , positivePeriod = Nothing
        , negativePeriod = Nothing
        , positiveDepth = 2
        , negativeDepth = 2
        }

investmentsRequest :: Granularity -> Text -> InvestmentsRequest
investmentsRequest granularity commodity =
    InvestmentsRequest
        { granularity = granularity
        , displayCommodity = Just commodity
        , accountPrefix = Nothing
        }

-- * Comparable projections

netWorthShape :: NetWorthReport -> String
netWorthShape report =
    show
        ( report.displayCommodity
        , kpiShape report.netWorthKpi
        , kpiShape report.assetsKpi
        , kpiShape report.liabilitiesKpi
        , fmap timelineShape report.timeline
        , breakdownShape report.assetsBreakdown
        , breakdownShape report.liabilitiesBreakdown
        , fmap performanceShape report.performance
        , report.incompletePeriods
        , fmap warningShape report.warnings
        )

cashflowShape :: CashflowReport -> String
cashflowShape report =
    show
        ( report.displayCommodity
        , report.totalCount
        , report.filteredCount
        , report.timelineAvailable
        , fmap timelineShape report.timeline
        , breakdownShape report.incomeBreakdown
        , breakdownShape report.expenseBreakdown
        , fmap warningShape report.warnings
        )

investmentsShape :: InvestmentsReport -> String
investmentsShape report =
    show
        ( report.displayCommodity
        , report.accountPrefix
        , summaryShape report.summary
        , [(point.period, point.startDate, summaryShape point.summary) | point <- report.timeline]
        , [(crumb.account, crumb.label) | crumb <- report.breadcrumbs]
        , fmap childShape report.children
        , fmap allocationShape report.assetClassAllocations
        , fmap show report.warnings
        )

kpiShape :: Kpi -> (Decimal, Maybe Decimal, Maybe Decimal, [Decimal])
kpiShape kpi = (kpi.value, kpi.absoluteChange, kpi.percentageChange, kpi.trend)

timelineShape :: TimelinePoint -> (Text, Day, Decimal, Decimal, Decimal, Set Text)
timelineShape point =
    (point.period, point.startDate, point.positive, point.negative, point.net, point.missing)

performanceShape :: PerformancePoint -> (Text, Day, Decimal, Decimal, Decimal)
performanceShape point =
    (point.period, point.startDate, point.netInvestment, point.netWorth, point.gainLoss)

breakdownShape :: CategoryBreakdown -> (Text, Decimal, [(Text, Decimal)], [(Text, Decimal)])
breakdownShape breakdown =
    ( breakdown.period
    , breakdown.total
    , fmap itemShape breakdown.items
    , fmap itemShape breakdown.otherItems
    )

itemShape :: BreakdownItem -> (Text, Decimal)
itemShape item = (item.account, item.quantity)

warningShape :: ConversionWarning -> Set Text
warningShape (MissingConversion missing) = missing

summaryShape ::
    InvestmentSummary ->
    ( Maybe Decimal
    , Maybe Decimal
    , Maybe Decimal
    , Maybe Decimal
    , Maybe Decimal
    , Maybe Decimal
    , Maybe Decimal
    , String
    )
summaryShape summary =
    ( summary.marketValue
    , summary.purchaseCost
    , summary.gainLoss
    , summary.xirr
    , summary.attributedIncome
    , summary.realisedGain
    , summary.totalProfit
    , show summary.completeness
    )

childShape ::
    InvestmentChild ->
    (Text, Text, Bool, String, Maybe Decimal, (Maybe Decimal, Maybe Decimal, Maybe Decimal, Maybe Decimal, Maybe Decimal, Maybe Decimal, Maybe Decimal, String))
childShape child =
    ( child.account
    , child.label
    , child.hasChildren
    , show child.assetClass
    , child.allocationPercentage
    , summaryShape child.summary
    )

allocationShape :: AssetClassAllocation -> (String, Maybe Decimal, Maybe Decimal, Maybe Decimal)
allocationShape allocation =
    ( show allocation.assetClass
    , allocation.marketValue
    , allocation.currentPercentage
    , allocation.targetPercentage
    )

-- * Environments

withEnvironment :: Text -> (Env -> Snapshot -> IO a) -> IO a
withEnvironment path run = do
    store <- newSnapshotStore
    snapshot <- loadInto store path
    environment <- environmentFor store
    run environment snapshot

environmentFor :: SnapshotStore -> IO Env
environmentFor store = do
    derivedStore <- newDerivedStore (8 * 1024 * 1024)
    derivedState <- newDerivedState
    pure Env{config = configuration, snapshotStore = store, derivedStore, derivedState}

loadInto :: SnapshotStore -> Text -> IO Snapshot
loadInto store path = do
    loaded <- reloadSnapshot store path
    case loaded of
        Left problem -> assertFailure ("snapshot load failed: " <> show problem)
        Right _ ->
            readCurrentSnapshot store
                >>= maybe (assertFailure "no snapshot published") pure

-- | Publish the chain of exactly this snapshot, with the caches a rebuild starts from.
publishChainFor :: Env -> Snapshot -> IO ()
publishChainFor environment snapshot =
    publishDerivedWith
        environment.derivedState
        snapshot.revision
        (chainOf snapshot.journal)
        emptyDerivedCaches

chainOf :: Hledger.Journal -> PrefixChain
chainOf journal =
    prefixChain (globalFingerprint journal configuration) journal.jtxns journal.jpricedirectives

-- * Journals

{- | The fixtures, plus a journal long enough for the monthly window to open
after the journal starts.

Every file fixture is short, so none of them alone would ever exercise a resume
from an earlier month; the synthetic journal is what does, and it is also the
only shape in which a historical edit is distinguishable from an append.
-}
sweepSources :: [(String, String -> (Text -> IO ()) -> IO ())]
sweepSources =
    ( "26 monthly deposits"
    , \label -> withTemporaryJournal label (syntheticJournal 26 "100.00" "1")
    )
        : [(Text.unpack path, \_ run -> run path) | path <- fixtures]

fixtures :: [Text]
fixtures =
    [ "test/fixtures/net-worth-future-price.journal"
    , "test/fixtures/shared.journal"
    , "test/fixtures/investments.journal"
    , "test/fixtures/investment-never-priced.journal"
    , "test/fixtures/investment-gapped-price.journal"
    , "test/fixtures/investment-foreign-cash.journal"
    , "test/fixtures/investment-dust-commodity.journal"
    , "test/fixtures/investment-nested-mapping.journal"
    , "test/fixtures/investment-gain-fee-roles.journal"
    ]

{- | @count@ consecutive months from January 2022, each with a deposit, a share
purchase, a month-end card purchase and a month-end price.

Long enough that the monthly window of 24 columns opens after the journal starts,
which is the only shape in which a resume has an earlier month to open from. The
first month's deposit and share count are parameters because that month is
/before/ the window: editing it is a historical change that every later column
inherits through a checkpoint, and an append never is.
-}
syntheticJournal :: Int -> Text -> Text -> Text
syntheticJournal count firstDeposit firstUnits =
    Text.unlines (concatMap monthEntries [0 .. count - 1])
  where
    monthEntries index =
        let mid = monthDay index 15
            end = monthDay index 31
            deposit = if index == 0 then firstDeposit else "100.00"
            units = if index == 0 then firstUnits else "1"
         in [ dayText mid <> " deposit"
            , "    assets:bank:checking      " <> deposit <> " INR"
            , "    income:salary"
            , ""
            , dayText mid <> " buy"
            , "    assets:investments:equity:xyz    " <> units <> " XYZ @ 50.00 INR"
            , "    assets:bank:checking"
            , ""
            , dayText end <> " card purchase"
            , "    expenses:food             25.00 INR"
            , "    liabilities:card"
            , ""
            , "P " <> dayText end <> " XYZ 60.00 INR"
            , "P " <> dayText end <> " USD 80.00 INR"
            , ""
            ]
    monthDay index dayOfMonth =
        let (year, month) = (2022 + (index `div` 12), 1 + (index `mod` 12))
         in fromGregorian (fromIntegral (year :: Int)) month dayOfMonth

dayText :: Day -> Text
dayText = Text.pack . show

withTemporaryJournal :: String -> Text -> (Text -> IO a) -> IO a
withTemporaryJournal label contents action =
    withJournalPath label $ \path -> do
        writeJournal path contents
        action (Text.pack path)

writeJournal :: FilePath -> Text -> IO ()
writeJournal = TextIO.writeFile

{- | One directory per case, and one path within it, so the global fingerprint's
file-set component is held constant across a case's revisions.
-}
withJournalPath :: String -> (FilePath -> IO a) -> IO a
withJournalPath label action =
    bracket acquire Directory.removeDirectoryRecursive (\dir -> action (dir </> "main.journal"))
  where
    acquire = do
        base <- Directory.getTemporaryDirectory
        let dir = base </> ("hedger-application-checkpoints-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir
