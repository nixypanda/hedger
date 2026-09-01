{- | Checkpointed net-worth columns measured against the unchecked report.

The decisive test is 'resumedReportTests': for every fixture, both
granularities, both commodities and three depths, the columns accumulated from
opening balances must project to exactly the totals, missing commodities and
breakdown items the full consolidated report projects to. 'resumedFiguresTests'
carries the same comparison through 'computeNetWorth', which is what pins the
strict KPIs and the conversion warnings.

The remaining cases pin the properties that equality depends on, each of which
can fail without any of the sweep cases noticing: the month-end boundary, the
rejection of a checkpoint whose prefix has moved, the global fingerprint that no
month hash can see, hledger's zero-row elision, and the retention policy.
-}
module Hedger.Domain.NetWorthCheckpointSpec (netWorthCheckpointSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Decimal (Decimal)
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (Day, addDays, fromGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    PrefixChain (..),
    chainHashAt,
    diffChains,
    globalFingerprint,
    prefixChain,
 )
import Hedger.Domain.Internal.NetWorth (assetLiabilityAccountTypes, assetLiabilityPeriodicReport)
import Hedger.Domain.Internal.NetWorthCheckpoint (
    OpeningBalances,
    QueryKey (..),
    ResumedBalanceSheet (..),
    assetLiabilityResumed,
    emptyOpeningBalances,
    openingBalanceCount,
    openingBalanceDays,
    openingHashAt,
    retainedOpeningDays,
    truncateOpeningBalances,
 )
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    ReportSide (..),
    breakdownItemsFor,
    periodicBalanceColumns,
    reportMissing,
    reportPeriodsFrom,
    reportSide,
    reportTotals,
 )
import Hedger.Domain.NetWorth (
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
    computeNetWorthResumed,
 )
import Hedger.Domain.Period (Granularity (..), ReportPeriod (..), epoch, lastMaybe)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..))
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.TestHelpers (configuration)

netWorthCheckpointSpec :: TestTree
netWorthCheckpointSpec =
    testGroup
        "net worth checkpoints"
        [ resumedReportTests
        , resumedFiguresTests
        , testCase
            "a resume across a month end counts a month-end transaction once"
            monthEndBoundaryCountedOnce
        , testCase
            "an opening balance whose chain hash has moved is ignored rather than trusted"
            staleChainHashIsIgnored
        , testCase
            "a global fingerprint change invalidates every opening balance"
            globalChangeInvalidatesOpenings
        , testCase
            "an opening stored under one query reference date is not reused under another"
            queryReferenceDateIsPartOfTheKey
        , testCase
            "truncation drops exactly the openings at or after the divergence month"
            truncationDropsFromDivergence
        , testCase
            "the rows hledger retains for a closed account and a residual are retained too"
            retainedRowsAgree
        , testCase
            "a withheld KPI stays withheld when the columns are resumed"
            withheldKpiSurvivesResume
        , testCase
            "retention keeps the window opening on top of the twelve-month grid"
            retentionKeepsWindowOpening
        , testCase
            "retention of an empty or single-month history keeps what it is given"
            retentionKeepsShortHistory
        ]

-- * The decisive equivalence: resumed columns against the unchecked report

{- | Every fixture, both granularities, both commodities, three depths.

Three seedings are compared per case: an empty map, which must degrade to the
unchecked answer; the map the cold sweep itself produced, which is the resume the
service would actually perform; and that map truncated back to its earlier
months, which forces the accumulation to replay a longer tail.
-}
resumedReportTests :: TestTree
resumedReportTests =
    testGroup
        "resumed columns agree with the consolidated report"
        [ testCase caseLabel $
            source caseLabel (\snapshot -> assertResumedReportMatches snapshot granularity commodity depth)
        | (label, source) <- sweepSources
        , granularity <- [Monthly, Yearly]
        , commodity <- ["INR", "USD"]
        , depth <- [1, 2, 3]
        , let caseLabel =
                label <> " " <> show granularity <> " " <> Text.unpack commodity <> " depth " <> show depth
        ]

assertResumedReportMatches :: Snapshot -> Granularity -> Text -> Int -> IO ()
assertResumedReportMatches snapshot granularity commodity depth = do
    let matched = snapshot.transactions
        query = queryKeyFor snapshot
        chain = chainOf snapshot.journal
        resume = assetLiabilityResumed snapshot matched query chain
        full = assetLiabilityPeriodicReport snapshot matched granularity commodity
        cold = resume emptyOpeningBalances granularity commodity
        warm = resume cold.openings granularity commodity
        partial = resume (earlierOpenings query commodity chain cold.openings) granularity commodity
    assertBool "the cold sweep must accumulate rather than fall back" cold.accumulated
    assertEqual "the cold sweep has nothing to resume from" Nothing cold.resumedFrom
    assertBool "the cold sweep must retain at least one opening balance" (openingBalanceCount cold.openings > 0)
    assertBool "the warm sweep must accumulate rather than fall back" warm.accumulated
    assertEqual
        "the warm sweep resumes from the newest retained opening at or before the window"
        (expectedResume snapshot matched query commodity granularity cold.openings)
        warm.resumedFrom
    traverse_
        (assertReportsAgree snapshot granularity commodity depth full)
        [("empty seed", cold.report), ("own openings", warm.report), ("earlier openings", partial.report)]

{- | The stored openings with the newest month dropped, so that a resume from
them has a genuine tail to replay rather than landing on the day it wants.
-}
earlierOpenings :: QueryKey -> Text -> PrefixChain -> OpeningBalances -> OpeningBalances
earlierOpenings query commodity chain stored =
    case lastMaybe (openingBalanceDays query commodity stored) of
        Nothing -> stored
        Just newest -> truncateOpeningBalances chain (DivergesFrom newest) stored

-- | The day a resume should have started from: the newest retained opening at or before the window.
expectedResume ::
    Snapshot ->
    [Hledger.Transaction] ->
    QueryKey ->
    Text ->
    Granularity ->
    OpeningBalances ->
    Maybe Day
expectedResume snapshot matched query commodity granularity stored = do
    firstColumn <- case periodicBalanceColumns snapshot matched granularity assetLiabilityAccountTypes of
        column : _ -> Just column
        [] -> Nothing
    let opening = addDays (-1) firstColumn.start
    lastMaybe [day | day <- openingBalanceDays query commodity stored, day <= opening]

assertReportsAgree ::
    Snapshot ->
    Granularity ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport ->
    (String, HledgerMultiBalance.MultiBalanceReport) ->
    IO ()
assertReportsAgree snapshot granularity commodity depth full (label, resumed) = do
    assertEqual (label <> ": columns") (periodKeys full) (periodKeys resumed)
    traverse_
        assertSideAgrees
        [ ("assets", [Hledger.Asset, Hledger.Cash])
        , ("liabilities", [Hledger.Liability])
        ]
  where
    periods = reportPeriodsFrom granularity full
    periodKeys report = [(period.key, period.start, period.end) | period <- reportPeriodsFrom granularity report]
    sideOf types report = reportSide snapshot.journal types report
    assertSideAgrees (side, types) = do
        let expected = sideOf types full
            actual = sideOf types resumed
        assertEqual
            (label <> ": " <> side <> " column totals")
            (columnValues (reportTotals commodity expected))
            (columnValues (reportTotals commodity actual))
        assertEqual
            (label <> ": " <> side <> " missing commodities")
            (reportMissing commodity expected)
            (reportMissing commodity actual)
        traverse_ (assertBreakdownAgrees side expected actual) periods
    assertBreakdownAgrees side expected actual period =
        assertEqual
            (label <> ": " <> side <> " breakdown for " <> Text.unpack period.key)
            (breakdownValues (breakdownOf expected period))
            (breakdownValues (breakdownOf actual period))
    breakdownOf side period = breakdownItemsFor commodity depth (Just period) periods side

columnValues :: [Conversion Decimal] -> [(Maybe Decimal, Set Text)]
columnValues = fmap (\column -> (column.value, column.missing))

breakdownValues :: (Text, [BreakdownItem]) -> (Text, [(Text, Decimal)])
breakdownValues (period, items) = (period, [(item.account, item.quantity) | item <- items])

-- * The same equivalence through the figures, which is what carries the KPIs

resumedFiguresTests :: TestTree
resumedFiguresTests =
    testGroup
        "resumed figures agree with computeNetWorth"
        [ testCase caseLabel $
            source caseLabel (\snapshot -> assertResumedFiguresMatch snapshot granularity commodity)
        | (label, source) <- sweepSources
        , granularity <- [Monthly, Yearly]
        , commodity <- ["INR", "USD"]
        , let caseLabel = label <> " figures " <> show granularity <> " " <> Text.unpack commodity
        ]

assertResumedFiguresMatch :: Snapshot -> Granularity -> Text -> IO ()
assertResumedFiguresMatch snapshot granularity commodity = do
    let matched = snapshot.transactions
        query = queryKeyFor snapshot
        chain = chainOf snapshot.journal
        expected = computeNetWorth snapshot matched granularity commodity Nothing Nothing 2 2
        resumed openings =
            computeNetWorthResumed snapshot matched query chain openings granularity commodity Nothing Nothing 2 2
        (cold, warmed) = resumed emptyOpeningBalances
        (warm, _) = resumed warmed
    assertFiguresEqual "empty seed" expected cold
    assertFiguresEqual "own openings" expected warm

assertFiguresEqual :: String -> NetWorthFigures -> NetWorthFigures -> IO ()
assertFiguresEqual label expected actual = do
    assertEqual (label <> " timeline") (timelineValues expected) (timelineValues actual)
    assertEqual (label <> " performance") (performanceValues expected) (performanceValues actual)
    assertEqual (label <> " incompletePeriods") expected.incompletePeriods actual.incompletePeriods
    assertEqual (label <> " selectedKey") expected.selectedKey actual.selectedKey
    assertEqual (label <> " currentAssets") expected.currentAssets actual.currentAssets
    assertEqual (label <> " currentLiabilities") expected.currentLiabilities actual.currentLiabilities
    assertEqual (label <> " currentNetWorth") expected.currentNetWorth actual.currentNetWorth
    assertEqual
        (label <> " assets breakdown")
        (expected.assetsBreakdownPeriod, breakdownItems expected.assetsBreakdownItems)
        (actual.assetsBreakdownPeriod, breakdownItems actual.assetsBreakdownItems)
    assertEqual
        (label <> " liabilities breakdown")
        (expected.liabilitiesBreakdownPeriod, breakdownItems expected.liabilitiesBreakdownItems)
        (actual.liabilitiesBreakdownPeriod, breakdownItems actual.liabilitiesBreakdownItems)
    assertEqual (label <> " missing") expected.missing actual.missing

timelineValues :: NetWorthFigures -> [(Text, Day, Decimal, Decimal, Decimal, Set Text)]
timelineValues figures =
    [ (point.period, point.startDate, point.positive, point.negative, point.net, point.missing)
    | point <- figures.timeline
    ]

performanceValues :: NetWorthFigures -> [(Text, Day, Decimal, Decimal, Decimal)]
performanceValues figures =
    [ (point.period, point.startDate, point.netInvestment, point.netWorth, point.gainLoss)
    | point <- figures.performance
    ]

breakdownItems :: [BreakdownItem] -> [(Text, Decimal)]
breakdownItems items = [(item.account, item.quantity) | item <- items]

-- * The month-end boundary

{- | The trap a resumed accumulation falls into most easily.

A column ends on its last day inclusive, so a transaction dated exactly on the
opening month's end belongs to the opening balance and must not be replayed by
the deltas that extend it. The journal puts an unmistakable 1000 INR deposit on
that exact day, and the first resumed column is asserted absolutely: a replay
would read 2330 and a skip 330. The correct answer is 1330: three months of 100
INR deposits, less three purchases of 50 INR, plus three units of XYZ valued at
60 INR at March's end, plus the 1000 INR deposit dated on February's month end.
Comparing only against the unchecked report would not distinguish a compensating
error made by both.
-}
monthEndBoundaryCountedOnce :: IO ()
monthEndBoundaryCountedOnce =
    withTemporaryJournal "boundary" (monthlyDeposits 26 [(fromGregorian 2022 2 28, "1000.00")]) $ \snapshot -> do
        let matched = snapshot.transactions
            query = queryKeyFor snapshot
            chain = chainOf snapshot.journal
            resume = assetLiabilityResumed snapshot matched query chain
            cold = resume emptyOpeningBalances Monthly "INR"
            warm = resume cold.openings Monthly "INR"
            columns = periodicBalanceColumns snapshot matched Monthly assetLiabilityAccountTypes
        assertEqual
            "the window must open on the month end carrying the extra deposit"
            [fromGregorian 2022 2 28]
            [addDays (-1) column.start | column <- take 1 columns]
        assertEqual
            "the resume must actually cross that month end"
            (Just (fromGregorian 2022 2 28))
            warm.resumedFrom
        assertEqual
            "the first resumed column counts the month-end deposit exactly once"
            (Just 1330)
            (firstAssetColumn snapshot "INR" warm.report)
        assertEqual
            "which is what the unchecked report reads too"
            (firstAssetColumn snapshot "INR" (assetLiabilityPeriodicReport snapshot matched Monthly "INR"))
            (firstAssetColumn snapshot "INR" warm.report)

firstAssetColumn :: Snapshot -> Text -> HledgerMultiBalance.MultiBalanceReport -> Maybe Decimal
firstAssetColumn snapshot commodity report =
    case reportTotals commodity (reportSide snapshot.journal [Hledger.Asset, Hledger.Cash] report) of
        column : _ -> column.value
        [] -> Nothing

-- * Rejecting openings that must not be reused

{- | The failure mode with no other symptom.

Openings are stored from one revision of a journal, then a transaction is
inserted into a month the openings cover. Every stored hash is stale, so trusting
any of them would omit the insertion from every later column and report a
plausible number.
-}
staleChainHashIsIgnored :: IO ()
staleChainHashIsIgnored =
    withJournalPath "stale" $ \path -> do
        before <- snapshotAt path (monthlyDeposits 26 [])
        after <- snapshotAt path (monthlyDeposits 26 [(fromGregorian 2022 1 20, "5000.00")])
        let beforeChain = chainOf before.journal
            afterChain = chainOf after.journal
            query = queryKeyFor before
            stored =
                ( assetLiabilityResumed
                    before
                    before.transactions
                    query
                    beforeChain
                    emptyOpeningBalances
                    Monthly
                    "INR"
                ).openings
            full = assetLiabilityPeriodicReport after after.transactions Monthly "INR"
            resumed =
                assetLiabilityResumed after after.transactions (queryKeyFor after) afterChain stored Monthly "INR"
        assertBool "the earlier revision must have produced openings" (openingBalanceCount stored > 0)
        assertEqual
            "the insertion must diverge at its own month"
            (DivergesFrom (fromGregorian 2022 1 31))
            (diffChains beforeChain afterChain)
        assertEqual
            "every stale opening must be truncated away"
            0
            (openingBalanceCount (truncateOpeningBalances afterChain Identical stored))
        assertEqual "no stale opening may be reused" Nothing resumed.resumedFrom
        assertBool
            "the inserted transaction must move the numbers, or this proves nothing"
            (firstAssetColumn after "INR" full /= firstAssetColumn before "INR" (assetLiabilityPeriodicReport before before.transactions Monthly "INR"))
        assertReportsAgree after Monthly "INR" 2 full ("stale seed", resumed.report)

{- | A commodity declaration changes no transaction, so every month hash is
unchanged and only the global fingerprint moves. Keying on the month hash alone
would therefore reuse every opening across exactly the edit that can restate
arbitrarily old columns.
-}
globalChangeInvalidatesOpenings :: IO ()
globalChangeInvalidatesOpenings =
    withJournalPath "global" $ \path -> do
        before <- snapshotAt path (monthlyDeposits 26 [])
        after <- snapshotAt path ("commodity 1000.000 INR\n\n" <> monthlyDeposits 26 [])
        let beforeChain = chainOf before.journal
            afterChain = chainOf after.journal
            query = queryKeyFor before
            cold =
                assetLiabilityResumed before before.transactions query beforeChain emptyOpeningBalances Monthly "INR"
            month = fromGregorian 2022 1 31
        assertEqual
            "the declaration must change only the global fingerprint"
            GlobalChanged
            (diffChains beforeChain afterChain)
        assertEqual
            "month hashes alone cannot see a global change"
            (chainHashAt beforeChain month)
            (chainHashAt afterChain month)
        assertBool
            "the key an opening is stored under must see it"
            (openingHashAt beforeChain month /= openingHashAt afterChain month)
        assertBool "the earlier revision must have produced openings" (openingBalanceCount cold.openings > 0)
        assertEqual
            "a global change must invalidate every opening"
            0
            (openingBalanceCount (truncateOpeningBalances afterChain Identical cold.openings))
        assertEqual
            "GlobalChanged must invalidate every opening against its own chain"
            0
            (openingBalanceCount (truncateOpeningBalances beforeChain GlobalChanged cold.openings))

{- | The reference date a query text was resolved against is part of the key.

A query is resolved against the snapshot's latest journal date, so a relative term
denotes a different span once that date moves — and it can restate arbitrarily old
columns, which a month chain that diverges only at the appended month would not
catch. Openings stored under one reference date must therefore not be reused under
another, even though the query text and every month hash are identical.
-}
queryReferenceDateIsPartOfTheKey :: IO ()
queryReferenceDateIsPartOfTheKey =
    withTemporaryJournal "reference" (monthlyDeposits 26 []) $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            resolved = queryKeyFor snapshot
            relative = QueryKey{text = "date:thismonth", reference = resolved.reference}
            laterReference = QueryKey{text = "date:thismonth", reference = addDays 40 resolved.reference}
            resume query openings = assetLiabilityResumed snapshot matched query chain openings Monthly "INR"
            stored = (resume relative emptyOpeningBalances).openings
        assertBool "the sweep must have stored openings" (openingBalanceCount stored > 0)
        assertBool
            "the same query text and reference date reuses them"
            ((resume relative stored).resumedFrom /= Nothing)
        assertEqual
            "a later reference date reuses none of them"
            Nothing
            (resume laterReference stored).resumedFrom
        assertEqual
            "and neither does a different query text"
            Nothing
            (resume resolved stored).resumedFrom

{- | 'DivergesFrom' keeps the months strictly before the divergence and drops the
divergent month itself, because that month's own content is what changed.
-}
truncationDropsFromDivergence :: IO ()
truncationDropsFromDivergence =
    withTemporaryJournal "truncate" (monthlyDeposits 26 []) $ \snapshot -> do
        let query = queryKeyFor snapshot
            chain = chainOf snapshot.journal
            cold =
                assetLiabilityResumed snapshot snapshot.transactions query chain emptyOpeningBalances Monthly "INR"
            days = openingBalanceDays query "INR" cold.openings
        assertBool "the sweep must retain several months" (length days > 1)
        traverse_
            (assertTruncationKeeps chain query cold.openings days)
            days
        assertEqual
            "an identical chain keeps every opening"
            days
            (openingBalanceDays query "INR" (truncateOpeningBalances chain Identical cold.openings))

assertTruncationKeeps :: PrefixChain -> QueryKey -> OpeningBalances -> [Day] -> Day -> IO ()
assertTruncationKeeps chain query stored days divergence =
    assertEqual
        ("divergence at " <> show divergence <> " keeps the earlier months")
        (filter (< divergence) days)
        (openingBalanceDays query "INR" (truncateOpeningBalances chain (DivergesFrom divergence) stored))

-- * The rows a report retains

{- | A consolidated report's per-side totals are the sum of the rows the report
retained, so the resumed report has to retain the same rows.

hledger retains more than it might appear to. Its zero-row test asks both that
the balance look zero in every column and that the account's posting count be
zero in every column, and with @Historical@ accumulation those counts are
cumulative, so an account that closed to zero before the window opened still has
a row. This journal has one, alongside an account left holding a residual in a
commodity the journal never prices, which must still surface as a missing
commodity.
-}
retainedRowsAgree :: IO ()
retainedRowsAgree =
    withTemporaryJournal "elision" closedAccountJournal $ \snapshot -> do
        let matched = snapshot.transactions
            query = queryKeyFor snapshot
            chain = chainOf snapshot.journal
            full = assetLiabilityPeriodicReport snapshot matched Monthly "INR"
            resume = assetLiabilityResumed snapshot matched query chain
            cold = resume emptyOpeningBalances Monthly "INR"
            warm = resume cold.openings Monthly "INR"
            assetSide report = reportSide snapshot.journal [Hledger.Asset, Hledger.Cash] report
            assetRows report = rowNames (assetSide report)
        assertBool
            "the fixture must produce a zero row hledger retains, or this proves nothing"
            ("assets:old:closed" `elem` assetRows full)
        assertBool
            "and a row holding an unconvertible residual"
            ("assets:old:bullion" `elem` assetRows full)
        assertEqual
            "the residual hledger keeps is reported as a missing commodity"
            (Set.singleton "XAU")
            (reportMissing "INR" (assetSide full))
        assertEqual
            "the resumed report retains exactly the same rows"
            (List.sort (assetRows full))
            (List.sort (assetRows warm.report))
        assertReportsAgree snapshot Monthly "INR" 2 full ("empty seed", cold.report)
        assertReportsAgree snapshot Monthly "INR" 2 full ("own openings", warm.report)

rowNames :: ReportSide -> [Text]
rowNames side = HledgerReportTypes.prrFullName <$> side.rows

{- | A commodity the journal never prices leaves the strict KPI withheld, and the
resumed columns must withhold it for the same reason rather than quietly
converting a partial sum.
-}
withheldKpiSurvivesResume :: IO ()
withheldKpiSurvivesResume =
    withFixture "test/fixtures/investment-never-priced.journal" $ \snapshot -> do
        let matched = snapshot.transactions
            query = queryKeyFor snapshot
            chain = chainOf snapshot.journal
            expected = computeNetWorth snapshot matched Monthly "INR" Nothing Nothing 2 2
            (actual, _) =
                computeNetWorthResumed snapshot matched query chain emptyOpeningBalances Monthly "INR" Nothing Nothing 2 2
        assertEqual "the fixture must withhold the assets KPI" Nothing expected.currentAssets
        assertBool "and must report a missing commodity" (not (Set.null expected.missing))
        assertFiguresEqual "never priced" expected actual

-- * Retention

{- | The policy is the investment slice's — the dense tail plus one month in
twelve — with the window's own opening month added, because that is the one the
next request resumes from and the twelve-month grid retains it only by luck.
-}
retentionKeepsWindowOpening :: IO ()
retentionKeepsWindowOpening = do
    let months = distinctMonths 40
        opening = months !! 15
        retained = retainedOpeningDays opening months
    assertBool "the window opening is retained" (opening `elem` retained)
    assertEqual
        "every twelfth month from the oldest is retained"
        [day | (index, day) <- zip [0 :: Int ..] months, index `mod` 12 == 0, index < 16]
        [day | (index, day) <- zip [0 :: Int ..] months, index `mod` 12 == 0, index < 16, day `elem` retained]
    assertEqual
        "the whole dense tail is retained, without gaps"
        (drop 16 months)
        (dropWhile (/= months !! 16) retained)
    assertEqual "retained months stay in order" (filter (`elem` retained) months) retained
    assertBool
        "a month that is neither the opening, on the grid, nor in the dense tail is dropped"
        ((months !! 5) `notElem` retained)

retentionKeepsShortHistory :: IO ()
retentionKeepsShortHistory = do
    let day = fromGregorian 2024 1 31
    assertEqual "an empty candidate list retains nothing" [] (retainedOpeningDays day [])
    assertEqual "a single candidate is retained" [day] (retainedOpeningDays day [day])

distinctMonths :: Int -> [Day]
distinctMonths count = take count [fromGregorian 2020 1 1 ..]

-- * Journals

{- | @count@ consecutive months from January 2022, each with a deposit and a
credit-card purchase dated on the month end, plus any extra deposits requested on
particular days.

Long enough that the monthly window of 24 columns opens after the journal starts,
which is the only shape in which a resume has an earlier month to open from. The
month-end purchases mean every month is a boundary case, and the liability side
is populated so both sides of the consolidated report are exercised.
-}
monthlyDeposits :: Int -> [(Day, Text)] -> Text
monthlyDeposits count extras =
    Text.unlines (concatMap monthEntries [0 .. count - 1] <> concatMap extraEntry extras)
  where
    monthEntries index =
        let mid = monthDay index 15
            end = monthDay index 31
         in [ dayText mid <> " deposit"
            , "    assets:bank:checking      100.00 INR"
            , "    income:salary"
            , ""
            , dayText mid <> " buy"
            , "    assets:investments:equity:xyz    1 XYZ @ 50.00 INR"
            , "    assets:bank:checking"
            , ""
            , dayText end <> " card purchase"
            , "    expenses:food             25.00 INR"
            , "    liabilities:card"
            , ""
            , "P " <> dayText end <> " XYZ 60.00 INR"
            , ""
            ]
    extraEntry (day, amount) =
        [ dayText day <> " extra deposit"
        , "    assets:bank:checking      " <> amount <> " INR"
        , "    income:bonus"
        , ""
        ]
    monthDay index dayOfMonth =
        let (year, month) = (2022 + (index `div` 12), 1 + (index `mod` 12))
         in fromGregorian (fromIntegral year) month dayOfMonth

dayText :: Day -> Text
dayText = Text.pack . show

{- | One account opened and closed before the report window opens, and one left
holding a residual in a commodity the journal never prices.
-}
closedAccountJournal :: Text
closedAccountJournal =
    Text.unlines
        [ "2022-01-10 opened"
        , "    assets:old:closed       100.00 INR"
        , "    equity:opening"
        , ""
        , "2022-01-11 closed again"
        , "    assets:old:closed      -100.00 INR"
        , "    equity:opening"
        , ""
        , "2022-01-12 residual in an unpriced commodity"
        , "    assets:old:bullion       0.0004 XAU"
        , "    equity:opening"
        , ""
        ]
        <> monthlyDeposits 26 []

-- * Fixtures and shared helpers

{- | The net-worth fixtures, plus a journal long enough for the monthly window to
open after the journal starts.

The file fixtures cover missing prices, foreign cash, dust residuals, gapped
price histories and a price dated after the final transaction. Every one of them
is short, so none of them on its own would ever exercise a resume from an earlier
month; the synthetic journal is what does.
-}
sweepSources :: [(String, String -> (Snapshot -> IO ()) -> IO ())]
sweepSources =
    ("26 monthly deposits", \label -> withTemporaryJournal label (monthlyDeposits 26 []))
        : [(Text.unpack path, const (withFixture path)) | path <- fixtures]

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

queryKeyFor :: Snapshot -> QueryKey
queryKeyFor snapshot =
    QueryKey{text = "", reference = fromMaybe epoch snapshot.latestJournalDate}

chainOf :: Hledger.Journal -> PrefixChain
chainOf journal =
    prefixChain (globalFingerprint journal configuration) journal.jtxns journal.jpricedirectives

withFixture :: Text -> (Snapshot -> IO ()) -> IO ()
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

withTemporaryJournal :: String -> Text -> (Snapshot -> IO ()) -> IO ()
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
        let dir = base </> ("hedger-net-worth-checkpoint-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir
