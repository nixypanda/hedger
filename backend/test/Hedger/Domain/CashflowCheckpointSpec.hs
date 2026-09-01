{- | Checkpointed cashflow columns measured against the unchecked path.

The decisive test is 'resumedFigureTests': for every fixture, both display
commodities, both granularities and three depths, 'computeCashflowResumed' must
equal 'computeCashflow' field by field — timeline, both breakdowns, totals and
missing sets — cold, warm, and seeded from part of history.

The remaining cases pin the properties that equality rests on, each of them
somewhere a fixture sweep alone would pass vacuously: that a calendar year is
exactly the sum of its twelve months, that collapsing unclipped rows to a depth
equals a report clipped to that depth, that a month boundary lands in one column
only, that a stale hash recomputes just its own month while the rest is genuinely
reused, and that a change visible only in the global fingerprint discards
everything.
-}
module Hedger.Domain.CashflowCheckpointSpec (cashflowCheckpointSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time (Day, addGregorianMonthsClip, fromGregorian, toGregorian)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Cashflow (
    BreakdownItem (..),
    CashflowFigures (..),
    TimelinePoint (..),
    computeCashflow,
    computeCashflowResumed,
 )
import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    PrefixChain (..),
    diffChains,
    globalFingerprint,
    prefixChain,
 )
import Hedger.Domain.Internal.Cashflow (
    cashflowReportPeriods,
    expensePeriodicReport,
    incomeExpensePeriodicReport,
    incomeGainPeriodicReport,
 )
import Hedger.Domain.Internal.CashflowCheckpoint (
    CashflowColumn (..),
    CashflowColumns,
    ColumnKey (..),
    ColumnSide (..),
    cashflowColumnsResumed,
    chunkMonths,
    collapseToDepth,
    columnCount,
    columnKeyAt,
    columnMonths,
    emptyColumns,
    insertColumn,
    maxChunkMonths,
    monthEndsIn,
    sumColumns,
    truncateColumns,
 )
import Hedger.Domain.Internal.PeriodReport (
    ReportSide,
    breakdownItemsFor,
    reportPeriodsFrom,
    reportTotals,
    wholeReport,
 )
import Hedger.Domain.Period (
    Granularity (Monthly, Yearly),
    ReportPeriod (..),
    atIndex,
    lastMaybe,
    monthPeriod,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed)
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.TestHelpers (configuration)

cashflowCheckpointSpec :: TestTree
cashflowCheckpointSpec =
    testGroup
        "cashflow checkpoints"
        [ resumedFigureTests
        , depthCollapseTests
        , testCase
            "a calendar year equals the sum of its twelve monthly columns"
            yearlyEqualsSumOfMonths
        , testCase
            "a span longer than one report window is still assembled in full"
            longSpanIsAssembledInFull
        , testCase
            "chunks never span more than the report window retains"
            chunkMonthsRespectsTheWindow
        , testCase
            "a report period decomposes into the months it is a sum of"
            monthEndsInCoversWholeMonths
        , testCase
            "a transaction dated on a month end lands in exactly one column"
            monthEndBoundaryLandsInOneColumn
        , testCase
            "a stale hash recomputes only its own month and reuses the rest"
            staleHashRecomputesOnlyThatMonth
        , testCase
            "a global fingerprint change invalidates every cached column"
            globalChangeInvalidatesColumns
        , testCase
            "truncation drops exactly the columns at or after the divergence month"
            truncationDropsFromDivergence
        , testCase
            "an unpriced commodity reproduces the same missing set"
            missingPriceReproducesMissingSet
        , testCase
            "a commodity cancelling within a year is missing monthly but not yearly"
            cancellingCommodityIsNotMissingForTheYear
        ]

-- * The decisive equivalence: resumed figures against the unchecked calculation

{- | 'computeCashflow' is the oracle. Three seedings are checked per case: an
empty map, which must degrade to the full path; the map the cold pass produced,
which must answer every column from the cache; and a map holding only the earlier
half of the request's months, which forces a mixture of reuse and recomputation
inside one assembly.
-}
resumedFigureTests :: TestTree
resumedFigureTests =
    testGroup
        "computeCashflowResumed agrees with computeCashflow"
        [ testCase (caseLabel path granularity commodity depth) $
            assertResumedFiguresMatch path granularity commodity depth
        | path <- fixtures
        , granularity <- [Monthly, Yearly]
        , commodity <- ["INR", "USD"]
        , depth <- [1, 2, 3]
        ]

assertResumedFiguresMatch :: Text -> Granularity -> Text -> Int -> IO ()
assertResumedFiguresMatch path granularity commodity depth =
    withFixture path $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            resumed cached = resume snapshot matched chain cached granularity commodity depth
            expected = oracle snapshot matched granularity commodity depth
            (cold, populated) = resumed emptyColumns
            (warm, _) = resumed populated
            (partial, _) = resumed (earlyHalf snapshot matched chain granularity commodity)
        assertFiguresEqual "empty seed" expected cold
        assertFiguresEqual "seeded from its own columns" expected warm
        assertFiguresEqual "seeded from earlier history only" expected partial

{- | The whole-journal request the sweeps use: no query, so every transaction is
matched, the latest period on both sides, and the same depth on both sides.
-}
resume ::
    Snapshot ->
    [Hledger.Transaction] ->
    PrefixChain ->
    CashflowColumns ->
    Granularity ->
    Text ->
    Int ->
    (CashflowFigures, CashflowColumns)
resume snapshot matched chain cached granularity commodity depth =
    computeCashflowResumed
        snapshot
        matched
        chain
        cached
        wholeJournalQuery
        granularity
        commodity
        Nothing
        Nothing
        depth
        depth

oracle :: Snapshot -> [Hledger.Transaction] -> Granularity -> Text -> Int -> CashflowFigures
oracle snapshot matched granularity commodity depth =
    computeCashflow snapshot matched granularity commodity Nothing Nothing depth depth

{- | Columns for the earlier half of the request's months only, so a resume from
this map has to compute a genuine remainder rather than land on every column.
-}
earlyHalf ::
    Snapshot -> [Hledger.Transaction] -> PrefixChain -> Granularity -> Text -> CashflowColumns
earlyHalf snapshot matched chain granularity commodity =
    snd (columnsFor snapshot matched chain emptyColumns commodity firstHalf)
  where
    months = requestMonths snapshot matched granularity
    firstHalf = take (max 1 (length months `div` 2)) months

-- | Every calendar month the request's report periods are made of.
requestMonths :: Snapshot -> [Hledger.Transaction] -> Granularity -> [Day]
requestMonths snapshot matched granularity =
    concat
        [ monthEndsIn period.start period.end
        | period <- cashflowReportPeriods snapshot matched granularity
        ]

columnsFor ::
    Snapshot ->
    [Hledger.Transaction] ->
    PrefixChain ->
    CashflowColumns ->
    Text ->
    [Day] ->
    ([CashflowColumn], CashflowColumns)
columnsFor snapshot matched chain cached commodity =
    cashflowColumnsResumed snapshot matched chain cached wholeJournalQuery commodity

assertFiguresEqual :: String -> CashflowFigures -> CashflowFigures -> IO ()
assertFiguresEqual label expected actual = do
    assertEqual (label <> " available") expected.available actual.available
    assertEqual (label <> " missing") expected.missing actual.missing
    assertEqual (label <> " totalCount") expected.totalCount actual.totalCount
    assertEqual (label <> " filteredCount") expected.filteredCount actual.filteredCount
    assertEqual
        (label <> " timeline")
        (timelineValues expected.timeline)
        (timelineValues actual.timeline)
    assertEqual
        (label <> " incomeBreakdownPeriod")
        expected.incomeBreakdownPeriod
        actual.incomeBreakdownPeriod
    assertEqual
        (label <> " incomeBreakdownItems")
        (breakdownValues expected.incomeBreakdownItems)
        (breakdownValues actual.incomeBreakdownItems)
    assertEqual
        (label <> " expenseBreakdownPeriod")
        expected.expenseBreakdownPeriod
        actual.expenseBreakdownPeriod
    assertEqual
        (label <> " expenseBreakdownItems")
        (breakdownValues expected.expenseBreakdownItems)
        (breakdownValues actual.expenseBreakdownItems)

timelineValues :: [TimelinePoint] -> [(Text, Day, Decimal, Decimal, Decimal, Set Text)]
timelineValues points =
    [ (point.period, point.startDate, point.positive, point.negative, point.net, point.missing)
    | point <- points
    ]

breakdownValues :: [BreakdownItem] -> [(Text, Decimal)]
breakdownValues items = [(item.account, item.quantity) | item <- items]

-- * Yearly columns are the sum of monthly columns

{- | The fact the whole slice rests on, asserted directly rather than inferred
from the fixture sweep.

@PerPeriod@ accumulation carries @AtThen@ valuation, so a posting is valued at its
own date whatever column it lands in and a year is the sum of its months. The
comparison is against reports built at yearly granularity: the column total, its
own missing set, and the rows collapsed to the deepest level 'atDepth'
distinguishes must all agree with the summed monthly columns.

The assertions on the monthly columns themselves are what stop this passing
vacuously: a year whose entries all sat in one month would prove nothing about
addition.
-}
yearlyEqualsSumOfMonths :: IO ()
yearlyEqualsSumOfMonths =
    withFixture "test/fixtures/shared.journal" $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            commodity = "INR"
            yearlyPeriods =
                reportPeriodsFrom Yearly (incomeExpensePeriodicReport snapshot matched Yearly commodity)
        -- The fixture's first year is the one whose entries are spread across it;
        -- its second year holds a single month and would prove nothing about adding.
        period <- case yearlyPeriods of
            first : _ -> pure first
            [] -> assertFailure "the fixture has no yearly period"
        let months = monthEndsIn period.start period.end
            (monthColumns, _) = columnsFor snapshot matched chain emptyColumns commodity months
            summed = sumColumns commodity monthColumns
            reference report = wholeReport (report snapshot matched Yearly commodity 4)
        assertEqual "a calendar year is twelve months" 12 (length months)
        assertBool
            "more than one month must carry expenses, or nothing is being added"
            (length (filter (not . negligibleSide commodity . (.expense)) monthColumns) > 1)
        assertSideMatchesColumn
            commodity
            "income"
            yearlyPeriods
            period
            (reference incomeGainPeriodicReport)
            summed.income
        assertSideMatchesColumn
            commodity
            "expense"
            yearlyPeriods
            period
            (reference expensePeriodicReport)
            summed.expense

{- | A cached side against one column of a separately built report: the total and
its missing commodities, then the rows collapsed to depth 4.
-}
assertSideMatchesColumn ::
    Text -> String -> [ReportPeriod] -> ReportPeriod -> ReportSide -> ColumnSide -> IO ()
assertSideMatchesColumn commodity label periods period reference side = do
    assertEqual
        (label <> " column total")
        (conversionPair <$> atIndex (indexOf period periods) (reportTotals commodity reference))
        (Just (conversionPair (conversionFromMixed commodity side.total)))
    assertEqual
        (label <> " rows at depth 4")
        (breakdownValues (snd (breakdownItemsFor commodity 4 (Just period) periods reference)))
        (breakdownValues (collapseToDepth commodity 4 side))

conversionPair :: Conversion Decimal -> (Maybe Decimal, Set Text)
conversionPair conversion = (conversion.value, conversion.missing)

indexOf :: ReportPeriod -> [ReportPeriod] -> Int
indexOf period periods = fromMaybe 0 (List.findIndex ((== period.key) . (.key)) periods)

negligibleSide :: Text -> ColumnSide -> Bool
negligibleSide commodity side = all negligible (Map.elems side.rows)
  where
    negligible amount = maybe True ((< 0.000001) . abs) (conversionFromMixed commodity amount).value

-- * Depth collapse

{- | Collapsing unclipped cached rows to depth @N@ must equal a report hledger
clipped to depth @N@ itself. This is what licenses caching one column per month
rather than one per (month, depth).
-}
depthCollapseTests :: TestTree
depthCollapseTests =
    testGroup
        "collapsing unclipped columns agrees with a depth-clipped report"
        [ testCase (caseLabel path granularity commodity depth) $
            assertDepthCollapseMatches path granularity commodity depth
        | path <- fixtures
        , granularity <- [Monthly, Yearly]
        , commodity <- ["INR", "USD"]
        , depth <- [1, 2, 3]
        ]

assertDepthCollapseMatches :: Text -> Granularity -> Text -> Int -> IO ()
assertDepthCollapseMatches path granularity commodity depth =
    withFixture path $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            periods = cashflowReportPeriods snapshot matched granularity
        case lastMaybe periods of
            Nothing -> assertFailure (Text.unpack path <> " produced no report period")
            Just period ->
                let (columns, _) =
                        columnsFor
                            snapshot
                            matched
                            chain
                            emptyColumns
                            commodity
                            (monthEndsIn period.start period.end)
                    column = sumColumns commodity columns
                    clipped report = wholeReport (report snapshot matched granularity commodity depth)
                    expected reference =
                        breakdownValues
                            (snd (breakdownItemsFor commodity depth (Just period) periods reference))
                 in do
                        assertEqual
                            "income"
                            (expected (clipped incomeGainPeriodicReport))
                            (breakdownValues (collapseToDepth commodity depth column.income))
                        assertEqual
                            "expense"
                            (expected (clipped expensePeriodicReport))
                            (breakdownValues (collapseToDepth commodity depth column.expense))

-- * Long spans

{- | A request whose months outrun the 24 monthly periods a report window
retains. Assembling it takes more than one report, and a chunking rule that let a
chunk exceed the window would silently lose that chunk's earliest months.
-}
longSpanIsAssembledInFull :: IO ()
longSpanIsAssembledInFull =
    withTemporaryJournal "long-span" longSpanJournal $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            months = requestMonths snapshot matched Yearly
            expected = oracle snapshot matched Yearly "INR" 3
            (cold, populated) = resume snapshot matched chain emptyColumns Yearly "INR" 3
            (warm, _) = resume snapshot matched chain populated Yearly "INR" 3
        assertBool
            ("the request must outrun one report window, saw " <> show (length months) <> " months")
            (length months > maxChunkMonths)
        assertFiguresEqual "long span, empty seed" expected cold
        assertFiguresEqual "long span, warm" expected warm

{- | Six calendar years of expenses, so a yearly request needs 72 monthly columns
against a report window that retains 24.
-}
longSpanJournal :: Text
longSpanJournal =
    Text.unlines (concat [yearlyEntry year | year <- [2019 .. 2024 :: Integer]])
  where
    yearlyEntry year =
        [ Text.pack (show year) <> "-06-15 mid-year spend"
        , "    expenses:food:groceries    " <> Text.pack (show year) <> ".00 INR"
        , "    assets:cash:checking"
        , ""
        ]

chunkMonthsRespectsTheWindow :: IO ()
chunkMonthsRespectsTheWindow = do
    let months = [monthEndAfter step | step <- [0 .. 40]]
        chunks = chunkMonths months
    assertEqual "chunking loses no month, in order" months (concat chunks)
    assertBool "chunking produces no empty chunk" (all (not . null) chunks)
    assertBool "every chunk fits inside one report window" (all fitsWindow chunks)
    assertEqual
        "41 consecutive months split at the window boundary"
        [maxChunkMonths, 41 - maxChunkMonths]
        (length <$> chunks)
  where
    monthEndAfter step = (monthPeriod (addGregorianMonthsClip step (fromGregorian 2019 1 1))).end
    fitsWindow chunk = case chunk of
        [] -> True
        first : rest -> all (\day -> monthOrdinal day - monthOrdinal first < fromIntegral maxChunkMonths) rest

monthOrdinal :: Day -> Integer
monthOrdinal day = let (year, month, _) = toGregorian day in year * 12 + fromIntegral month

monthEndsInCoversWholeMonths :: IO ()
monthEndsInCoversWholeMonths = do
    assertEqual
        "a month yields its own end"
        [fromGregorian 2023 2 28]
        (monthEndsIn (fromGregorian 2023 2 1) (fromGregorian 2023 2 28))
    assertEqual
        "a calendar year yields twelve ends"
        12
        (length (monthEndsIn (fromGregorian 2024 1 1) (fromGregorian 2024 12 31)))
    assertEqual
        "a month only partly covered still contributes"
        [fromGregorian 2023 1 31, fromGregorian 2023 2 28]
        (monthEndsIn (fromGregorian 2023 1 15) (fromGregorian 2023 2 10))
    assertEqual
        "an inverted range yields nothing"
        []
        (monthEndsIn (fromGregorian 2023 2 1) (fromGregorian 2023 1 1))

-- * The month-end boundary

{- | The trap a per-month cache falls into most easily. A transaction dated
exactly on a month end belongs to that month and to no other, so both columns are
asserted absolutely: a double count would inflate February and a skip would empty
January, and neither could pass.
-}
monthEndBoundaryLandsInOneColumn :: IO ()
monthEndBoundaryLandsInOneColumn =
    withTemporaryJournal "boundary" boundaryJournal $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            months = [fromGregorian 2023 1 31, fromGregorian 2023 2 28]
            (columns, _) = columnsFor snapshot matched chain emptyColumns "INR" months
        assertEqual "the chain must have an entry for both months" months (fst <$> chain.months)
        assertEqual
            "each month's expense total stands alone"
            [Just 100, Just 23]
            [(conversionFromMixed "INR" column.expense.total).value | column <- columns]
        assertEqual
            "and so does the account row every transaction shares"
            [Just 100, Just 23]
            [rowValue "INR" "expenses:food" column.expense | column <- columns]

rowValue :: Text -> Hledger.AccountName -> ColumnSide -> Maybe Decimal
rowValue commodity account side =
    (conversionFromMixed commodity (Map.findWithDefault HledgerAmount.nullmixedamt account side.rows)).value

{- | One expense on January's month end, where a column boundary falls, and one on
February's, which is also the last day the assembly asks for — so both ends of the
range it selects transactions over are exercised.
-}
boundaryJournal :: Text
boundaryJournal =
    Text.unlines
        [ "2023-01-31 spend on the month end"
        , "    expenses:food    100.00 INR"
        , "    assets:cash:checking"
        , ""
        , "2023-02-15 spend mid month"
        , "    expenses:food     20.00 INR"
        , "    assets:cash:checking"
        , ""
        , "2023-02-28 spend on the last requested day"
        , "    expenses:food      3.00 INR"
        , "    assets:cash:checking"
        ]

-- * Rejecting columns that must not be reused

{- | A stale hash must cost exactly one month's recomputation.

Both halves are asserted, and the poisoning is what makes the reuse half real. A
column no honest computation could produce is stored under the /valid/ keys for
January and March and under a /stale/ key for February. If the assembly
recomputed everything the poison would never surface; if it trusted the stale key
February would be wrong; and the cache growing by exactly one entry is what says
only February was computed.
-}
staleHashRecomputesOnlyThatMonth :: IO ()
staleHashRecomputesOnlyThatMonth =
    withTemporaryJournal "stale" threeMonthJournal $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            months = fst <$> chain.months
            (correct, _) = columnsFor snapshot matched chain emptyColumns "INR" months
        keys <- traverse (requireKey chain) months
        case (keys, correct) of
            ([january, february, march], [_, correctFebruary, _]) -> do
                let seeded =
                        insertColumn january poisonColumn
                            . insertColumn february{chainHash = january.chainHash} poisonColumn
                            . insertColumn march poisonColumn
                            $ emptyColumns
                    (resumed, updated) = columnsFor snapshot matched chain seeded "INR" months
                assertEqual "three columns were seeded" 3 (columnCount seeded)
                assertEqual
                    "January and March come from the cache, February is recomputed"
                    [Just poisonQuantity, expenseTotal correctFebruary, Just poisonQuantity]
                    (expenseTotal <$> resumed)
                assertEqual
                    "only the reused months carry the poisoned account"
                    [True, False, True]
                    [Map.member poisonAccount column.expense.rows | column <- resumed]
                assertEqual
                    "the recomputed month carries the journal's own row"
                    [rowValue "INR" "expenses:food" correctFebruary.expense]
                    [rowValue "INR" "expenses:food" column.expense | column <- take 1 (drop 1 resumed)]
                assertEqual "exactly one month was added to the cache" 4 (columnCount updated)
            _ -> assertFailure ("expected three populated months, saw " <> show (length months))
  where
    expenseTotal column = (conversionFromMixed "INR" column.expense.total).value

requireKey :: PrefixChain -> Day -> IO ColumnKey
requireKey chain monthEnd =
    maybe
        (assertFailure ("no chain entry for " <> show monthEnd))
        pure
        (columnKeyAt chain wholeJournalQuery "INR" monthEnd)

threeMonthJournal :: Text
threeMonthJournal =
    boundaryJournal
        <> Text.unlines
            [ ""
            , "2023-03-20 spend again"
            , "    expenses:food      7.00 INR"
            , "    assets:cash:checking"
            ]

poisonColumn :: CashflowColumn
poisonColumn = CashflowColumn{income = poisonSide, expense = poisonSide}
  where
    poisonSide = ColumnSide{rows = Map.singleton poisonAccount amount, total = amount, missing = Set.empty}
    amount =
        HledgerAmount.mixed
            [HledgerAmount.nullamt{Hledger.acommodity = "INR", Hledger.aquantity = poisonQuantity}]

poisonAccount :: Hledger.AccountName
poisonAccount = "poison:marker"

poisonQuantity :: Decimal
poisonQuantity = 424242

{- | A commodity declaration changes no transaction, so every month hash is
byte-identical and only the global fingerprint moves. Cached columns must still
all go: a display-style change is not date-scoped and can restate arbitrarily old
output. This is the case that fails if a column is keyed on the chain hash rather
than on the checkpoint hash.
-}
globalChangeInvalidatesColumns :: IO ()
globalChangeInvalidatesColumns =
    withJournalPath "global" $ \path -> do
        before <- snapshotAt path threeMonthJournal
        after <- snapshotAt path ("commodity 1000.000 INR\n\n" <> threeMonthJournal)
        let beforeChain = chainOf before.journal
            afterChain = chainOf after.journal
            months = fst <$> beforeChain.months
            (_, stored) = columnsFor before before.transactions beforeChain emptyColumns "INR" months
        assertEqual
            "the declaration must change only the global fingerprint"
            GlobalChanged
            (diffChains beforeChain afterChain)
        assertEqual
            "month hashes alone cannot see a global change"
            (fmap snd beforeChain.months)
            (fmap snd afterChain.months)
        assertBool "the earlier revision must have produced columns" (columnCount stored > 0)
        assertEqual
            "a global change must invalidate every column"
            0
            (columnCount (truncateColumns afterChain Identical stored))
        assertEqual
            "GlobalChanged must invalidate every column against its own chain"
            0
            (columnCount (truncateColumns beforeChain GlobalChanged stored))

{- | 'DivergesFrom' keeps the months strictly before the divergence and drops the
divergent month itself, because that month's own content is what changed.
-}
truncationDropsFromDivergence :: IO ()
truncationDropsFromDivergence =
    withTemporaryJournal "truncate" threeMonthJournal $ \snapshot -> do
        let chain = chainOf snapshot.journal
            months = fst <$> chain.months
            (_, stored) = columnsFor snapshot snapshot.transactions chain emptyColumns "INR" months
            monthsKept diff = columnMonths wholeJournalQuery "INR" (truncateColumns chain diff stored)
        assertEqual "three populated months" 3 (length months)
        assertEqual "all three are cached" months (columnMonths wholeJournalQuery "INR" stored)
        sequence_
            [ assertEqual
                ("divergence at " <> show divergence <> " keeps the earlier months")
                (filter (< divergence) months)
                (monthsKept (DivergesFrom divergence))
            | divergence <- months
            ]
        assertEqual "an identical chain keeps every column" months (monthsKept Identical)

{- | An expense booked in a commodity that never has a price, so the display
conversion is incomplete and the timeline is withheld. The resumed path must
reproduce the same set, which is what decides whether a report is shown at all.
-}
missingPriceReproducesMissingSet :: IO ()
missingPriceReproducesMissingSet =
    withTemporaryJournal "missing-price" unpricedJournal $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            expected = oracle snapshot matched Monthly "INR" 2
            (actual, _) = resume snapshot matched chain emptyColumns Monthly "INR" 2
        assertEqual "the fixture must leave a commodity unconverted" (Set.singleton "ZZZ") expected.missing
        assertBool "an incomplete conversion withholds the timeline" (not expected.available)
        assertFiguresEqual "unpriced commodity" expected actual

{- | Why 'sumColumns' recomputes the missing set instead of unioning the months'.

An unpriced commodity that cancels within the year is missing from two monthly
columns and from no yearly one, because hledger computes the yearly column first
and only then asks what could not be converted. Unioning the monthly sets would
withhold a yearly timeline that the unchecked path shows.
-}
cancellingCommodityIsNotMissingForTheYear :: IO ()
cancellingCommodityIsNotMissingForTheYear =
    withTemporaryJournal "cancelling" cancellingJournal $ \snapshot -> do
        let matched = snapshot.transactions
            chain = chainOf snapshot.journal
            expectedFor granularity = oracle snapshot matched granularity "INR" 2
            resumedFor granularity = fst (resume snapshot matched chain emptyColumns granularity "INR" 2)
        assertEqual
            "the commodity is missing month by month"
            (Set.singleton "ZZZ")
            (expectedFor Monthly).missing
        assertEqual
            "and converts completely once the year is added up"
            Set.empty
            (expectedFor Yearly).missing
        assertFiguresEqual "cancelling, monthly" (expectedFor Monthly) (resumedFor Monthly)
        assertFiguresEqual "cancelling, yearly" (expectedFor Yearly) (resumedFor Yearly)

cancellingJournal :: Text
cancellingJournal =
    Text.unlines
        [ "2023-01-10 priced spend"
        , "    expenses:food    100.00 INR"
        , "    assets:cash:checking"
        , ""
        , "2023-02-10 unpriced spend"
        , "    expenses:travel     5 ZZZ"
        , "    assets:cash:zzz"
        , ""
        , "2023-03-10 unpriced refund"
        , "    expenses:travel    -5 ZZZ"
        , "    assets:cash:zzz"
        ]

unpricedJournal :: Text
unpricedJournal =
    Text.unlines
        [ "2023-01-10 priced spend"
        , "    expenses:food    100.00 INR"
        , "    assets:cash:checking"
        , ""
        , "2023-02-10 unpriced spend"
        , "    expenses:travel     5 ZZZ"
        , "    assets:cash:zzz"
        ]

-- * Fixtures and shared helpers

{- | The query text a whole-journal request carries. Keys are over the query's
canonical text, so the empty query stands for \"every transaction\".
-}
wholeJournalQuery :: Text
wholeJournalQuery = ""

{- | The same journals 'Hedger.Domain.PeriodReportSpec' measures the consolidated
report over: missing prices, foreign cash, dust residuals, nested mappings and
gapped price histories.
-}
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

caseLabel :: Text -> Granularity -> Text -> Int -> String
caseLabel path granularity commodity depth =
    Text.unpack path
        <> " "
        <> show granularity
        <> " "
        <> Text.unpack commodity
        <> " depth "
        <> show depth

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
        let dir = base </> ("hedger-cashflow-checkpoint-spec-" <> label)
        exists <- Directory.doesDirectoryExist dir
        when exists (Directory.removeDirectoryRecursive dir)
        Directory.createDirectoryIfMissing True dir
        pure dir
