{- | Checkpointing for the cashflow reports, one calendar month at a time.

A cashflow report is @CalcChange@ accumulated @PerPeriod@, and
'Hedger.Domain.Internal.PeriodReport.periodicBalanceReport' pairs @PerPeriod@
with @AtThen@ valuation. @AtThen@ values every posting at the posting's own date,
so a column is the sum of the postings dated inside it, each valued
independently of every other column. Unlike net worth — whose @Historical@
accumulation makes column @k@ depend on all of columns @0..k@ — a cashflow month
is therefore self-contained: it is a function of that month's entries alone, with
no accumulator to thread and nothing to resume.

Two consequences shape this module:

* A month is immutable once its entries are fixed, so the cache unit is one
  month and a request assembles its columns from the cache rather than folding.
* A calendar year is the sum of its twelve months. Row amounts and totals add
  exactly; the @missing@ set does /not/ union, and 'sumColumns' recomputes it —
  see that function's note.

Columns are cached /unclipped/, per account, and depth is applied afterwards by
'collapseToDepth'. One cached month therefore serves every requested depth. That
is exact because hledger clips account names on the postings before it
aggregates, so an unclipped row's balance already contains everything beneath it
and re-collapsing merely re-sums the same postings —
'Hedger.Domain.Internal.PeriodReport.reportSide' documents the same fact for the
side split.

Nothing here does I/O or retains an hledger intermediate: a cached column holds
only a @Map AccountName MixedAmount@, one @MixedAmount@ and a @Set Text@, all
forced before storage.
-}
module Hedger.Domain.Internal.CashflowCheckpoint (
    -- * Cached columns
    ColumnKey (..),
    ColumnSide (..),
    CashflowColumn (..),
    CashflowColumns,
    emptyColumns,
    emptyColumn,
    columnCount,
    columnMonths,
    columnKeyAt,
    lookupColumn,
    insertColumn,
    truncateColumns,

    -- * Projections
    sumColumns,
    collapseToDepth,
    monthEndsIn,

    -- * Resumed calculation
    cashflowColumnsResumed,
    chunkMonths,
    maxChunkMonths,
)
where

import Data.Decimal (Decimal, decimalMantissa, decimalPlaces)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, toGregorian)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Account (atDepth)
import Hedger.Domain.Fingerprint (ChainDiff (..), Hash, PrefixChain)
import Hedger.Domain.Internal.Cashflow (incomeExpensePeriodicReport)
import Hedger.Domain.Internal.InvestmentCheckpoint (checkpointHashAt)
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    ReportSide (..),
    reportPeriodsFrom,
    reportSide,
 )
import Hedger.Domain.Period (Granularity (Monthly), ReportPeriod (..), atIndex, epoch, monthPeriod)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed, missingCommodities)

{- | Everything a cached column's contents depend on.

The query is identified by its /canonical text/ rather than by the transaction
list it selected: the text is small, comparable and stable, whereas hashing a
matched list would cost as much as recomputing the column. Callers must pass the
canonicalised text of the query that produced the matched transactions; two
different queries sharing a key would serve each other's numbers.

@reference@ is the date that text was resolved against, and it is part of the
key for the same reason it is part of
'Hedger.Domain.Internal.NetWorthCheckpoint.QueryKey': a query resolves against
@snapshot.latestJournalDate@, so a relative term such as @date:thismonth@ denotes
a different span once an append moves that date, and it can restate arbitrarily
old columns — which the month chain, diverging only at the appended month, does
not flag.
-}
data ColumnKey = ColumnKey
    { query :: !Text
    , reference :: !Day
    , commodity :: !Text
    , monthEnd :: !Day
    , chainHash :: !Hash
    }
    deriving stock (Eq, Ord)

{- | One side of one column: per-account amounts at full, unclipped account
names, the column's own total, and the commodities that could not be projected
into the display commodity anywhere in the column.

@missing@ spans the total /and/ every row, matching
'Hedger.Domain.Internal.PeriodReport.reportMissing', which is what decides
whether a cashflow timeline is available at all.
-}
data ColumnSide = ColumnSide
    { rows :: !(Map Hledger.AccountName Hledger.MixedAmount)
    , total :: !Hledger.MixedAmount
    , missing :: !(Set Text)
    }

data CashflowColumn = CashflowColumn
    { income :: !ColumnSide
    , expense :: !ColumnSide
    }

{- | Cached columns for every (query, commodity, month) observed so far.

There is no sparse-retention policy, unlike the investment checkpoints: a yearly
report needs all twelve of a year's months, so dropping months would not save a
recomputation, only defer it. A column is a handful of small values, so keeping
every month costs O(months) tiny records rather than O(months) fold states.
-}
newtype CashflowColumns = CashflowColumns (Map ColumnKey CashflowColumn)

emptyColumns :: CashflowColumns
emptyColumns = CashflowColumns Map.empty

columnCount :: CashflowColumns -> Int
columnCount (CashflowColumns stored) = Map.size stored

{- | The cached months for one query — text /and/ reference date — and commodity,
oldest first.

The reference date is a parameter rather than ignored so that this cannot be used
to observe a column under the query text alone: doing so would report a hit for a
relative query resolved against some other date, which is the case
'ColumnKey.reference' exists to separate.
-}
columnMonths :: Text -> Day -> Text -> CashflowColumns -> [Day]
columnMonths query reference commodity (CashflowColumns stored) =
    List.sort
        [ key.monthEnd
        | key <- Map.keys stored
        , key.query == query
        , key.reference == reference
        , key.commodity == commodity
        ]

{- | The key a month's column is cached under, or 'Nothing' when the chain has no
entry for that month — in which case the month has no journal entry at all and
its column is 'emptyColumn' rather than something to store.
-}
columnKeyAt :: PrefixChain -> Text -> Day -> Text -> Day -> Maybe ColumnKey
columnKeyAt chain query reference commodity monthEnd =
    ( \hash ->
        ColumnKey
            { query = query
            , reference = reference
            , commodity = commodity
            , monthEnd = monthEnd
            , chainHash = hash
            }
    )
        <$> checkpointHashAt chain monthEnd

lookupColumn :: ColumnKey -> CashflowColumns -> Maybe CashflowColumn
lookupColumn key (CashflowColumns stored) = Map.lookup key stored

insertColumn :: ColumnKey -> CashflowColumn -> CashflowColumns -> CashflowColumns
insertColumn key column (CashflowColumns stored) =
    CashflowColumns (Map.insert key (forcedColumn column) stored)

-- | A month with no entries at all: no rows, a zero total, nothing missing.
emptyColumn :: CashflowColumn
emptyColumn = CashflowColumn{income = emptySide, expense = emptySide}

emptySide :: ColumnSide
emptySide =
    ColumnSide
        { rows = Map.empty
        , total = HledgerAmount.nullmixedamt
        , missing = Set.empty
        }

{- | Drop every column the diff condemns, and every column whose stored hash no
longer matches the chain. The second condition is the one that must never be
skipped: a wrongly kept column yields silently stale numbers with no other
symptom.
-}
truncateColumns :: PrefixChain -> ChainDiff -> CashflowColumns -> CashflowColumns
truncateColumns chain diff (CashflowColumns stored) =
    CashflowColumns (Map.filterWithKey keep stored)
  where
    keep key _ = keyStillValid chain diff key.monthEnd key.chainHash

{- | The same predicate 'Hedger.Domain.Internal.InvestmentCheckpoint' applies to
its own keys, restated here because that module keeps it private. Both rest on
'checkpointHashAt', which is the only safe thing to key a checkpoint on: the
chain's month hashes are seeded from a constant rather than from the global
fingerprint, so they are byte-identical across a commodity-style,
account-type-declaration or configuration change, and a key built from a month
hash alone would survive exactly the edits 'GlobalChanged' exists to catch.

The diff is taken as a 'ChainDiff' rather than a @Maybe Day@ on purpose: mapping
it down to a divergence date turns 'GlobalChanged' into \"no divergence\", which
reads as \"keep everything\".
-}
keyStillValid :: PrefixChain -> ChainDiff -> Day -> Hash -> Bool
keyStillValid chain diff day hash = case diff of
    GlobalChanged -> False
    DivergesFrom divergence -> day < divergence && hashMatches
    Identical -> hashMatches
  where
    hashMatches = checkpointHashAt chain day == Just hash

{- | Add columns together — the yearly-from-monthly projection.

Row amounts and column totals add exactly, because @AtThen@ has already valued
each posting at its own date: a year's column and the sum of its months' columns
are sums of the same valued postings.

@missing@ is the exception, and unioning the months' sets would be wrong. A
foreign commodity can be non-negligible in individual months and cancel to
nothing across the year, in which case hledger's yearly report reports no
missing commodity at all. So the set is recomputed from the summed amounts,
which is what a report built at yearly granularity would see.

A single column is returned untouched, so the monthly path stores and serves
exactly the same value.
-}
sumColumns :: Text -> [CashflowColumn] -> CashflowColumn
sumColumns _ [] = emptyColumn
sumColumns _ [only] = only
sumColumns commodity columns =
    CashflowColumn
        { income = sumSides commodity (fmap (.income) columns)
        , expense = sumSides commodity (fmap (.expense) columns)
        }

sumSides :: Text -> [ColumnSide] -> ColumnSide
sumSides commodity sides =
    ColumnSide{rows = rows, total = total, missing = missingIn commodity total rows}
  where
    rows = Map.unionsWith HledgerAmount.maPlus (fmap (.rows) sides)
    total = List.foldl' HledgerAmount.maPlus HledgerAmount.nullmixedamt (fmap (.total) sides)

missingIn :: Text -> Hledger.MixedAmount -> Map Hledger.AccountName Hledger.MixedAmount -> Set Text
missingIn commodity total rows =
    Set.unions (missingCommodities commodity <$> total : Map.elems rows)

{- | Unclipped rows collapsed to a requested depth.

Deliberately identical to
'Hedger.Domain.Internal.PeriodReport.breakdownItemsFor': absolute values are
taken first, dust is dropped before grouping rather than after, and accounts are
then summed under 'atDepth'. Dropping dust first is what makes the two agree —
several dust rows under one parent must not add up to a visible item.
-}
collapseToDepth :: Text -> Int -> ColumnSide -> [BreakdownItem]
collapseToDepth commodity depth side =
    [ BreakdownItem{account = account, quantity = quantity}
    | (account, quantity) <-
        Map.toList (Map.fromListWith (+) [(atDepth depth account, quantity) | (account, quantity) <- visible])
    ]
  where
    visible =
        [ (account, abs quantity)
        | (account, amount) <- Map.toList side.rows
        , Just quantity <- [(conversionFromMixed commodity amount).value]
        , abs quantity >= dustQuantity
        ]

-- | The threshold 'Hedger.Domain.Internal.PeriodReport.breakdownItemsFor' applies.
dustQuantity :: Decimal
dustQuantity = 0.000001

{- | The month ends of every calendar month overlapping an inclusive day range.

A monthly report period yields its own single month; a yearly one yields the
twelve months it is the sum of. Overlap rather than containment, so a period
whose final month is clipped short still contributes that month.
-}
monthEndsIn :: Day -> Day -> [Day]
monthEndsIn start end
    | end < start = []
    | otherwise = takeWhile withinRange (iterate nextMonthEnd (monthPeriod start).end)
  where
    withinRange monthEnd = (monthPeriod monthEnd).start <= end
    nextMonthEnd monthEnd = (monthPeriod (addDays 1 monthEnd)).end

{- | The requested months' columns, computing only those not already cached.

Returns one column per requested day, in the requested order, plus the input map
extended with everything computed on this sweep. A month with no chain entry has
no journal entry either — the chain covers price directives as well as
transactions — so its column is provably empty and is neither computed nor
cached.

Reuse is licensed by the hash, not by the date: a cached column whose stored
'checkpointHashAt' no longer matches the chain belongs to a different journal
prefix, so it is recomputed rather than trusted.
-}
cashflowColumnsResumed ::
    Snapshot ->
    -- | The transactions the query matched.
    [Hledger.Transaction] ->
    PrefixChain ->
    CashflowColumns ->
    -- | Canonical query text; see 'ColumnKey'.
    Text ->
    -- | Display commodity.
    Text ->
    -- | Month ends wanted.
    [Day] ->
    ([CashflowColumn], CashflowColumns)
cashflowColumnsResumed snapshot matched chain (CashflowColumns stored) query commodity requested =
    (fmap resolve requested, CashflowColumns (Map.union computed stored))
  where
    -- The reference date is not a parameter because it is not an independent
    -- one: a caller resolves its query against this very snapshot's
    -- 'latestJournalDate', so taking it from the snapshot makes the key's
    -- reference agree with the query's meaning by construction instead of by
    -- the caller remembering to pass the same date twice. Read here rather than
    -- inside 'computeMonths', which deliberately moves the latest date back per
    -- chunk to steer the report window.
    keyFor = columnKeyAt chain query (fromMaybe epoch snapshot.latestJournalDate) commodity
    pending =
        [ (monthEnd, key)
        | monthEnd <- List.sort (List.nub requested)
        , Just key <- [keyFor monthEnd]
        , not (Map.member key stored)
        ]
    freshColumns = computeMonths snapshot matched commodity (fmap fst pending)
    computed =
        Map.fromList
            [ (key, Map.findWithDefault emptyColumn monthEnd freshColumns)
            | (monthEnd, key) <- pending
            ]
    resolve monthEnd = case keyFor monthEnd of
        Nothing -> emptyColumn
        Just key ->
            fromMaybe emptyColumn (Map.lookup key computed `orElse` Map.lookup key stored)

orElse :: Maybe value -> Maybe value -> Maybe value
orElse (Just value) _ = Just value
orElse Nothing fallback = fallback

{- | The columns for a set of months, built from as few hledger reports as the
24-month report window allows.

'Hedger.Domain.Internal.PeriodReport.reportWindow' keeps only the last 24 monthly
periods, and the window is anchored at @latestJournalDate@ rather than at the
transactions handed in, so the months a report yields cannot be chosen by
filtering transactions alone. Presenting the snapshot with its latest date moved
back to the end of a chunk moves the window there instead, and chunking to
'maxChunkMonths' keeps every wanted month inside it.

Moving the latest date also narrows the price directives the report keeps to
those at or before the chunk's end. That is invisible under @AtThen@: every
posting in the chunk is dated at or before that day, and hledger resolves a price
as of the queried date, so a later directive could not have been consulted
anyway.
-}
computeMonths ::
    Snapshot ->
    [Hledger.Transaction] ->
    Text ->
    [Day] ->
    Map Day CashflowColumn
computeMonths snapshot matched commodity months =
    Map.unions (chunkColumns <$> chunkMonths months)
  where
    chunkColumns [] = Map.empty
    chunkColumns (firstMonth : laterMonths)
        | null chunkTransactions = Map.empty
        | otherwise = Map.fromList (reportColumns snapshot commodity report)
      where
        chunkStart = (monthPeriod firstMonth).start
        chunkEnd = List.foldl' max firstMonth laterMonths
        chunkTransactions =
            filter (\transaction -> transaction.tdate >= chunkStart && transaction.tdate <= chunkEnd) matched
        report =
            incomeExpensePeriodicReport
                snapshot{latestJournalDate = Just chunkEnd}
                chunkTransactions
                Monthly
                commodity

{- | Group ascending month ends so that each group spans at most
'maxChunkMonths' calendar months.
-}
chunkMonths :: [Day] -> [[Day]]
chunkMonths [] = []
chunkMonths (first : rest) = (first : within) : chunkMonths beyond
  where
    (within, beyond) = span nearFirst rest
    nearFirst day = monthOrdinal day - monthOrdinal first < fromIntegral maxChunkMonths

{- | The number of monthly periods
'Hedger.Domain.Internal.PeriodReport.reportWindow' retains. A chunk spanning more
than this would silently lose its earliest months to that window's @takeLast@.
-}
maxChunkMonths :: Int
maxChunkMonths = 24

monthOrdinal :: Day -> Integer
monthOrdinal day =
    let (year, month, _) = toGregorian day
     in year * 12 + fromIntegral month

{- | One column per report column, split into the two cashflow sides.

The report is built at monthly granularity, so each of its date spans is one
calendar month and its index into a row's amounts is the index of that month.
-}
reportColumns ::
    Snapshot ->
    Text ->
    HledgerMultiBalance.MultiBalanceReport ->
    [(Day, CashflowColumn)]
reportColumns snapshot commodity report =
    [ ( period.end
      , forcedColumn
            CashflowColumn
                { income = sideColumn index incomeSide
                , expense = sideColumn index expenseSide
                }
      )
    | (index, period) <- zip [0 ..] (reportPeriodsFrom Monthly report)
    ]
  where
    incomeSide = reportSide snapshot.journal [Hledger.Revenue, Hledger.Gain] report
    expenseSide = reportSide snapshot.journal [Hledger.Expense] report
    sideColumn index side =
        ColumnSide{rows = rows, total = total, missing = missingIn commodity total rows}
      where
        total = fromMaybe HledgerAmount.nullmixedamt (atIndex index side.columns)
        rows =
            Map.fromListWith
                HledgerAmount.maPlus
                [ (row.prrName.displayFull, amount)
                | row <- side.rows
                , Just amount <- [atIndex index row.prrAmounts]
                ]

{- | Force a column's payload before it is stored.

The record's fields are strict, but a @!@ reaches only the constructor: the
@Map@ would still hold unevaluated 'Hledger.MixedAmount's, each of those
unevaluated 'Decimal' quantities, and the @Set@ an unevaluated tree. Left as
thunks they keep the whole hledger report alive and defer its work into whichever
later request first reads the column — the deferred-work failure the caching plan
warns about, which no allocation figure would reveal.
-}
forcedColumn :: CashflowColumn -> CashflowColumn
forcedColumn column =
    forcedSide column.income `seq` forcedSide column.expense `seq` column

forcedSide :: ColumnSide -> ()
forcedSide side =
    forceAmount side.total `seq`
        forceEach forceAmount (Map.elems side.rows) `seq`
            forceEach forceText (Map.keys side.rows) `seq`
                forceEach forceText (Set.toList side.missing)

forceEach :: (value -> ()) -> [value] -> ()
forceEach force = List.foldl' (\() value -> force value) ()

forceAmount :: Hledger.MixedAmount -> ()
forceAmount amount =
    forceEach
        (\part -> forceText part.acommodity `seq` forceDecimal part.aquantity)
        (HledgerAmount.amountsRaw amount)

forceText :: Text -> ()
forceText value = Text.length value `seq` ()

-- | 'seq' on a 'Decimal' stops at its constructor, leaving the mantissa a thunk.
forceDecimal :: Decimal -> ()
forceDecimal value = decimalPlaces value `seq` decimalMantissa value `seq` ()
