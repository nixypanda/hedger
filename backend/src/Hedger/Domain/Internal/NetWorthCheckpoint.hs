{- | Checkpointing for the net-worth balance sheet.

Unlike the investment scope scan, whose fold this codebase owns, net worth's
accumulation happens inside hledger: a @Historical@ multi-balance report sums
every posting from the start of the journal into each column and then values that
accumulated column @AtEnd@ of the column's own last day. This module takes the
accumulation over and leaves everything else where it was.

What makes that safe is the shape of hledger's own calculation. In
@Hledger.Reports.MultiBalanceReport@ the historical row is

> historical = avalue $ cumulativeSum unvaluedChanges

— a cumulative sum of /unvalued/ per-period changes, valued afterwards, one
valuation date per column. Two consequences follow, and they are the whole
argument for this module:

1. A cumulative balance at a month end is a raw, unvalued quantity per account.
   It is a function of the journal prefix up to that month and of nothing else,
   so it survives any later append — including an appended price directive,
   which cannot change a quantity.
2. Valuation is applied /after/ the sum, per column, so a resumed column can be
   valued exactly as hledger would value it: with the report journal's own price
   oracle and display styles, at the column's inclusive end date.

So a checkpoint holds the unvalued cumulative balances per account at a month
end, later months are extended by per-account /changes/ that hledger still
produces (one unvalued @CalcChange@ + @PerPeriod@ report over the suffix), and
only the final valuation and the per-side selection stay where they were.

Nothing here holds an hledger intermediate: a checkpoint is
@Map AccountName MixedAmount@ and nothing else. No 'Hledger.Journal', no
'HledgerMultiBalance.MultiBalanceReport', no closure over either.
-}
module Hedger.Domain.Internal.NetWorthCheckpoint (
    -- * Keys
    QueryKey (..),
    OpeningKey (..),
    openingHashAt,

    -- * Opening balances
    AccountBalances,
    OpeningBalances,
    emptyOpeningBalances,
    openingBalanceCount,
    openingBalanceDays,
    lookupOpeningBalances,
    truncateOpeningBalances,

    -- * Retention
    retainedOpeningDays,

    -- * The resumed report
    ResumedBalanceSheet (..),
    assetLiabilityResumed,
)
where

import Data.Decimal (decimalMantissa, decimalPlaces)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Dates qualified as HledgerDates
import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Fingerprint (ChainDiff (..), Hash, PrefixChain (..))
import Hedger.Domain.Internal.InvestmentCheckpoint (checkpointHashAt, retainedMonths)
import Hedger.Domain.Internal.NetWorth (
    assetLiabilityAccountTypes,
    assetLiabilityDeltaReport,
    assetLiabilityPeriodicReport,
 )
import Hedger.Domain.Internal.PeriodReport (
    periodicBalanceColumns,
    reportJournal,
    reportValuation,
 )
import Hedger.Domain.Period (Granularity, ReportPeriod (..), atIndex, lastMaybe, monthStart)
import Hedger.Domain.Snapshot (Snapshot (..))

{- | The request text a set of matched transactions came from, and the date that
text was resolved against.

Keyed on the text rather than on the matched transaction list because the list is
large, and because the chain hash already attests that the journal prefix behind
it has not moved: the same text over the same prefix matches the same
transactions.

The reference date is part of the key because it is part of the query's meaning.
@Hedger.Application.Analytics@ resolves a query against
@snapshot.latestJournalDate@, so a relative term such as @date:thismonth@ denotes
a different span once an append moves that date — and it can restate arbitrarily
old columns, which the month chain, diverging only at the appended month, would
not catch.
-}
data QueryKey = QueryKey
    { text :: !Text
    , reference :: !Day
    }
    deriving stock (Eq, Ord, Show)

{- | Everything that fixes one month's opening balances: the query they were
matched by, the display commodity of the request that produced them, the month
end itself, and the hash of the journal prefix behind it.

The commodity does not change an unvalued balance, and is in the key only so that
it cannot silently start to matter.
-}
data OpeningKey = OpeningKey
    { query :: !QueryKey
    , commodity :: !Text
    , monthEnd :: !Day
    , chainHash :: !Hash
    }
    deriving stock (Eq, Ord, Show)

-- | Raw, unvalued cumulative balances per account, exclusive of subaccounts.
type AccountBalances = Map Hledger.AccountName Hledger.MixedAmount

{- | Opening balances at month ends: sparse in the months it retains, complete in
the accounts each retained month covers.

Completeness in accounts is what makes a resume lossless. The map is stored
before hledger's zero-row elision is reapplied, so an account that closed to zero
long ago is still present with its zero balance rather than being silently
dropped and then missed by a later month that revives it.
-}
newtype OpeningBalances = OpeningBalances (Map OpeningKey AccountBalances)

emptyOpeningBalances :: OpeningBalances
emptyOpeningBalances = OpeningBalances Map.empty

openingBalanceCount :: OpeningBalances -> Int
openingBalanceCount (OpeningBalances stored) = Map.size stored

-- | The retained month ends for one query and commodity, oldest first.
openingBalanceDays :: QueryKey -> Text -> OpeningBalances -> [Day]
openingBalanceDays query commodity (OpeningBalances stored) =
    List.sort
        [ key.monthEnd
        | key <- Map.keys stored
        , key.query == query
        , key.commodity == commodity
        ]

lookupOpeningBalances :: OpeningKey -> OpeningBalances -> Maybe AccountBalances
lookupOpeningBalances key (OpeningBalances stored) = Map.lookup key stored

{- | Drop every opening balance the diff condemns, and every one whose stored
hash no longer matches the chain.

Takes the 'ChainDiff' itself rather than a @Maybe Day@ divergence date, because
'GlobalChanged' has no divergence date and mapping it to 'Nothing' would read as
"nothing diverged" and keep everything — which is precisely the case
'GlobalChanged' exists to catch.
-}
truncateOpeningBalances :: PrefixChain -> ChainDiff -> OpeningBalances -> OpeningBalances
truncateOpeningBalances chain diff (OpeningBalances stored) =
    OpeningBalances (Map.filterWithKey keep stored)
  where
    keep key _ = case diff of
        GlobalChanged -> False
        DivergesFrom divergence -> key.monthEnd < divergence && hashMatches key
        Identical -> hashMatches key
    hashMatches key = openingHashAt chain key.monthEnd == Just key.chainHash

{- | The hash an opening balance is keyed by: the chain hash for the month
containing the day, extended with the journal-global fingerprint.

Shared with the investment checkpoints rather than reimplemented, because the
reason it is not simply the month hash is the same reason there: the month chain
is seeded from a constant, so month hashes are byte-identical across a
commodity-style, account-type-declaration or configuration change, and a key
built from one alone would survive exactly the edits 'GlobalChanged' exists to
catch.
-}
openingHashAt :: PrefixChain -> Day -> Maybe Hash
openingHashAt = checkpointHashAt

{- | Which of a sweep's month ends to retain.

The general policy is the investment slice's: every month in its dense tail, plus
one month in twelve before that, so retained state is O(years) rather than
O(months). @Map AccountName MixedAmount@ per month shares no structure between
months, unlike the cons-shared accumulator lists that made dense retention free
there, so being sparse matters more here.

The window's own opening month is retained on top of that, and it is the one that
earns the resume. A monthly request's columns are the last 24 months of the
journal, so the day it needs to open from is the end of the month /before/ that
window — one month outside any dense tail measured from the end of history, and
retained by the twelve-month grid only by luck.
-}
retainedOpeningDays ::
    -- | The day the requested window opens from.
    Day ->
    -- | Candidate month ends, oldest first.
    [Day] ->
    [Day]
retainedOpeningDays opening candidates =
    [day | day <- candidates, day == opening || Set.member day sparse]
  where
    sparse = Set.fromList (retainedMonths candidates)

{- | A balance-sheet report and the opening balances the sweep that produced it
can contribute back.

'accumulated' says whether the report came from accumulated checkpoints or from
the unchecked path. It is not an optimisation detail: a test that cannot see it
would pass vacuously whenever the resume quietly fell back.
-}
data ResumedBalanceSheet = ResumedBalanceSheet
    { report :: !HledgerMultiBalance.MultiBalanceReport
    , openings :: !OpeningBalances
    , resumedFrom :: !(Maybe Day)
    , accumulated :: !Bool
    }

{- | 'assetLiabilityPeriodicReport' with its columns accumulated from opening
balances instead of summed from the start of the journal by hledger.

Observationally equal to 'assetLiabilityPeriodicReport' in everything the
net-worth calculation projects out of it: each side's column totals, each side's
missing commodities, and the depth-collapsed breakdown of any column. The
equivalence rests on four things:

* Columns. The requested columns are hledger's own, from
  'periodicBalanceColumns', so the report window and the day each column is
  valued at are not re-derived here.
* The month-end boundary. A column ends on its last day inclusive, and a
  transaction dated on that day belongs to it. An opening balance stored at month
  end @m@ covers @{p : date p <= m}@, and the delta report starts at @m + 1@, so
  nothing dated on @m@ is replayed and nothing after it is dropped.
* Accumulation. hledger's own historical row is @cumulativeSum@ over the same
  unvalued monthly changes, in the same order, with the same starting balance.
* Valuation. Each accumulated column is valued at the column's inclusive end
  date, per account, with the report journal's price oracle and display styles —
  the same function, the same argument and the same date hledger uses. Per
  account rather than once per column total, because that is what hledger's rows
  hold and what 'Hedger.Domain.Internal.PeriodReport.reportSide' sums.

Falls back to the unchecked report whenever it cannot establish that
correspondence: no columns at all, or a requested column end that is not one of
the delta report's month ends.
-}
assetLiabilityResumed ::
    Snapshot ->
    [Hledger.Transaction] ->
    QueryKey ->
    PrefixChain ->
    OpeningBalances ->
    Granularity ->
    Text ->
    ResumedBalanceSheet
assetLiabilityResumed snapshot matched query chain stored granularity commodity =
    case (columns, lastMaybe columns) of
        (firstColumn : _, Just finalColumn) ->
            resumeFrom
                snapshot
                matched
                query
                chain
                stored
                granularity
                commodity
                columns
                firstColumn.start
                finalColumn.end
        _ -> unchecked snapshot matched stored granularity commodity
  where
    columns = periodicBalanceColumns snapshot matched granularity assetLiabilityAccountTypes

unchecked ::
    Snapshot ->
    [Hledger.Transaction] ->
    OpeningBalances ->
    Granularity ->
    Text ->
    ResumedBalanceSheet
unchecked snapshot matched stored granularity commodity =
    ResumedBalanceSheet
        { report = assetLiabilityPeriodicReport snapshot matched granularity commodity
        , openings = stored
        , resumedFrom = Nothing
        , accumulated = False
        }

resumeFrom ::
    Snapshot ->
    [Hledger.Transaction] ->
    QueryKey ->
    PrefixChain ->
    OpeningBalances ->
    Granularity ->
    Text ->
    [ReportPeriod] ->
    Day ->
    Day ->
    ResumedBalanceSheet
resumeFrom snapshot matched query chain stored granularity commodity columns windowStart windowEnd =
    case traverse (\column -> Map.lookup column.end cumulative) columns of
        Nothing ->
            -- A requested column end that no month end covers. Not expected —
            -- monthly and yearly columns both end on a month end — but cheaper
            -- to fall back than to serve an unproven column.
            unchecked snapshot matched stored granularity commodity
        Just balances ->
            ResumedBalanceSheet
                { report = resumedReport snapshot matched commodity columns balances
                , openings = insertOpenings query commodity chain retained cumulative stored
                , resumedFrom = fst <$> reusable
                , accumulated = True
                }
  where
    opening = addDays (-1) windowStart
    reusable = latestReusableOpening chain stored query commodity opening
    (deltaStart, seed) = case reusable of
        Just (at, balances) -> (addDays 1 at, balances)
        Nothing -> (monthStart (earliestPostingDay matched windowStart), Map.empty)
    deltas = assetLiabilityDeltaReport snapshot matched (deltaStart, windowEnd)
    deltaEnds = columnEnds deltas
    cumulative =
        Map.fromList (zip deltaEnds (accumulateColumns seed (columnDeltas deltaEnds deltas.prRows)))
    retained = retainedOpeningDays opening deltaEnds

{- | The earliest day any posting of these transactions is dated, defaulting to
the window start when there are none.

Taken over posting dates as well as transaction dates because hledger assigns a
posting to a column by its own date, so a posting dated before its transaction —
and before every other transaction — would fall outside a window derived from
transaction dates alone, and be counted by the unchecked report's starting
balance but not by the delta report.
-}
earliestPostingDay :: [Hledger.Transaction] -> Day -> Day
earliestPostingDay transactions fallbackDay =
    fromMaybe fallbackDay (minimumMaybe (concatMap days transactions))
  where
    days transaction =
        transaction.tdate
            : [fromMaybe transaction.tdate posting.pdate | posting <- transaction.tpostings]

minimumMaybe :: (Ord value) => [value] -> Maybe value
minimumMaybe values = case values of
    [] -> Nothing
    _ -> Just (minimum values)

{- | The newest opening balance at or before a day whose stored hash still
matches the chain. A stored balance whose hash has moved on is not stale data for
the same prefix, it is data for a different prefix, so it is skipped.
-}
latestReusableOpening ::
    PrefixChain ->
    OpeningBalances ->
    QueryKey ->
    Text ->
    Day ->
    Maybe (Day, AccountBalances)
latestReusableOpening chain (OpeningBalances stored) query commodity limit =
    lastMaybe
        [ (key.monthEnd, balances)
        | (key, balances) <- Map.toAscList stored
        , key.query == query
        , key.commodity == commodity
        , key.monthEnd <= limit
        , openingHashAt chain key.monthEnd == Just key.chainHash
        ]

-- | Each column's inclusive end date, in column order.
columnEnds :: HledgerMultiBalance.MultiBalanceReport -> [Day]
columnEnds report = [addDays (-1) end | Just end <- HledgerDates.spanEnd <$> report.prDates]

{- | One unvalued change map per column, keyed by full account name.

Rows are combined rather than assumed distinct: an unclipped flat report gives
one row per account, but combining costs nothing and makes a duplicated row a
wrong total rather than a lost one.
-}
columnDeltas :: [Day] -> [HledgerMultiBalance.MultiBalanceReportRow] -> [AccountBalances]
columnDeltas ends rows =
    [ Map.fromListWith
        HledgerAmount.maPlus
        [ (row.prrName.displayFull, amount)
        | row <- rows
        , Just amount <- [atIndex index row.prrAmounts]
        ]
    | index <- [0 .. length ends - 1]
    ]

{- | The running cumulative balance after each column's changes.

This is hledger's @cumulativeSum@ over the same values in the same order, with
the opening balance standing in for the historical bucket it puts before the
first column.
-}
accumulateColumns :: AccountBalances -> [AccountBalances] -> [AccountBalances]
accumulateColumns seed = drop 1 . List.scanl' add seed
  where
    add carried delta = Map.unionWith HledgerAmount.maPlus carried delta

{- | The synthetic report the net-worth calculation reads instead of hledger's.

Rows carry valued amounts per column and full account names, which is all
'Hedger.Domain.Internal.PeriodReport.reportSide', @reportMissing@ and
@breakdownItemsFor@ consult. Row /order/ is account-name order rather than
hledger's declaration order; none of those three projections depends on it.

Every accumulated account becomes a row, with no zero-row elision, and that is
what hledger does here too. hledger drops a flat row when its balance looks zero
in every column /and/ its posting count is zero in every column — but with
@Historical@ accumulation the counts are cumulative, because @cumulativeSum@ folds
@BalanceData@ with a @Semigroup@ that adds @bdnumpostings@. So every account with
a posting anywhere at or before the report's end has a positive count in every
column and is retained, including one that closed to zero long ago. The
accumulated map holds exactly those accounts, since each of them appears in the
delta report for the month it was posted in.

This matters rather than being a detail of presentation: a consolidated report's
per-side totals are the sum of its /retained/ rows, so eliding a row here that
hledger keeps would change a column total.

The totals row is the root account's inclusive balance, as hledger's is: the sum
of every account's unvalued balance, valued once. It is deliberately not the sum
of the emitted rows, which is what a consolidated report's sides are and what
'reportSide' recomputes per side.
-}
resumedReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Text ->
    [ReportPeriod] ->
    [AccountBalances] ->
    HledgerMultiBalance.MultiBalanceReport
resumedReport snapshot matched commodity columns balances =
    HledgerReportTypes.PeriodicReport
        { prDates = spanOf <$> columns
        , prRows = [row (HledgerReportTypes.flatDisplayName account) amounts | (account, amounts) <- valued]
        , prTotals = row () totals
        }
  where
    valueAt = reportValuation (reportJournal snapshot matched) commodity
    columnDays = (.end) <$> columns
    accounts = Set.toAscList (Set.unions (Map.keysSet <$> balances))
    valued =
        [ (account, zipWith valueAt columnDays (balanceOf account <$> balances))
        | account <- accounts
        ]
    balanceOf account = Map.findWithDefault HledgerAmount.nullmixedamt account
    totals =
        zipWith
            valueAt
            columnDays
            [ List.foldl' HledgerAmount.maPlus HledgerAmount.nullmixedamt (Map.elems column)
            | column <- balances
            ]
    row name amounts =
        HledgerReportTypes.PeriodicReportRow
            { prrName = name
            , prrAmounts = amounts
            , prrTotal = fromMaybe HledgerAmount.nullmixedamt (lastMaybe amounts)
            , prrAverage = snd (HledgerAmount.sumAndAverageMixedAmounts amounts)
            }
    spanOf column =
        Hledger.DateSpan
            (Just (Hledger.Exact column.start))
            (Just (Hledger.Exact (addDays 1 column.end)))

{- | Store the retained months of this sweep.

Every stored map is forced deeply first. The field is strict, but a @!@ reaches
only the @Map@'s root: without this the per-account @maPlus@ sums accumulated
across the sweep would sit in the checkpoint as thunks and be paid by whichever
later request first read them, which is the deferred-work failure this design is
supposed to avoid.
-}
insertOpenings ::
    QueryKey ->
    Text ->
    PrefixChain ->
    [Day] ->
    Map Day AccountBalances ->
    OpeningBalances ->
    OpeningBalances
insertOpenings query commodity chain retained cumulative (OpeningBalances stored) =
    OpeningBalances (List.foldl' insert stored retained)
  where
    insert acc day = case (openingHashAt chain day, Map.lookup day cumulative) of
        (Just hash, Just balances) -> Map.insert (keyFor day hash) (forcedBalances balances) acc
        _ -> acc
    keyFor day hash =
        OpeningKey{query = query, commodity = commodity, monthEnd = day, chainHash = hash}

forcedBalances :: AccountBalances -> AccountBalances
forcedBalances balances = Map.foldl' (\() amount -> forceMixed amount) () balances `seq` balances

forceMixed :: Hledger.MixedAmount -> ()
forceMixed amount =
    List.foldl' (\() part -> forceAmount part) () (HledgerAmount.amountsRaw amount)

-- | 'seq' on a 'Data.Decimal.Decimal' stops at its constructor, leaving the mantissa a thunk.
forceAmount :: Hledger.Amount -> ()
forceAmount amount =
    Text.length amount.acommodity
        `seq` amount.acost
        `seq` decimalPlaces amount.aquantity
        `seq` decimalMantissa amount.aquantity
        `seq` ()
