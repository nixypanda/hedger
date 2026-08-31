{- | Checkpointing for the investment scope scan.

'Hedger.Domain.Internal.InvestmentPerformance.scopeScansAt' is a strict left fold
of 'Accumulator' over date-sorted transactions that emits a 'ScopeScan' whenever
it passes a requested day. Every accumulator field is cumulative-to-date and
'stepAccumulator' looks at nothing but the accumulator so far and the single
transaction it is handed, so the state at a month end is a pure function of the
journal prefix up to that month. That is what makes it cacheable: a checkpoint
taken at month @m@ can seed the same fold over the transactions after @m@ and
produce bit-identical results, provided the prefix that produced it is still the
same prefix — which is exactly what the chain hash at @m@ attests.

Nothing here does I/O or holds hledger intermediates; a checkpoint holds only the
strict 'Accumulator' record and the small 'ScopeScan' it emitted.
-}
module Hedger.Domain.Internal.InvestmentCheckpoint (
    -- * Checkpoints
    CheckpointKey (..),
    ScopeCheckpoint (..),
    ScopeCheckpoints,
    emptyCheckpoints,
    checkpointCount,
    checkpointMonths,
    checkpointHashAt,
    truncateCheckpoints,

    -- * Solved rates
    RateKey (..),
    SolvedRates,
    emptySolvedRates,
    solvedRateCount,
    lookupSolvedRate,
    insertSolvedRate,
    truncateSolvedRates,

    -- * Retention policy
    retainedMonths,
    retainsMonth,
    denseTailMonths,
    sparseCheckpointInterval,

    -- * Resumed calculation
    ScopeCache (..),
    emptyScopeCache,
    truncateScopeCache,
    scopeScansResumed,
    summariesAtResumed,
    summariesAtScopesResumed,
)
where

import Data.Decimal (Decimal, decimalMantissa, decimalPlaces)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Fingerprint (
    ChainDiff (..),
    GlobalFingerprint (..),
    Hash (..),
    PrefixChain (..),
    chainHashAt,
    combineHash,
    hashText,
 )
import Hedger.Domain.Internal.InvestmentPerformance (
    Accumulator,
    InvestmentPerformanceContext (..),
    ScopeScan (..),
    SummaryResult,
    holdingsForScopesAtWithJournal,
    holdingsSeriesAtWithJournal,
    initialAccumulator,
    scopePlan,
    snapshotAccumulator,
    stepAccumulator,
    summaryFromHoldingsUsing,
    xirrFromCashFlows,
 )
import Hedger.Domain.Period (Granularity)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Xirr (cashFlowAmount, cashFlowDate)
import Hedger.Domain.Xirr qualified as Xirr

{- | Everything that changes the fold's result at one emitted day: the scope and
display commodity that parameterise 'stepAccumulator', the day itself, and the
hash of the journal prefix that produced the state.
-}
data CheckpointKey = CheckpointKey
    { scope :: !(Maybe Text)
    , commodity :: !Text
    , monthEnd :: !Day
    , chainHash :: !Hash
    }
    deriving stock (Eq, Ord)

{- | The fold state at a month end together with the view it emitted.

Retaining accumulators at many months is affordable because they share
structure: 'Accumulator.cashFlowsTotal' is consed newest-first, so the list at
month @k@ has the list at month @k-1@ as its tail, and the @Set Text@ fields are
persistent trees with path copying. Dense checkpoints therefore cost O(n) in
total rather than O(n x months).
-}
data ScopeCheckpoint = ScopeCheckpoint
    { accumulator :: !Accumulator
    , scan :: !ScopeScan
    }

newtype ScopeCheckpoints = ScopeCheckpoints (Map CheckpointKey ScopeCheckpoint)

emptyCheckpoints :: ScopeCheckpoints
emptyCheckpoints = ScopeCheckpoints Map.empty

checkpointCount :: ScopeCheckpoints -> Int
checkpointCount (ScopeCheckpoints stored) = Map.size stored

-- | The checkpointed days for one scope and commodity, oldest first.
checkpointMonths :: Maybe Text -> Text -> ScopeCheckpoints -> [Day]
checkpointMonths scope commodity (ScopeCheckpoints stored) =
    List.sort
        [ key.monthEnd
        | key <- Map.keys stored
        , key.scope == scope
        , key.commodity == commodity
        ]

{- | Drop every checkpoint the diff condemns, and every checkpoint whose stored
hash no longer matches the chain. The second condition is the one that must never
be skipped: a wrongly kept checkpoint yields silently stale numbers and has no
other symptom.
-}
truncateCheckpoints :: PrefixChain -> ChainDiff -> ScopeCheckpoints -> ScopeCheckpoints
truncateCheckpoints chain diff (ScopeCheckpoints stored) =
    ScopeCheckpoints (Map.filterWithKey keep stored)
  where
    keep key _ = keyStillValid chain diff key.monthEnd key.chainHash

{- | An annualised return already solved for one day's cash-flow prefix. Keyed
like a checkpoint, so an append re-solves only the periods whose prefix changed.
-}
data RateKey = RateKey
    { scope :: !(Maybe Text)
    , commodity :: !Text
    , at :: !Day
    , chainHash :: !Hash
    }
    deriving stock (Eq, Ord)

{- | Solved rates keyed by period. The value matches what the summary calculation
already produces per day: 'Xirr.XirrError' distinguishes the ordinary
"not enough data" outcomes from a genuine non-convergence, and the summary needs
that distinction to raise 'UnresolvedReturn'.
-}
newtype SolvedRates = SolvedRates (Map RateKey (Either Xirr.XirrError Decimal))

emptySolvedRates :: SolvedRates
emptySolvedRates = SolvedRates Map.empty

solvedRateCount :: SolvedRates -> Int
solvedRateCount (SolvedRates stored) = Map.size stored

lookupSolvedRate :: RateKey -> SolvedRates -> Maybe (Either Xirr.XirrError Decimal)
lookupSolvedRate key (SolvedRates stored) = Map.lookup key stored

insertSolvedRate :: RateKey -> Either Xirr.XirrError Decimal -> SolvedRates -> SolvedRates
insertSolvedRate key outcome (SolvedRates stored) = SolvedRates (Map.insert key outcome stored)

truncateSolvedRates :: PrefixChain -> ChainDiff -> SolvedRates -> SolvedRates
truncateSolvedRates chain diff (SolvedRates stored) =
    SolvedRates (Map.filterWithKey keep stored)
  where
    keep key _ = keyStillValid chain diff key.at key.chainHash

keyStillValid :: PrefixChain -> ChainDiff -> Day -> Hash -> Bool
keyStillValid chain diff day hash = case diff of
    GlobalChanged -> False
    DivergesFrom divergence -> day < divergence && hashMatches
    Identical -> hashMatches
  where
    hashMatches = checkpointHashAt chain day == Just hash

{- | The hash a checkpoint or solved rate is keyed by: the chain hash for the
month containing the day, extended with the journal-global fingerprint.

Binding the global fingerprint in here is what makes this key safe, and it is not
optional. 'Hedger.Domain.Fingerprint.prefixChain' seeds the month chain from a
constant rather than from the fingerprint, so the month hashes are identical
across a commodity-style, account-type-declaration or configuration change even
though such a change can restate arbitrarily old output. A key built from the
month hash alone would therefore still match after exactly the edits that
'GlobalChanged' exists to catch.

The month hash covers every entry up to the /end/ of the day's month, so for a
mid-month day it commits to a strict superset of the prefix the value depends on.
That is conservative in the safe direction: an unchanged hash still implies an
unchanged prefix, and the only cost is re-deriving a mid-month value after a
later-in-the-month append.
-}
checkpointHashAt :: PrefixChain -> Day -> Maybe Hash
checkpointHashAt chain day = extend (globalHash chain.global) <$> chainHashAt chain day
  where
    extend global (Hash month) = combineHash global month

globalHash :: GlobalFingerprint -> Hash
globalHash global =
    List.foldl'
        (\acc (Hash part) -> combineHash acc part)
        (hashText "investment-checkpoint-global-v1")
        [ global.files
        , global.parseOptions
        , global.commodityStyles
        , global.accountTypes
        , global.configVersion
        ]

-- | Every month in the recent past is checkpointed, so an append resumes at the previous month.
denseTailMonths :: Int
denseTailMonths = 24

{- | Beyond the dense tail, one month in twelve. A resume from the sparse region
re-folds at most a year of transactions, and retained state is O(years) rather
than O(months).
-}
sparseCheckpointInterval :: Int
sparseCheckpointInterval = 12

{- | The months worth checkpointing, given the chain's month ends oldest first:
every month in the last 'denseTailMonths', plus every
'sparseCheckpointInterval'-th month before that, counted from the oldest month so
the choice is stable as history grows at the near end.
-}
retainedMonths :: [Day] -> [Day]
retainedMonths months =
    [ day
    | (index, day) <- zip [0 :: Int ..] months
    , index >= total - denseTailMonths || index `mod` sparseCheckpointInterval == 0
    ]
  where
    total = length months

retainsMonth :: PrefixChain -> Day -> Bool
retainsMonth chain day = Set.member day (retainedSet chain)

retainedSet :: PrefixChain -> Set.Set Day
retainedSet chain = Set.fromList (retainedMonths (fmap fst chain.months))

{- | 'scopeScansAt' resumed from the newest usable checkpoint.

Observationally equal to 'scopeScansAt' for the same context, scope and days.

'scopeScansAt' consumes a transaction before emitting a day exactly when
@tdate <= day@, so the scan at day @d@ is the fold over @{t : t.tdate <= d}@ and
a transaction dated on a month end belongs to that month. A checkpoint stored at
@m@ is, by that same rule, the fold over @{t : t.tdate <= m}@.

The resume then holds because 'context.sortedTransactions' is date-sorted, so
@dropWhile ((<= m) . tdate)@ splits it precisely at that boundary: nothing dated
at or before @m@ is replayed (no double counting) and nothing dated after @m@ is
dropped (no skipping). @m@ is chosen at or before the earliest requested day, so
every emitted day still sees the same transactions in the same order as a fold
from 'initialAccumulator' would. What licenses reusing the state at all is the
hash: 'checkpointHashAt' matching attests that the prefix which produced it is
still the same prefix.

The returned map is the input map plus the checkpoints observed on this sweep.
-}
scopeScansResumed ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    ScopeCheckpoints ->
    Maybe Text ->
    [Day] ->
    ([ScopeScan], ScopeCheckpoints)
scopeScansResumed context chain checkpoints scope days =
    case List.sortOn snd (zip [0 :: Int ..] days) of
        [] -> ([], checkpoints)
        indexedDays@((_, earliest) : _) ->
            let resume = latestReusableCheckpoint context chain checkpoints scope earliest
                (seedAccumulator, seedTransactions) = case resume of
                    Nothing -> (initialAccumulator, context.sortedTransactions)
                    Just (at, checkpoint) ->
                        (checkpoint.accumulator, dropWhile ((<= at) . (.tdate)) context.sortedTransactions)
                -- The emitted view is a function of the accumulator alone, and the key
                -- pins the accumulator, so a checkpoint landing exactly on the earliest
                -- requested day also saves that day's O(flows) reverse.
                seedScan = case resume of
                    Just (at, checkpoint) | at == earliest -> Just checkpoint.scan
                    _ -> Nothing
                (indexedScans, fresh) =
                    walk context chain scope seedScan earliest seedAccumulator seedTransactions indexedDays
             in (fmap snd (List.sortOn fst indexedScans), insertCheckpoints fresh checkpoints)

insertCheckpoints :: [(CheckpointKey, ScopeCheckpoint)] -> ScopeCheckpoints -> ScopeCheckpoints
insertCheckpoints fresh (ScopeCheckpoints stored) =
    ScopeCheckpoints (List.foldl' (\acc (key, checkpoint) -> Map.insert key checkpoint acc) stored fresh)

{- | The chronological sweep of 'scopeScansAt', seeded from a checkpoint and also
reporting the checkpoints the retention policy wants from the days it emits.
-}
walk ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    Maybe Text ->
    Maybe ScopeScan ->
    Day ->
    Accumulator ->
    [Hledger.Transaction] ->
    [(Int, Day)] ->
    ([(Int, ScopeScan)], [(CheckpointKey, ScopeCheckpoint)])
walk context chain scope seedScan seedDay = go
  where
    plan = scopePlan context scope
    retained = retainedSet chain

    snapshotFor day accumulator = case seedScan of
        Just scan | day == seedDay -> scan
        _ -> snapshotAccumulator context accumulator

    checkpointFor day accumulator scan = case checkpointHashAt chain day of
        Just hash
            | Set.member day retained ->
                [
                    ( CheckpointKey
                        { scope = scope
                        , commodity = context.commodity
                        , monthEnd = day
                        , chainHash = hash
                        }
                    , ScopeCheckpoint{accumulator = accumulator, scan = forcedScan scan}
                    )
                ]
        _ -> []

    emit accumulator remaining day index restPending =
        let scan = snapshotFor day accumulator
            (scans, checkpoints) = go accumulator remaining restPending
         in ((index, scan) : scans, checkpointFor day accumulator scan <> checkpoints)

    go _ _ [] = ([], [])
    go accumulator [] ((index, day) : restPending) = emit accumulator [] day index restPending
    go accumulator allTransactions@(transaction : restTransactions) pending@((index, day) : restPending)
        | transaction.tdate <= day =
            go (stepAccumulator context plan accumulator transaction) restTransactions pending
        | otherwise = emit accumulator allTransactions day index restPending

{- | Force every field of an emitted scan before it is stored.

'ScopeCheckpoint.scan' is strict, but a @!@ reaches only the 'ScopeScan'
constructor, and 'ScopeScan' itself has lazy fields. Without this the @reverse@
of the cash-flow list, the @Set.toList@ of the account set and the accumulated
'Decimal' sums would sit in the checkpoint as thunks and be paid by whichever
later request first reads it — the deferred-work failure the caching plan's risk
table calls out.
-}
forcedScan :: ScopeScan -> ScopeScan
forcedScan scan =
    forceAccounts `seq`
        scan.mappingComplete `seq`
            forceMaybe forceDecimal scan.retained `seq`
                forceMaybe forceTriple scan.attributed `seq`
                    forceMaybe forceFlows scan.cashFlows `seq`
                        scan
  where
    forceAccounts = List.foldl' (\() account -> Text.length account `seq` ()) () scan.accounts
    forceTriple (income, gain, fee) =
        forceDecimal income `seq` forceDecimal gain `seq` forceDecimal fee
    forceFlows flows =
        List.foldl'
            (\() flow -> cashFlowDate flow `seq` forceDecimal (cashFlowAmount flow))
            ()
            flows

forceMaybe :: (a -> ()) -> Maybe a -> ()
forceMaybe force = maybe () force

-- | 'seq' on a 'Decimal' stops at its constructor, leaving the mantissa a thunk.
forceDecimal :: Decimal -> ()
forceDecimal value = decimalPlaces value `seq` decimalMantissa value `seq` ()

{- | The newest checkpoint at or before a day whose stored chain hash still
matches. A checkpoint whose hash has moved on is not merely stale, it is a
different journal prefix, so it is skipped rather than used.
-}
latestReusableCheckpoint ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    ScopeCheckpoints ->
    Maybe Text ->
    Day ->
    Maybe (Day, ScopeCheckpoint)
latestReusableCheckpoint context chain (ScopeCheckpoints stored) scope earliest =
    List.foldl' keepLater Nothing candidates
  where
    candidates =
        [ (key.monthEnd, checkpoint)
        | (key, checkpoint) <- Map.toAscList stored
        , key.scope == scope
        , key.commodity == context.commodity
        , key.monthEnd <= earliest
        , checkpointHashAt chain key.monthEnd == Just key.chainHash
        ]
    keepLater best candidate = case best of
        Just (bestDay, _) | bestDay >= fst candidate -> best
        _ -> Just candidate

{- | Checkpoints and solved rates for the investment family, carried together
because the resumed entry points update both.
-}
data ScopeCache = ScopeCache
    { checkpoints :: !ScopeCheckpoints
    , rates :: !SolvedRates
    }

emptyScopeCache :: ScopeCache
emptyScopeCache = ScopeCache{checkpoints = emptyCheckpoints, rates = emptySolvedRates}

truncateScopeCache :: PrefixChain -> ChainDiff -> ScopeCache -> ScopeCache
truncateScopeCache chain diff cache =
    ScopeCache
        { checkpoints = truncateCheckpoints chain diff cache.checkpoints
        , rates = truncateSolvedRates chain diff cache.rates
        }

{- | 'Hedger.Domain.Internal.InvestmentPerformance.summariesAtPrepared' with the
scope sweep resumed from a checkpoint and solved rates reused for unchanged
periods. Holdings still come from the two bulk reports; those are checkpointed
separately.
-}
summariesAtResumed ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    ScopeCache ->
    Maybe Text ->
    Granularity ->
    [Day] ->
    ([SummaryResult], ScopeCache)
summariesAtResumed context chain cache scope granularity days =
    (reverse reversedResults, ScopeCache{checkpoints = checkpoints, rates = rates})
  where
    (scans, checkpoints) = scopeScansResumed context chain cache.checkpoints scope days
    marketHoldings =
        holdingsSeriesAtWithJournal context.snapshot.journal context.assetClasses scope granularity days
    costHoldings =
        holdingsSeriesAtWithJournal
            context.historicalCostJournal
            context.assetClasses
            scope
            granularity
            days
    (reversedResults, rates) =
        List.foldl' step ([], cache.rates) (List.zip4 days marketHoldings costHoldings scans)
    step (results, carried) parts =
        let (result, carried') = summaryWithRates context chain scope carried parts
         in (result : results, carried')

{- | 'Hedger.Domain.Internal.InvestmentPerformance.summariesAtScopesPrepared'
with each scope's sweep resumed from its own checkpoint. The shared single
traversal it replaces still folded the whole journal once per request; resuming
per scope folds only the tail, which is the smaller cost even though the
traversal is no longer shared.
-}
summariesAtScopesResumed ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    ScopeCache ->
    Day ->
    [Maybe Text] ->
    ([(Maybe Text, SummaryResult)], ScopeCache)
summariesAtScopesResumed _ _ cache _ [] = ([], cache)
summariesAtScopesResumed context chain cache day scopes =
    (reverse reversedResults, cache')
  where
    marketHoldings =
        holdingsForScopesAtWithJournal context.snapshot.journal context.assetClasses scopes day
    costHoldings =
        holdingsForScopesAtWithJournal context.historicalCostJournal context.assetClasses scopes day
    (reversedResults, cache') =
        List.foldl' scopeStep ([], cache) (zip3 scopes marketHoldings costHoldings)
    scopeStep (results, carried) (scope, holdings, cost) =
        let (scans, checkpoints) = scopeScansResumed context chain carried.checkpoints scope [day]
            scan = case scans of
                [only] -> only
                _ -> error "scopeScansResumed: expected exactly one ScopeScan for one requested day"
            (result, rates) = summaryWithRates context chain scope carried.rates (day, holdings, cost, scan)
         in ( (scope, result) : results
            , ScopeCache{checkpoints = checkpoints, rates = rates}
            )

{- | One day's summary, taking its annualised return from the rate cache when the
period's prefix is unchanged and contributing the solved outcome back.
-}
summaryWithRates ::
    InvestmentPerformanceContext ->
    PrefixChain ->
    Maybe Text ->
    SolvedRates ->
    (Day, Hledger.MixedAmount, Hledger.MixedAmount, ScopeScan) ->
    (SummaryResult, SolvedRates)
summaryWithRates context chain scope rates (day, holdings, cost, scan) =
    (result, updatedRates)
  where
    key hash = RateKey{scope = scope, commodity = context.commodity, at = day, chainHash = hash}
    hashAt = checkpointHashAt chain day
    cached = do
        hash <- hashAt
        lookupSolvedRate (key hash) rates
    solver = case cached of
        Just outcome -> cachedRate outcome
        Nothing -> xirrFromCashFlows
    (result, used) = summaryFromHoldingsUsing solver context scope day holdings cost scan
    updatedRates = case (hashAt, used) of
        (Just hash, Just outcome) -> insertSolvedRate (key hash) outcome rates
        _ -> rates

{- | A solver that answers from a cached outcome while keeping
'xirrFromCashFlows'' guards: a day with no cash flows or no market value has no
rate at all, cached or not.
-}
cachedRate ::
    Either Xirr.XirrError Decimal ->
    Maybe [Xirr.CashFlow] ->
    Day ->
    Maybe Decimal ->
    Maybe (Either Xirr.XirrError Decimal)
cachedRate outcome flows _ terminalMarket = do
    _ <- flows
    _ <- terminalMarket
    pure outcome
