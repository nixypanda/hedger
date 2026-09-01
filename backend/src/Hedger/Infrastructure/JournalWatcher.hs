{- | Automatic journal reloading.

The watcher listens for filesystem events (via @fsnotify@: FSEvents on macOS,
inotify on Linux) on every file that made up the current snapshot — the root
journal plus any files it @include@s — and asks the 'SnapshotStore' to reload
when one of them changes. It is event-driven, so an idle server does no work.

Three cases need care and are handled here:

  * Editors often save atomically (write a temporary file, then rename over the
    original). The membership check is on the final path, and any event that
    replaces a watched file wakes the reloader.

  * @include@ sets grow and shrink between reloads. After every reload the watch
    set is re-derived from the fresh snapshot and the watched directories are
    reconciled, so newly included files start being watched and removed ones
    stop.

  * A single save can emit several events. A short debounce coalesces a burst
    into one reload.

@fsnotify@ watches directories, not individual files, so we watch each distinct
parent directory and filter events down to the exact set of journal files.

After each reload this module also rebuilds the derived analytics view: it
truncates the checkpoint caches by how far the prefix chain diverged and warms
the view the dashboard opens on, so an append costs the months it touched rather
than the journal's whole history.

Two of the checkpoint modules are reached through @Domain.Internal@ because
"Hedger.Domain.NetWorth" and "Hedger.Domain.Cashflow" re-export only their
resumed entry points, not the cache types those entry points take and return.
"Hedger.Domain.InvestmentPerformance" does re-export its cache, and is used
through the public module for that reason.
-}
module Hedger.Infrastructure.JournalWatcher (
    DerivedTargets (..),
    guardReloaderStep,
    rebuildAfterReload,
    watchTargets,
    withJournalWatcher,
)
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (
    SomeAsyncException,
    SomeException,
    displayException,
    evaluate,
    finally,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forever, when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory qualified as Directory
import System.FSNotify (
    Event,
    WatchManager,
    eventPath,
    watchDir,
    withManager,
 )
import System.FilePath (takeDirectory)

import Hedger.Config (Config (..))
import Hedger.Domain.Fingerprint (
    ChainDiff (DivergesFrom, GlobalChanged, Identical),
    PrefixChain (..),
    diffChains,
    globalFingerprint,
    prefixChain,
 )

import Hedger.Domain.Cashflow (computeCashflowResumed)
import Hedger.Domain.Internal.CashflowCheckpoint (columnCount)
import Hedger.Domain.Internal.InvestmentCheckpoint (checkpointCount, solvedRateCount)
import Hedger.Domain.Internal.NetWorthCheckpoint (QueryKey (..), openingBalanceEntries)
import Hedger.Domain.InvestmentPerformance (
    ScopeCache (..),
    investmentDataTransactions,
    prepareInvestmentPerformance,
    reportPeriods,
    summariesAtResumed,
 )
import Hedger.Domain.NetWorth (computeNetWorthResumed)
import Hedger.Domain.Period (Granularity (Monthly), epoch)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches (..),
    DerivedState,
    DerivedView (..),
    RebuildOutcome (..),
    currentGeneration,
    readDerivedView,
    rebuildDerived,
    truncateDerivedCaches,
 )
import Hedger.Infrastructure.DerivedStore (DerivedStore, clearStore, invalidateOtherRevisions)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    SnapshotStore,
    readCurrentSnapshot,
    reloadSnapshot,
 )

import Hledger.Data.Types qualified as Hledger

{- | How long to wait after the first event of a burst before reloading, so a
single save that emits several events triggers just one reload.
-}
debounceMicroseconds :: Int
debounceMicroseconds = 100_000

{- | Everything the derived (cached analytics) rebuild needs, grouped so the
reload loop keeps a readable parameter list.
-}
data DerivedTargets = DerivedTargets
    { derivedState :: !DerivedState
    , derivedStore :: !DerivedStore
    , config :: !Config
    }

withJournalWatcher :: SnapshotStore -> DerivedTargets -> Text -> IO a -> IO a
withJournalWatcher store derived journalPath action =
    withManager $ \manager -> do
        watchedDirs <- newIORef Map.empty
        targets <- newIORef Set.empty
        wakeReloader <- newTVarIO False
        let requestReload = atomically (writeTVar wakeReloader True)
        reconcileWatches manager store journalPath watchedDirs targets requestReload
        reloader <-
            forkIO
                ( runReloader
                    store
                    derived
                    journalPath
                    manager
                    watchedDirs
                    targets
                    requestReload
                    wakeReloader
                )
        action `finally` killThread reloader

{- | Runs 'reloadCycle' forever, guarding each iteration with 'guardReloaderStep'
so a synchronous exception (e.g. a watched path vanishing mid-reconcile during
an atomic-rename storm) is logged and the loop keeps going instead of the
reloader thread dying silently.
-}
runReloader ::
    SnapshotStore ->
    DerivedTargets ->
    Text ->
    WatchManager ->
    IORef (Map FilePath (IO ())) ->
    IORef (Set FilePath) ->
    IO () ->
    TVar Bool ->
    IO ()
runReloader store derived journalPath manager watchedDirs targets requestReload wakeReloader =
    forever (guardReloaderStep step)
  where
    step =
        reloadCycle store derived journalPath manager watchedDirs targets requestReload wakeReloader

reloadCycle ::
    SnapshotStore ->
    DerivedTargets ->
    Text ->
    WatchManager ->
    IORef (Map FilePath (IO ())) ->
    IORef (Set FilePath) ->
    IO () ->
    TVar Bool ->
    IO ()
reloadCycle store derived journalPath manager watchedDirs targets requestReload wakeReloader = do
    atomically (readTVar wakeReloader >>= check)
    threadDelay debounceMicroseconds
    -- Clear the flag so events coalesced during the debounce window are
    -- covered by the reload below; any that arrive afterwards set it again
    -- and drive a fresh cycle, so no change is missed.
    atomically (writeTVar wakeReloader False)
    result <- reloadSnapshot store journalPath
    Text.putStrLn (reloadMessage result)
    -- The snapshot is already published at this point, so transactions and
    -- accounts are fresh while the derived view below is still catching up.
    traverse_ (rebuildAfterReload derived wakeReloader) result
    reconcileWatches manager store journalPath watchedDirs targets requestReload

{- | Recompute the chain, truncate the checkpoint caches by how far it diverged,
warm the common view against what survived, and publish all of it with the new
revision in one step. Truncating rather than discarding is what makes an append
cost one month instead of the journal's whole history.
-}
rebuildAfterReload :: DerivedTargets -> TVar Bool -> Snapshot -> IO ()
rebuildAfterReload derived wakeReloader snapshot = do
    outcome <- rebuildDerived derived.derivedState (rebuildSuperseded derived wakeReloader) compute
    Text.putStrLn (rebuildMessage snapshot outcome)
  where
    compute = do
        let fingerprint = globalFingerprint snapshot.journal derived.config
            chain = prefixChain fingerprint snapshot.transactions snapshot.journal.jpricedirectives
        previous <- readDerivedView derived.derivedState
        let difference = flip diffChains chain <$> previous.chain
        invalidateResponses derived snapshot.revision difference
        forceChain chain
        let retained = truncateDerivedCaches chain difference previous.caches
        caches <- forceCaches (warmDefaultView derived snapshot chain retained)
        pure (snapshot.revision, chain, caches)

{- | The response store has no notion of a partially valid entry, so a
divergence anywhere drops every response not keyed to the new revision. The
checkpoint caches above are the ones that survive a divergence.
-}
invalidateResponses :: DerivedTargets -> Text -> Maybe ChainDiff -> IO ()
invalidateResponses derived revision difference = case difference of
    Nothing -> clearStore derived.derivedStore
    Just GlobalChanged -> clearStore derived.derivedStore
    Just (DivergesFrom _day) -> invalidateOtherRevisions derived.derivedStore revision
    Just Identical -> pure ()

{- | Resume the view the dashboard opens on — default display commodity, root
scope, no query, monthly — so the checkpoints the next request wants already
exist. Without this the first request after every edit still pays full cost,
which is the entire point of rebuilding eagerly.

Monthly only. A yearly net-worth or cashflow request spans all history, so the
month it would open from precedes the journal and has no chain entry to key a
checkpoint on; warming it would cost a full pass and cache nothing.

Measured on @examples/performance/india-us-portfolio-large-50000.journal@
(50,000 transactions, 1,036 months): the warm costs about 1.4 s against empty
caches and about 0.15 s once they are retained. The 'forceChain' pass in the
same rebuild used to dominate both at ~15.5 s; its encoding has since been
rewritten and it is no longer the leading term. Note the shape though: resuming
from an /empty/ cache is
dearer than not resuming at all (1.2 s against 0.375 s for @computeNetWorth@),
because the delta report and the checkpoint stores are paid without a prefix to
skip. A 'GlobalChanged' edit therefore pays a little extra here to make every
subsequent edit cheap.
-}
warmDefaultView :: DerivedTargets -> Snapshot -> PrefixChain -> DerivedCaches -> DerivedCaches
warmDefaultView derived snapshot chain caches =
    DerivedCaches{scopes = warmScopes, netWorth = warmNetWorth, cashflow = warmCashflow}
  where
    commodity = derived.config.defaultDisplayCommodity
    latest = fromMaybe epoch snapshot.latestJournalDate
    -- The unfiltered request: an empty query matches every transaction, so the
    -- canonical query text is empty and the matched set is the snapshot's own.
    matched = snapshot.transactions
    queryKey = QueryKey{text = "", reference = latest}
    (_, warmNetWorth) =
        computeNetWorthResumed
            snapshot
            matched
            queryKey
            chain
            caches.netWorth
            Monthly
            commodity
            Nothing
            Nothing
            defaultDepth
            defaultDepth
    (_, warmCashflow) =
        computeCashflowResumed
            snapshot
            matched
            chain
            caches.cashflow
            ""
            Monthly
            commodity
            Nothing
            Nothing
            defaultDepth
            defaultDepth
    (_, warmScopes) =
        summariesAtResumed
            (prepareInvestmentPerformance snapshot assetClasses derived.config.investmentMappings commodity)
            chain
            caches.scopes
            rootScope
            Monthly
            periodDays
    assetClasses = derived.config.assetClassMappings
    rootScope = Nothing
    periodDays =
        nub [end | (_, _, end) <- reportPeriods Monthly dataTransactions latest]
    dataTransactions = investmentDataTransactions snapshot assetClasses rootScope

{- | The depth every analytics endpoint applies when the request omits one. Depth
is a projection over unclipped cached rows, so it does not affect what is
cached; it is here only because the resumed entry points take it.
-}
defaultDepth :: Int
defaultDepth = 2

{- | Forcing the rolling hashes here keeps the chain pass inside the rebuild's
measured window instead of deferring it into whichever request first inspects
the chain.

This is where a rebuild's time actually goes, and by a wide margin: ~15.5 s of
the ~17 s rebuild on the 50,000-transaction fixture, against ~7 s to parse the
journal and ~1.4 s for every checkpoint the warm below computes. Whatever the
checkpoints save a request, a reload of a large journal cannot currently get
below the cost of hashing its history.
-}
forceChain :: PrefixChain -> IO ()
forceChain chain = traverse_ (evaluate . snd) chain.months

{- | The same discipline for the checkpoint caches. A lazy cache field would
defer the fold that produced it into the first request that reads it, which is
exactly the cold-latency failure the eager rebuild exists to prevent — and a
@!@ on a record field only reaches the constructor, so forcing the box is not
by itself enough.

Here it is enough, and it is worth writing down why, because it rests on two
facts outside this module. Every checkpoint module forces its payloads deeply
before storing them, so no stored value is a thunk. And both ways a cache map
is built preserve that under a single 'evaluate': truncation goes through
'Data.Map.Strict.filterWithKey', whose recursion binds both subtrees with a bang
pattern, so weak-head evaluation of the result walks the entire spine; and the
resumed calculations extend their maps with strict inserts under a strict fold.
'openingBalanceEntries' additionally folds the per-account maps, which is where
this family's bytes are and the one place a spine walk alone would not reach.
-}
forceCaches :: DerivedCaches -> IO DerivedCaches
forceCaches caches = do
    _ <- evaluate (checkpointCount caches.scopes.checkpoints)
    _ <- evaluate (solvedRateCount caches.scopes.rates)
    _ <- evaluate (openingBalanceEntries caches.netWorth)
    _ <- evaluate (columnCount caches.cashflow)
    evaluate caches

{- | The coalescing requirement: a five-file save must drive one rebuild, not
five sequential ones. A pending wake means the snapshot this rebuild was
computed from is already superseded; a generation past our token means another
rebuild has claimed the state.
-}
rebuildSuperseded :: DerivedTargets -> TVar Bool -> Int -> IO Bool
rebuildSuperseded derived wakeReloader token = do
    pending <- readTVarIO wakeReloader
    generation <- currentGeneration derived.derivedState
    pure (pending || generation /= token)

{- | Runs an action, catching and logging a synchronous exception so the
caller (the reloader's @forever@ loop) can continue instead of dying.
Asynchronous exceptions — notably the @ThreadKilled@ that 'withJournalWatcher'
delivers via @killThread@ to stop the watcher — are rethrown untouched so the
reloader thread stays killable.
-}
guardReloaderStep :: IO () -> IO ()
guardReloaderStep action = do
    outcome <- try action
    case outcome of
        Right () -> pure ()
        Left exception
            | isAsynchronous exception -> throwIO exception
            | otherwise ->
                Text.putStrLn
                    ("Journal watcher reconcile failed; retrying: " <> Text.pack (displayException exception))

isAsynchronous :: SomeException -> Bool
isAsynchronous exception =
    case fromException exception :: Maybe SomeAsyncException of
        Just _ -> True
        Nothing -> False

reloadMessage :: Either SnapshotLoadError Snapshot -> Text
reloadMessage result = case result of
    Right snapshot ->
        "Reloaded journal after a file change (revision " <> snapshot.revision <> ")."
    Left (JournalReadFailed problem) ->
        "Journal reload after a file change failed; keeping the previous snapshot: " <> problem

rebuildMessage :: Snapshot -> RebuildOutcome -> Text
rebuildMessage snapshot outcome = case outcome of
    RebuildCompleted ->
        "Rebuilt derived analytics data (derived revision " <> snapshot.revision <> ")."
    RebuildAbandoned ->
        "Abandoned the derived analytics rebuild for revision "
            <> snapshot.revision
            <> "; a newer journal change superseded it."
    RebuildFailed problem ->
        "Derived analytics rebuild failed; keeping the previous derived data: " <> problem

reconcileWatches ::
    WatchManager ->
    SnapshotStore ->
    Text ->
    IORef (Map FilePath (IO ())) ->
    IORef (Set FilePath) ->
    IO () ->
    IO ()
reconcileWatches manager store journalPath watchedDirs targets requestReload = do
    files <- watchTargets store journalPath
    writeIORef targets (Set.fromList files)
    let desiredDirs = Set.fromList (takeDirectory <$> files)
    existing <- readIORef watchedDirs
    let toStop = Map.withoutKeys existing desiredDirs
        keptDirs = Map.restrictKeys existing desiredDirs
        toStart = Set.difference desiredDirs (Map.keysSet existing)
    sequence_ (Map.elems toStop)
    started <-
        traverse
            (\dir -> (,) dir <$> watchDir manager dir (const True) (handleEvent targets requestReload))
            (Set.toList toStart)
    writeIORef watchedDirs (Map.union keptDirs (Map.fromList started))

handleEvent :: IORef (Set FilePath) -> IO () -> Event -> IO ()
handleEvent targets requestReload event = do
    watched <- readIORef targets
    when (eventPath event `Set.member` watched) requestReload

-- The root path is always included so a journal that currently fails to parse
-- is still watched and picked up once it parses again.
watchTargets :: SnapshotStore -> Text -> IO [FilePath]
watchTargets store journalPath = do
    snapshot <- readCurrentSnapshot store
    let included = maybe [] (fmap fst . (.jfiles) . (.journal)) snapshot
    root <- canonicalize (Text.unpack journalPath)
    absoluteIncluded <- traverse canonicalize included
    pure (nub (root : absoluteIncluded))

{- | Resolve to a canonical path so event paths (which the OS reports resolved)
compare equal to our target set even when the journal lives under a symlink.
-}
canonicalize :: FilePath -> IO FilePath
canonicalize = Directory.canonicalizePath
