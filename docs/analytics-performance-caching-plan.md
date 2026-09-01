# Analytics caching plan: checkpointed folds over an immutable history

_Status: S0–S5 implemented and green; not yet reachable from a request · Last
updated: 2026-09-01_

Part of the [analytics performance index](analytics-performance-plan.md). This is
the only remaining optimization phase. Prerequisites are complete: see
[flow optimization](analytics-performance-flow-optimization.md) for the measured
baseline and [rejected approaches](analytics-performance-rejected.md) for the
constraints that shape this design.

This document is written to be implementable by someone who has not read the
other two. It states the idea, the invariants that make it correct, the data
shapes, and an ordered checklist of slices. Every slice is independently
shippable and independently verifiable.

## Implementation status

| Slice | State | Notes |
|---|---|---|
| S0a `DerivedStore` | **Done** | Single-flight, byte-budgeted, revision-ordered eviction |
| S0b response cache | **Done** | Five endpoints wired; metadata patched on every hit |
| S1 `Fingerprint` | **Done** | Three plan corrections came out of it, recorded below |
| S2 derived revision | **Done** | Rebuild in `reloadCycle`, at boot, and on `SIGHUP`; `refreshing` freshness |
| S3 investment scope scan | **Done** | Pure. Three defects found and mutation-verified |
| S4 net worth | **Done** | Pure. Moves accumulation out of hledger; five corrections, two mutation-verified defects |
| S5 cashflow columns | **Done** | Pure. Refuted the `missing`-set half of yearly-from-monthly |
| S6 scoped invalidation | Not started | |
| S7 wire checkpoints to the request path | **Done** | Caches in `DerivedState`; all three use cases resume and merge back |
| S8 chain-pass encoding | **Done** | 5.7x faster, 10x less allocated; had been the dominant reload cost |

The checkpoint caches live in `DerivedState`, not in the LRU `DerivedStore`:
`lookupOrCompute` is a *memoizing* API with no put operation, so a checkpoint map
a request updates cannot be written back through it. `DerivedState` is rebuilt by
the reload cycle and already holds the chain, which is where chain-hash-keyed
caches belong; the LRU store keeps serving whole responses. Request write-back is
lock-free because checkpoint maps are monotone under a fixed chain — two requests
computing the same key compute the same value — with a revision guard to stop a
request resurrecting checkpoints for a chain that has since been truncated.

**Still outstanding: no end-to-end latency measurement.** Every figure in this
document is a component or benchmark-harness number. Nobody has yet measured a
cold request, a warm request and a post-append request against the running
server, which is the only measurement that answers whether this work achieved its
goal. Do that before declaring the phase complete.

## The idea

Every expensive computation in the analytics path is a **left fold over
date-sorted journal entries, snapshotted at period ends**:

- `scopeScansAt` (`Domain/Internal/InvestmentPerformance.hs:557`) is literally a
  strict left fold: `walk` threads an `Accumulator` over
  `context.sortedTransactions` and calls `snapshotAccumulator` when it passes a
  requested day. `Accumulator` is a fully strict record of five fields with no
  lookahead.
- Net worth's `Historical` accumulation is a running sum from the start of the
  journal, with `AtEnd` valuation applied to each accumulated column.
- XIRR consumes a prefix of external cash flows, one prefix per period end.

A fold snapshot at month *k* is a function of the journal prefix up to month *k*
and nothing else. So **historical snapshots are immutable under append** — of
transactions *and* of price directives. The ordinary edit to a personal journal
is an append. Today every such edit throws away all 601 months of derived work
and recomputes from scratch.

The design has three parts:

1. **A prefix hash chain.** One O(n) pass over date-sorted entries produces a
   rolling hash snapshotted at each month end — a Merkle chain over journal
   history. Cheap relative to the parse that already happened — but only after
   S8; see "The chain pass was the dominant reload cost" below. As first
   implemented this claim was false by a wide margin, and it went unnoticed for
   four slices because nothing measured the rebuild end to end.
2. **Checkpoints.** Store the fold state and the derived columns at month ends,
   keyed by the chain hash at that month.
3. **Tail rebuild.** On reload, compare the new chain against the old one
   element-wise, find the first divergent month, discard checkpoints from there,
   and resume each fold from the last surviving checkpoint.

An append diverges only at the final month, so the rebuild recomputes **one
month instead of 601**. Cost becomes proportional to how far back the edit
reached, not to journal size. Because the key is a content hash, an undo or a
revert *hits* the cache rather than invalidating it.

There is no distributed system here. One process, one journal, one reload thread
already serialized by `reloadCycle` in `Infrastructure/JournalWatcher.hs`. A
generation counter and an `IORef` swap of an immutable derived value are
sufficient; no locking beyond the existing `updateLock`, no coherence protocol,
no cache stampede between nodes.

## Correctness: what makes a historical checkpoint reusable

This is the part to get right. Everything else is mechanical.

### Append-stability holds because

- **Fold state is backward-looking.** `stepAccumulator` inspects only the
  transaction it is given and the accumulator so far. Account discovery,
  explicit-cost status, retained performance, attribution and external cash flows
  are all cumulative-to-date.
- **`AtEnd` valuation at month *k* consults only prices dated at or before month
  *k*.** hledger's price oracle resolves the most recent price at or before the
  valuation date, so a price appended today cannot change a 2019 column. Note
  precisely what this rests on: `InvestmentPerformanceContext.valueAt` closes
  over a `journalPriceOracle` built from the *entire* journal, so append
  stability depends wholly on `mixedAmountValueAtDate` never consulting a price
  dated after its query date. That is hledger's documented behaviour, but it is
  an assumption about a library rather than a property this code can enforce. If
  it ever fails, every checkpointing slice fails silently together.
- **Price directives after `latestJournalDate` are already truncated from the
  report journal** by the completed window-alignment work. Do not remove that;
  this design depends on it.

### Append-stability fails, and must trigger invalidation, when

| Change | Invalidates | Handling |
|---|---|---|
| Transaction added, edited or removed with date *d* | all months ≥ *d* | Per-period chain: hash transactions into the period containing their date |
| Price directive added, edited or removed with date *d* | all months ≥ *d* | **Hash price directives into the chain by directive date.** Missing this yields silently stale historical valuations — the worst available failure mode |
| Transaction cost changed (infers a market price at its date) | all months ≥ *d* | Covered, because posting costs are part of the transaction content hash |
| Commodity display-style inference changes | **everything** | Journal-global and not date-scoped. Fold a hash of resolved styles into the *global* fingerprint |
| Account-type declarations change | **everything** | Same: global fingerprint |
| Journal file set or parse options change | **everything** | Same: global fingerprint |
| Config change (investment/asset-class mappings, fiscal year, default commodity) | **everything** for the affected family | Config version in the global fingerprint |

### Three corrections established while implementing S1

1. **`files` must hash paths, not contents.** An earlier revision of this
   document said `(path, content) pairs`. That is wrong and self-defeating:
   hashing contents makes every whitespace reformat a `GlobalChanged`, which
   discards all of history and directly contradicts the most important test in
   the slice. Hash `jfiles` paths in order. File *contents* are already covered
   semantically — by the per-month transaction and price hashes, and by the
   style and account-type components.
2. **Hash `jdeclaredaccounttypes`, never `jaccounttypes`.** The latter also
   contains types *inferred* from account names and parents, so it grows
   whenever an append introduces a new account — hashing it would turn ordinary
   appends into full rebuilds. The inferred part is a pure function of account
   names, which already live inside the transaction hashes.
3. **`jtxnmodifiers` and `jperiodictxns` are journal-global and are NOT in the
   fingerprint.** Auto-posting rules and periodic transactions are currently
   inert because `definputopts` has `auto` and `forecast` off, so this is not a
   live bug. But if either option is ever enabled, editing an auto-posting rule
   would silently restate all history with no invalidation. Enabling either
   option **must** be accompanied by adding them to `GlobalFingerprint` and
   bumping `parseOptions`.

Also confirmed against hledger 1.52: commodity styles are genuinely global
(`journalCommodityStyles` composes global, declared, `D`-directive default and
inferred styles with no date scoping), so a `D`-directive change correctly lands
in `commodityStyles` and needs no separate handling.

The rule: **anything the journal derives globally rather than by date goes in a
global fingerprint that invalidates the whole chain.** Styles and account-type
declarations rarely change once a journal is established, so this costs almost no
hit rate and removes a class of subtle bugs entirely.

### Two implementation traps

0. **The local hledger checkout is the wrong version to read.**
   `/Users/nixypanda/Documents/open-source/hledger` is hledger **1.99**; this
   build resolves **hledger-lib 1.52**, and field names differ (1.52 has
   `ptype :: PostingType` where 1.99 has `preal :: PostingRealness`). Read field
   names from the 1.52 tarball in the nix store, not from that checkout.
1. **Do not hash `tsourcepos` or `tindex`.** They shift when unrelated lines move
   in the file, so hashing them would make inserting a comment at the top of a
   journal invalidate all of history. Hash semantic content only: date, date2,
   status, code, description, comment, tags, and per posting the account, amount
   (commodity and quantity), cost, balance assertion, status and comment. Write a
   test that reformats whitespace and asserts the chain is unchanged.
2. **Do not retain hledger intermediates in a checkpoint.** The reverted
   shared-window experiment measured a *regression* from hoisting a filtered
   journal across two report passes — retaining a large intermediate defeats
   fusion and prompt collection, and it does not show up in allocation figures.
   Checkpoints must hold `Decimal`, `Set Text`, `Map AccountName MixedAmount` and
   small strict records. Never a `MultiBalanceReport`, never a `Journal`, never a
   closure over either.

### The chain pass was the dominant reload cost

Found by S7a, which was the first slice to time `rebuildAfterReload` end to end,
and fixed by S8. Worth reading as a process finding as much as a technical one:
the chain had been the single most expensive thing in a reload since S1, and four
slices were built on top of it without anyone noticing, because every benchmark
measured a *report* and none measured a *rebuild*.

**It was linear, not quadratic** — 9.44× time for 10× the data, so the per-month
fold was genuinely one pass and the chain *design* was never at fault. Only its
encoding was. The `benchmark/Main.hs` figures:

| Fixture | Before | After | Allocated before | Allocated after |
|---|---:|---:|---:|---:|
| 5,000 | 394 ms | **79.5 ms** | 1.3 GB | **139 MB** |
| 50,000 | 3.72 s | **649 ms** | 12 GB | **1.2 GB** |

In the same harness the 50,000 parse is 6.57 s and `analytics.networth` is
559 ms, so the chain went from **57% of the parse to 9.4%**, and from 6.7× the
net-worth report it exists to let us skip to **1.1×**.

**The cause was one bug wearing two hats.** Every level of nesting hashed its
children by rendering each digest to 32 hex characters and concatenating, so a
transaction cost roughly *fifteen* MD5-plus-hex round trips. Nothing outside the
module ever reads a sub-transaction digest, so postings, amounts and tags are now
encoded inline into one `Builder` and hashed once: one digest per transaction,
with hex surviving only on the month hashes and the five global components.

**Known floor, and why S8 stopped.** With temporary probes the residual cost is
uniform at ~150 bytes allocated per `Builder` primitive write, at ~127 writes per
transaction — it is *not* amounts, `normalizeDecimal`, or `encodeUtf8`. Replacing
per-node `[Builder]` lists with `<>` chains helped (176 → 138 MB at 5,000);
`{-# INLINE #-}` on the node encoders did nothing, so GHC is not fusing the
builder chain. Going further means cutting the write count or leaving `Builder`
for hand-rolled pokes into a pre-sized buffer: an estimated 3–6× on a 649 ms
term, bought with unsafe pointer code inside the module that decides whether
caches are valid. Declined at 9.4% of parse. If it is ever wanted, the
write-count reduction is the safe half.

**Hash values changed, which is fail-safe** — a changed fingerprint invalidates
and rebuilds. Entry and month tags moved to `-v2`; the global fingerprint's
encoding is deliberately byte-identical, so a reload after this change sees
`DivergesFrom` at the first month rather than a `GlobalChanged` storm. All five
semantic `FingerprintSpec` assertions passed unmodified, because every one
asserts a relationship rather than a literal digest — which is why the spec
survived a complete change of representation.

**One measurement caveat.** The same pre-fix code measured 3.72 s in the
benchmark harness against the ~15.5 s observed from the server. The figures above
are internally consistent but roughly 4× lower in absolute terms than server
reality, which is the bench-versus-server RTS discrepancy this project has
recorded before. Re-measure `rebuildAfterReload` in the server before quoting an
absolute reload number.

### Key a checkpoint on `checkpointHashAt`, never on `chainHashAt`

Established while implementing S3, after the bug had been written and caught.

`prefixChain` seeds its month chain from an absent-hash sentinel, **not** from
`chain.global`. The consequence is easy to miss and has no symptom: month hashes
are byte-identical across a commodity-style change, an account-type declaration
change, or a config change. A checkpoint validated against `chainHashAt` alone
therefore survives *exactly* the edits that `GlobalChanged` exists to catch, and
serves silently restated history.

`Domain.Internal.InvestmentCheckpoint.checkpointHashAt` extends the month hash
with a fold of the five `GlobalFingerprint` components, and is the only correct
thing to key a checkpoint on. **S4, S5 and every later checkpointing slice must
use it.** The chain deliberately keeps the two separable so `diffChains` can
still distinguish "global changed, discard everything" from "diverges at month
*m*, truncate from there" — seeding the chain from the global hash would collapse
that distinction into a first-month divergence.

Two related traps from the same slice:

- **Truncation must take a `ChainDiff`, not a `Maybe Day`.** A caller mapping
  `ChainDiff` to `Maybe Day` writes `GlobalChanged -> Nothing`, which reads as
  "no divergence date" and keeps everything.
- **A `!` on a record field only reaches the constructor.** `!ScopeScan` left
  `ScopeScan`'s own lazy fields — reversed cash-flow lists, `Set.toList`
  results, summed `Decimal`s — sitting in the checkpoint as thunks, deferring
  fold work into the first request that reads it. Force the payload, not just
  the box.

### Further exclusions and caveats established while implementing S1

- **The full exclusion list for transaction hashing** is wider than
  `tsourcepos`/`tindex`. Also exclude `tprecedingcomment` (shifts when unrelated
  comments move), `baposition`, `pdsourcepos`, `astyle` (global, per the
  correction above), `ptransaction` (**must** be excluded — it closes a cycle
  back to the transaction and the hash diverges otherwise), and `poriginal` (a
  parse artefact). Do *include* `acostbasis`: it is semantic lot data, and it is
  safe because it is inferred only from earlier transactions, which diverge
  first anyway.
- **Normalize `Decimal` quantities before hashing.** `Decimal`'s `Show` is finer
  grained than its `Eq`: `1.5` and `1.50` compare equal but show differently, so
  naive quantity hashing spuriously invalidates whenever hledger's style
  application shifts a precision.
- **A `!` on a list field forces only the cons cell.** `PrefixChain.months` is
  `![(Day, Hash)]`, so whoever publishes a chain must force it deeply before
  publication, or the hashing work is deferred into the first request that reads
  it — the exact failure the risk table warns about.
- **`globalFingerprint` taking a `Config` inverts the documented layering.**
  `Hedger.Domain.Fingerprint` ends up importing `Hedger.Config`, which sits at
  the composition root. It compiles (no cycle) but it is the one place this work
  violates the architecture in `docs/backend.md`. The fix is to pass a small
  `ConfigFingerprint` record of the four relevant fields, built by the caller,
  so Domain stays innermost. Do this when touching the module next.

## Data shapes

Sketches, not final signatures. Adjust names to fit the modules they land in.

```haskell
-- Invalidates the entire chain when any component changes.
data GlobalFingerprint = GlobalFingerprint
    { files          :: !Hash   -- jfiles PATHS in order, NOT contents (see below)
    , parseOptions   :: !Hash   -- "definputopts-v1", as SnapshotStore already does
    , commodityStyles:: !Hash   -- resolved display styles
    , accountTypes   :: !Hash   -- jdeclaredaccounttypes ONLY, never jaccounttypes
    , configVersion  :: !Hash   -- mappings, fiscal year, default commodity
    }

-- One entry per month end, oldest first. chainHash k = H(chainHash (k-1) <> periodContent k)
data PrefixChain = PrefixChain
    { global :: !GlobalFingerprint
    , months :: !(Vector (Day, Hash))
    }

-- Net worth, per (query, commodity). Columns are monthly; yearly is projected.
data NetWorthDerived = NetWorthDerived
    { openingBalances :: !(Map Day (Map Hledger.AccountName Hledger.MixedAmount))
      -- ^ Sparse: raw unvalued cumulative balances at retained checkpoint months.
    , assetColumns    :: !(Vector (Conversion Decimal))
    , liabilityColumns:: !(Vector (Conversion Decimal))
    , gainColumns     :: !(Vector (Conversion Decimal))
    , breakdownRows   :: !(Map Day (Map Hledger.AccountName Decimal))
      -- ^ Unclipped per-account rows; depth collapsing stays a projection.
    }

-- Investments, per (scope, commodity).
data ScopeDerived = ScopeDerived
    { checkpoints :: !(Map Day Accumulator)   -- already strict; see sharing note
    , scans       :: !(Map Day ScopeScan)
    , solvedRates :: !(Map Day (Either XirrFailure Decimal))
    }

data Derived = Derived
    { derivedRevision :: !Text
    , chain           :: !PrefixChain
    , netWorth        :: !(Map NetWorthKey NetWorthDerived)
    , scopes          :: !(Map ScopeKey ScopeDerived)
    , holdings        :: !(Map HoldingsKey HoldingsColumns)
    }
```

Notes that matter for memory:

- **`Accumulator` checkpoints share structure.** `cashFlowsTotal` is consed
  newest-first, so the list at month *k* has the list at month *k−1* as its tail;
  dense checkpoints cost O(n) in total, not O(n × months). `Set Text` fields are
  persistent maps with path copying, so they share most nodes too. Dense
  checkpointing is affordable; measure it rather than assuming.
- **`openingBalances` should be sparse.** Retain a checkpoint every 12 months plus
  every month in the last 24. Resume then re-folds at most ~12 months, and memory
  is O(accounts × years) instead of O(accounts × months). This is the "close the
  books" idea: a checkpoint *is* an opening balance.
- **Store monthly only.** Year ends are month ends, and `Historical` + `AtEnd` at a
  given date does not depend on the interval that produced it, so yearly columns
  are a selection of monthly columns. Cashflow is `PerPeriod` + `AtThen`, whose
  per-posting valuation makes yearly columns the *sum* of twelve monthly columns.
  Both need a pinned equivalence test before being trusted.

## Module placement

Per the layering rules in `docs/backend.md` (Interface → Application → Domain;
Infrastructure → Domain at the snapshot boundary only):

- `Hedger.Domain.Fingerprint` — pure hashing of transactions, price directives,
  styles, account types; `PrefixChain` construction and diffing.
- `Hedger.Domain.Derived` — pure checkpoint types, the resume functions, and the
  projections from checkpoints to read-models. No `AppM`, no `SnapshotMetadata`.
- `Hedger.Infrastructure.DerivedStore` — the mutable `IORef Derived`, generation
  counter, single-flight, bounded worker pool, eviction.
- `Hedger.Application.*` — unchanged signatures where possible; use cases read
  derived data instead of calling `computeNetWorth` / `summariesAtPrepared`
  directly, and fall back to full computation on a miss.

Domain functions stay pure and take plain values, so every one of them is
directly testable against the existing oracles.

## Invalidation pipeline

`reloadCycle` already gives a single-threaded, debounced reload loop with a 100 ms
window and a `wakeReloader` flag that is re-set by events arriving during a
rebuild. Extend it rather than adding a second scheduler.

```text
FSNotify event
  -> existing 100 ms debounce and coalesce (JournalWatcher)
  -> parse journal (existing reloadSnapshot)
  -> publish Snapshot immediately            (transactions/accounts stay fresh)
  -> compute GlobalFingerprint + PrefixChain (one O(n) pass)
  -> global changed?  yes -> discard all derived data
                      no  -> diff chain, find first divergent month
  -> discard checkpoints >= that month
  -> resume each fold from the last surviving checkpoint, newest generation only
  -> publish Derived atomically (IORef swap) and bump derivedRevision
```

Requirements on this pipeline:

- **Coalesce and cancel.** Take a generation token at the start of a rebuild.
  Check `wakeReloader` at each period boundary during the resume and abandon the
  rebuild if a newer event has arrived. Without this a five-file save triggers
  five sequential tail rebuilds.
- **Publish snapshot and derived data separately, but report which is which.**
  `Application/Common.hs` already emits `freshness = "fresh" | "stale"`, and the
  frontend's `Data.Freshness` already decodes `refreshing`. So a rebuild in
  flight can be surfaced with **no frontend change**: emit `refreshing` while the
  derived revision trails the snapshot revision.
- **Carry a derived revision in response metadata**, distinct from the snapshot
  revision. `Application/Events.hs` waits on `awaitRevisionChange`, and
  `SnapshotStore` writes the revision `TVar` the instant the snapshot is
  published — so without a separate derived revision, clients are told to refetch
  before the derived view exists, which is exactly the stampede to avoid.
- **Full-rebuild fallback.** If the chain is absent, the global fingerprint
  changed, or the diff is ambiguous, discard everything and recompute. Cheap
  insurance that reduces correctness to a single question: did we ever wrongly
  *keep* a checkpoint?
- **Failed reloads keep serving the last successful snapshot and its derived
  data**, as today. A failed derived rebuild must retain the last complete
  derived view and surface an operational error rather than publish partial data.

## Slices

Each slice is shippable on its own. Do them in order; the later ones assume the
earlier ones' verification harness.

### S0 — Whole-response cache (no incrementality)

Cheapest useful thing. Do it first so there is a working cache, a hit-rate
metric, and a memory budget in place before any incremental logic exists.

- [ ] Add `Hedger.Infrastructure.DerivedStore` with an `IORef`-held bounded map,
      keyed by `(snapshotRevision, requestKey)`, byte-budgeted with
      revision-ordered eviction (oldest revision first, not LRU-by-count —
      entries differ by orders of magnitude in size).
- [ ] Cache the fully projected `NetWorthReport`, `CashflowReport`,
      `InvestmentsReport`, `ActivityReport` and accounts-tree responses.
- [ ] Single-flight per key: concurrent misses on the same key await one
      computation. Required before anything else, because one SSE revision event
      makes every connected client refetch simultaneously.
- [ ] Bounded worker pool for misses and rebuilds. Capacity control only —
      parallel report execution is a measured regression, see rejected
      approaches.
- [ ] Metrics: hits, misses, evictions, single-flight waiters, bytes held, by
      report family.
- [ ] Benchmark cold miss, warm hit, and concurrent identical miss, one case at a
      time.

Expected at 50k: warm hit ~1–5 ms for every endpoint (JSON encoding only). Cold
paths unchanged. Ship this and read the hit-rate metric before building S3–S5 —
it tells you which families actually matter.

### S1 — Prefix chain and global fingerprint

No behaviour change. Build the machinery and prove it correct in isolation.

- [ ] `Hedger.Domain.Fingerprint`: semantic transaction hashing (**excluding
      `tsourcepos` and `tindex`**), price-directive hashing by date, style and
      account-type hashing, config-version hashing.
- [ ] Rolling per-month chain from date-sorted entries in one pass.
- [ ] Chain diff returning `Either GlobalChanged (Maybe FirstDivergentMonth)`.
- [ ] Tests: whitespace/comment reformatting leaves the chain unchanged;
      appending a transaction diverges only at the last month; inserting a
      historical transaction diverges at its month; **inserting a historical
      price directive diverges at its month**; changing a commodity's decimal
      places changes the global fingerprint.
- [ ] Benchmark the chain pass at 50k. It must be small next to the parse; if it
      is not, the whole design is mispriced.

### S2 — Eager rebuild and derived revision

Turn S0's cache into something that is warm *before* the first request.

- [ ] Separate `derivedRevision` in `DerivedStore`, published by `IORef` swap.
- [ ] Hook the rebuild into `reloadCycle` after `reloadSnapshot`.
- [ ] Generation token; abandon a rebuild when `wakeReloader` is set again.
- [ ] Emit `freshness = "refreshing"` while the derived revision trails the
      snapshot revision. No frontend change needed.
- [ ] Metrics: rebuild duration, revision, abandoned rebuilds, queue depth.
- [ ] Decide and document the eager set — start with the default display
      commodity, root scope, monthly.

Expected at 50k: the *cold* request becomes warm for eagerly built views, which
is the only way the 200 ms target is reachable. Rebuild cost is still a full
recompute (~5 s for investments) — S3–S5 fix that.

### S3 — Checkpoint the investment scope scan

The single biggest win: 1.20 s (timeline) + 1.64 s (drill-down) at 50k.

- [ ] Store `Accumulator` and `ScopeScan` per (scope, commodity, month end),
      keyed by chain hash.
- [ ] Resume `scopeScansAt` from the last surviving checkpoint instead of
      `initialAccumulator`. The fold already has the right shape; this is a
      change to its seed and its emitted days, not to `stepAccumulator`.
- [ ] Cache solved XIRR rates per checkpointed period. An append then re-solves
      one period instead of twenty-four (~1.0 s). Do **not** reopen the algebraic
      grid-sharing rejected on numerical-safety grounds.
- [ ] Measure retained memory for dense checkpoints; if it is too large, retain
      sparsely as in `openingBalances` and re-fold the gap.
- [ ] Equivalence test against the retained singular `summaryAtPrepared` oracle
      across all 125 investment-performance cases, comparing every summary field
      and warning.

### S4 — Checkpoint net worth

Highest correctness risk in the plan, because it moves accumulation out of
hledger. Gate it behind S3's harness.

- [x] Done, pure only — `Domain.Internal.NetWorthCheckpoint` plus
      `Domain.NetWorth.computeNetWorthResumed`, with `computeNetWorth` and the
      `currentAssetCashBalance` / `currentLiabilityBalance` oracles retained
      unchanged. Not yet wired to a cache.
- [x] `OpeningBalances` caches, per (canonical query text and its reference date,
      commodity, month end, checkpoint hash), the raw **unvalued** cumulative
      balances per account.
- [x] Later months are extended by deltas from one unvalued `CalcChange` /
      `PerPeriod` report over the suffix, accumulated per account with `maPlus`.
      The delta report is always **monthly**, whatever the request's granularity:
      a year end is also a month end, so one shape of delta report serves both.
- [x] Each accumulated column is valued `AtEnd` at the column's own inclusive end
      date by `Internal.PeriodReport.reportValuation`, which is hledger's
      `mixedAmountValueAtDate` over the **report journal's** oracle and styles —
      the price-truncated journal, not the snapshot's.
- [x] Rows are cached unclipped and per account; depth stays a projection through
      the existing `breakdownItemsFor`.
- [x] `NetWorthCheckpointSpec`: every fixture × granularity × commodity × three
      depths, comparing column totals, missing sets and the breakdown of *every*
      column, plus the same comparison through `computeNetWorth`'s figures.
- [x] Yearly is not projected from monthly columns at all — see the corrections
      below. It is accumulated from monthly deltas and valued at the year end,
      which is what makes it identical rather than merely additive.

### Five corrections established while implementing S4 (net worth)

1. **The equivalence is exact, and it rests on one line of hledger.**
   `MultiBalanceReport` computes a historical row as
   `historical = avalue $ cumulativeSum unvaluedChanges`: it sums *unvalued*
   per-period changes and values the result once per column, at that column's
   inclusive end day taken from `dayPartitionToPeriodData`. So the resumed path is
   not an approximation of hledger's arithmetic, it is the same arithmetic with
   the prefix of the sum supplied from a checkpoint. If that line ever changes to
   value before summing, this slice fails silently and completely.
2. **The row set needs no zero-row elision, because hledger elides nothing here.**
   `isBoring` drops a flat row only when its balance looks zero in every column
   *and* its posting count is zero in every column — but with `Historical`
   accumulation those counts are cumulative, since `cumulativeSum` folds
   `BalanceData` with a `Semigroup` that adds `bdnumpostings`. Every account with
   a posting anywhere at or before the report's end therefore keeps a row,
   including one that closed to zero years ago. An earlier revision of this slice
   reimplemented the elision and dropped exactly those rows; because a
   consolidated report's per-side totals are the sum of its *retained* rows, that
   is a wrong total waiting for a journal where the row is not exactly zero.
   Pinned by `retainedRowsAgree`; the whole fixture sweep passes with the bug in.
3. **`amountLooksZero` is not a display-precision test on report amounts.** Report
   amounts carry the precision of their own quantity rather than the journal's
   declared commodity style — even after valuation, where the converted amount
   takes the precision of the result. Two hand-built fixtures that were supposed
   to produce a "looks zero but is not" row both failed to, which is what led to
   correction 2.
4. **The valuation journal must be the report's, not the snapshot's.** hledger
   expands the final interval to its calendar boundary, so the last column is
   valued at a date that can fall *after* `latestJournalDate`. The report journal
   already truncates price directives there; valuing from `snapshot.journal`
   instead sees a directive the report cannot, and the future-price fixture
   catches it immediately. `Domain.Valuation.valueMixedAt` is therefore the wrong
   entry point for this, and it also rebuilds the price oracle per date.
5. **A query's reference date belongs in the key.** A query text is resolved
   against `snapshot.latestJournalDate`, so a relative term such as
   `date:thismonth` denotes a different span once an append moves that date, and
   it can restate arbitrarily old columns — which the month chain, diverging only
   at the appended month, does not catch. `QueryKey` carries the reference date
   alongside the text for that reason. Any later family keyed on query text needs
   the same.

Two smaller notes:

- **The yearly report gains nothing from checkpoints.** Its window is the whole
  journal, so the day it would open from is the December before the journal
  begins, which has no chain entry. Monthly is where the win is: the window is the
  last 24 months, so a resume replaces a full-history posting pass with a delta
  report over the tail.
- **Retention must be sparse here, unlike S3.** One month's
  `Map AccountName MixedAmount` shares no structure with the next, so dense
  retention costs `months × accounts`. Measured on the fixtures: 5,000-transaction
  journal, 29 retained months and 870 account entries against 209 candidate
  months; 50,000-transaction journal, 73 retained months and 2,190 entries against
  1,036 candidate months — roughly an eighth and a thirteenth of dense. The policy
  is S3's dense-tail-plus-twelfth-month grid *plus the window's own opening month*,
  which the grid retains only by luck and which is the one month a monthly request
  actually resumes from.

### S5 — Checkpoint cashflow, holdings and activity

- [x] Cashflow: done, pure only — `Domain.Internal.CashflowCheckpoint` plus
      `Domain.Cashflow.computeCashflowResumed`, with `computeCashflow` retained as
      the oracle. Not yet wired to a cache. Columns are cached unclipped and
      per-account, so one entry serves every depth; `collapseToDepth` applies
      depth as a projection.
- [ ] Holdings: cache one monthly market/historical-cost report pair per
      (commodity, chain hash) and project every scope from it — holdings are
      additive over account prefixes, which is the same fact the completed
      shared-report work relies on. Worth ~0.7–1.2 s.
- [ ] Activity and accounts-tree: benchmark first. They are currently unmeasured,
      and this document's own history says three rounds of reasoning about which
      parts were expensive were wrong.

### Four corrections established while implementing S5 (cashflow)

1. **Yearly-from-monthly holds for amounts, but not for the `missing` set.** Row
   amounts and column totals add exactly under `AtThen`, as designed. The missing
   set does *not*: a foreign commodity can be non-negligible in individual months
   and cancel to nothing across the year, and hledger computes a yearly column
   first and only then asks what it could not convert. Unioning the monthly sets
   withholds a yearly timeline the unchecked path shows, so `sumColumns`
   recomputes `missing` from the summed amounts. Pinned by
   `cancellingCommodityIsNotMissingForTheYear`; nothing else in the suite catches
   it.
2. **A month's report window cannot be selected by filtering transactions.**
   `reportWindow` anchors the window at `snapshot.latestJournalDate` — the query
   contributes the end date, the transactions only the start — and then keeps the
   *last 24* monthly periods. Filtering transactions to a month therefore still
   yields the journal's final 24 months. Presenting the snapshot with
   `latestJournalDate` moved back to a chunk's end moves the window instead, and
   chunking to 24 months keeps every wanted month inside it. Safe under `AtThen`
   because the narrowed price-directive set only loses directives dated after
   every posting in the chunk, and because `jinferredmarketprices` is a journal
   field that replacing `jtxns` does not recompute.
3. **The depth-clipped reports were never the ones to cache.** `computeCashflow`
   already builds its combined report unclipped and collapses in
   `breakdownItemsFor`; `incomeGainPeriodicReport` and `expensePeriodicReport`
   (the `Just depth` ones) are only used as the spec's oracle. The unclipped
   equivalence held for every fixture at every depth in 1..3.
4. **`atDepth` clamps to 1..4 while `periodicBalanceReport` does not.** A
   collapse at depth 5 or more cannot match a report clipped to that depth. The
   UI never asks for more than 4, but a depth-collapse equivalence test above 4
   would fail for that reason alone.

### S6 — Scoped invalidation and persistence

- [ ] Per-family chains using the existing scope-relevance predicate from
      `stepAccumulator`: an expense-only edit should not invalidate the
      investments cube or the asset columns at all. Turns invalidation from
      "truncate at a date" into "truncate at a date, for the families that care".
- [ ] Per-file pre-filter: `journal.jfiles` is already `(path, content)`. Users
      who split by year get an unchanged-history signal before any folding.
- [ ] Optional: persist checkpoints to SQLite keyed by chain hash. Content
      addressing makes this a plain KV table. Measure journal parse time on boot
      first — if parsing dominates, this buys less than it appears to.

## Verification

Performance work must preserve exact observable behaviour. The pattern that
worked for the flow phase is the pattern to use here: keep the existing
implementation as the reference and assert field-by-field agreement.

- **Oracles already in the repo**: `summaryAtPrepared` (singular investment
  path), `currentAssetCashBalance` and `currentLiabilityBalance` in
  `Internal/NetWorth.hs`, and the separately filtered reports that
  `PeriodReportSpec` compares against.
- **Shadow verification.** Behind a flag, recompute a view fully in the
  background after publishing the incremental result, compare, and log any
  divergence with the chain hashes involved. Run it always in CI over the
  fixtures, and make it available in production for a soak period. This is the
  only mechanism that catches a wrongly *kept* checkpoint, which is the failure
  mode with no other symptom.
- **Coverage** (carried over, still required): period cutoff dates and
  transactions on boundaries; nested scopes above and below mapping scopes;
  transfers wholly inside a scope; cash-paid and reinvested income; fees and
  realised gains; missing prices, missing cost, and foreign cost; explicit-cost
  accounts without performance mappings; empty portfolios and portfolios that
  become empty historically; ambiguous or non-convergent XIRR; monthly truncation
  to 24 periods and full yearly history; net-worth queries, valuation dates,
  signs, breakdown depths, and incomplete periods; snapshot revision changes
  during calculation.
- **New coverage specific to this design**: historical price insertion;
  historical transaction insertion; whitespace-only reformatting; commodity
  decimal-place change; account-type declaration change; config change; file
  added/removed from the include set; an edit that reverts the journal to a prior
  state (must *hit*); a reload arriving mid-rebuild (must abandon and restart).
- **Measurement discipline**: threaded RTS at `-N1`, `-j1`, one selected case per
  process. Force every field of every benchmarked record. Report time, allocated
  bytes and maximum residency. Treat any component slower than its own parent as
  a measurement bug. Use a separate unrestricted-`-N` load test for production
  concurrency; a serial microbenchmark is not concurrency evidence.

Property tests would be valuable for single-scope/multi-scope equivalence,
prefix-aggregation laws, and a chain law (`resume from checkpoint k` equals
`fold from scratch` for all k). **They require user go-ahead before being
added.**

## Expected outcome

| Path | Now (50k) | After S0–S2 (warm) | After S3–S5 (append edit) |
|---|---:|---:|---:|
| Net worth | ~741 ms | ~1–5 ms | rebuild ~ms, not ~741 ms |
| Cashflow | ~196 ms | ~1–5 ms | rebuild ~ms |
| Investments (complete) | ~5.27 s | ~1–5 ms | rebuild ~tens of ms |
| Rebuild after one appended transaction | n/a | ~6 s (full recompute) | proportional to the edited tail |

S0–S2 make repeat and post-warm requests interactive. S3–S5 make the *rebuild*
cheap, which is what keeps the service interactive while someone is actively
editing their journal — the case the current design handles worst.

## Risks

| Risk | Consequence | Mitigation |
|---|---|---|
| A checkpoint is wrongly kept after a historical change | Silently stale numbers with no error | Hash price directives by date; global fingerprint for anything not date-scoped; shadow verification |
| `tsourcepos` or `tindex` included in the hash | Cache never hits; all the complexity, none of the benefit | Explicit test that reformatting leaves the chain unchanged |
| Commodity style inference treated as date-scoped | Wrong historical display values | Styles live in the global fingerprint |
| Checkpoints retain hledger intermediates | Memory growth and the measured fusion regression | Cache compact projected values only; residency benchmark per slice |
| Lazy checkpoint fields defer work into a request | Unexpected cold latency | Force every field before publication; `Accumulator` is already strict, keep it so |
| Dense checkpoints cost more memory than expected | Service becomes less predictable | Sparse retention (every 12 months + last 24) and a byte budget |
| Eager rebuild takes too much CPU after every save | Slow reloads, poor interactive response | Existing debounce, generation-token cancellation, bounded workers, eager set limited to common views |
| Cache grows with query variants | Unbounded memory | Byte-budgeted eviction, revision-ordered; never eagerly cache arbitrary query space |
| Derived data from mixed revisions in one response | Internally inconsistent screen | Chain hash in every key; atomic `IORef` publication of a whole `Derived` value |
| Revision event fires before derived data exists | Stampede of cold requests | Separate derived revision; `refreshing` freshness; single-flight |
| Background failure hides new journal data indefinitely | Permanently stale dashboard | Retain last complete derived view, surface the error, retry, log and measure |
| Optimizing from a reading of the code rather than a measurement | Effort spent on minor terms | Component benchmark first; this has already produced two wrong priority orders |

## Open decisions

1. Eager set: which (commodity, granularity, scope) combinations justify eager
   rebuilding. Start with default commodity, root scope, monthly; let S0's
   hit-rate metric decide the rest.
2. Cache budget: bytes, and the eviction order across revisions.
3. Checkpoint density for `Accumulator` versus `openingBalances` — measure before
   choosing.
4. Whether the derived revision is exposed in the API contract or only as
   `freshness`.
5. Worker limit on the target NixOS host, and whether reload work is lower
   priority than interactive misses.
6. Whether shadow verification stays available in production behind a flag after
   the soak period.
