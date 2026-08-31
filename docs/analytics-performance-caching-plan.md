# Analytics caching plan: checkpointed folds over an immutable history

_Status: designed, not started · Last updated: 2026-08-31_

Part of the [analytics performance index](analytics-performance-plan.md). This is
the only remaining optimization phase. Prerequisites are complete: see
[flow optimization](analytics-performance-flow-optimization.md) for the measured
baseline and [rejected approaches](analytics-performance-rejected.md) for the
constraints that shape this design.

This document is written to be implementable by someone who has not read the
other two. It states the idea, the invariants that make it correct, the data
shapes, and an ordered checklist of slices. Every slice is independently
shippable and independently verifiable.

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
   history. Cheap relative to the parse that already happened.
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
  valuation date, so a price appended today cannot change a 2019 column.
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

The rule: **anything the journal derives globally rather than by date goes in a
global fingerprint that invalidates the whole chain.** Styles and account-type
declarations rarely change once a journal is established, so this costs almost no
hit rate and removes a class of subtle bugs entirely.

### Two implementation traps

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

## Data shapes

Sketches, not final signatures. Adjust names to fit the modules they land in.

```haskell
-- Invalidates the entire chain when any component changes.
data GlobalFingerprint = GlobalFingerprint
    { files          :: !Hash   -- (path, content) pairs, length-delimited
    , parseOptions   :: !Hash   -- "definputopts-v1", as SnapshotStore already does
    , commodityStyles:: !Hash   -- resolved display styles
    , accountTypes   :: !Hash   -- declared account types
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

- [ ] Cache, per (query, commodity), the raw **unvalued** cumulative balances per
      account at sparse checkpoint months.
- [ ] For months after a checkpoint, obtain deltas from one `CalcChange` /
      `PerPeriod` **unvalued** report restricted to the suffix date range, and
      accumulate with `(+)` per account. This is the only new arithmetic; hledger
      still produces the deltas.
- [ ] Value each accumulated column `AtEnd` using the journal's existing price
      oracle. Reuse the in-repo valuation path (`conversionFromMixed`, and the
      `Day -> MixedAmount -> MixedAmount` shape already used by
      `InvestmentPerformanceContext.valueAt`) — do not hand-roll valuation.
- [ ] Cache unclipped `breakdownRows`; keep depth collapsing as a projection, as
      `breakdownItemsFor` already does with `atDepth`.
- [ ] Extend `PeriodReportSpec`: for every fixture × granularity × commodity ×
      depth, the checkpointed columns, missing sets and breakdown items must equal
      the current full consolidated report exactly.
- [ ] Pin the yearly-from-monthly projection with its own cases before relying on
      it.

### S5 — Checkpoint cashflow, holdings and activity

- [ ] Cashflow: `PerPeriod` + `AtThen` columns are per-period and additive, so
      cache monthly columns and sum for yearly. Note the reports are built
      **depth-clipped** (`Just depth` in `Internal/Cashflow.hs`) — either cache
      unclipped and collapse in the application layer, or include depth in the
      key.
- [ ] Holdings: cache one monthly market/historical-cost report pair per
      (commodity, chain hash) and project every scope from it — holdings are
      additive over account prefixes, which is the same fact the completed
      shared-report work relies on. Worth ~0.7–1.2 s.
- [ ] Activity and accounts-tree: benchmark first. They are currently unmeasured,
      and this document's own history says three rounds of reasoning about which
      parts were expensive were wrong.

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
