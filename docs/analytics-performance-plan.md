# Analytics performance improvement plan

_Status: in progress · Last updated: 2026-08-26_

## Summary

The `net-worth/query` and `investments/query` endpoints are CPU-bound after the
journal has already been loaded into memory. Journal parsing is not part of the
normal request path: Hedger keeps an immutable `Snapshot` in memory and reloads
it after filesystem events. The performance problem is repeated derivation of
reports from that snapshot.

The investment **timeline** has been fixed. It previously recomputed a full
summary independently for every period, so a 24-month history behaved like 24
current reports. It now runs two periodic balance reports and one chronological
sweep for the whole timeline. At 50,000 transactions the monthly history went
from exceeding a 45-second benchmark timeout to 2.4 seconds.

What remains, in order of measured cost:

1. The investment **drill-down** summaries. One request also computes the
   current scope, every immediate child, and every configured allocation scope
   at the latest date. Each still runs its own balance reports and its own
   sweep. This is now the dominant cost of the endpoint.
2. Net-worth report consolidation and base-report caching.
3. Revision-scoped derived data, bounded memoization, and single-flight.
4. API decomposition for progressive rendering.
5. Bounded backend concurrency, only after duplicated work has been removed.

Frontend parallelism by itself is not a sufficient optimization. It can reduce
wall-clock latency on an idle multicore server, but the remaining computations
would still repeat full journal scans. Under concurrent use this raises total
CPU, allocation, garbage collection, and memory-bandwidth pressure.

## Goals

- Make the default Net Worth screen feel immediate for a 50,000-transaction
  journal.
- Make the whole Investments screen, not just its timeline, usable at 50,000
  transactions.
- Make calculation cost scale primarily with journal size, rather than journal
  size multiplied by the number of displayed periods and scopes.
- Preserve the existing valuation, cost, completeness, warning, and XIRR
  semantics.
- Keep journal reloads atomic: a response must be internally consistent with a
  single snapshot revision.
- Bound memory use and background CPU so caching does not make the service less
  predictable.

## Non-goals

- Introducing a database as the source of truth.
- Persisting derived reports across server restarts in the first iteration.
- Precomputing every possible free-form hledger query.
- Changing accounting semantics to obtain a faster result.
- Hiding slow calculations behind unbounded worker or request concurrency.

## Measured results

Cabal development build with `-O1`, run against the repository's 5,000- and
50,000-transaction portfolio fixtures. Baseline is commit `b424580` (`Speed up
investment performance reports`); "now" is the current branch head. These are
local measurements and should be treated as directional, not as portable
service-level objectives.

| Fixture | Operation | Baseline | Now |
|---|---|---:|---:|
| 5,000 | Investment current summary | ~193 ms | ~148 ms |
| 5,000 | Investment monthly history, 24 periods | ~3.85 s | **~289 ms** |
| 5,000 | Net worth | ~132 ms | ~137 ms |
| 50,000 | Investment current summary | ~1.98 s | ~1.47 s |
| 50,000 | Investment monthly history, 24 periods | >45 s, timed out | **~2.38 s** |
| 50,000 | Net worth | ~698 ms | ~752 ms |

Net worth is unchanged because nothing in this work touched it; the small
deltas are machine noise.

The benchmark suite now reports allocation, copy, and peak-memory figures
alongside timings, and has a component group that attributes one investment
summary across its parts. Component benchmarks for the net-worth periodic
reports and for the two current net-worth balance reports are still missing,
and are a prerequisite for the net-worth work below.

### What the component measurements established

Attributing one summary at 50,000 transactions was what identified the real
costs; three rounds of prior reasoning about which parts were expensive were
wrong. Two findings are worth keeping:

- The dominant per-day cost was not either hledger balance report. It was the
  hand-rolled `retainedPerformanceAtWith` scan, at 632 ms and 2.3 GB allocated
  per call, because it iterated mappings in the outer generator and so rescanned
  the journal once per mapping.
- The XIRR solver evaluates NPV at 1025 fixed grid points per solve, so it cost
  394 ms at 50,000 flows before its per-evaluation constants were hoisted, and
  43 ms after. The solver is now a small term.

Any further optimization here should start from a component measurement, not
from a reading of the code.

## Current request architecture

### Snapshot loading and filesystem events

The backend already implements the first level of caching:

1. The journal is parsed into an immutable `Snapshot` containing the hledger
   journal, transactions, account tree, latest date, and revision.
2. `SnapshotStore` holds the current snapshot in an `IORef`.
3. `JournalWatcher` watches the root journal and included files using FSNotify,
   which uses FSEvents on macOS and inotify on Linux.
4. Events are debounced before the journal is reparsed.
5. A successful reload atomically publishes the new snapshot and revision.
6. A failed reload retains the last successful snapshot and records the error.

The revision is an MD5 over the journal files' contents, so it is stable across
a save that does not change content, and a derived cache keyed on it survives
such saves.

Consequently, adding another raw-journal cache would not improve these endpoint
requests. The useful next cache layer contains data derived from a snapshot.

### Investments

One request prepares a shared context containing the snapshot and investment
configuration, a price oracle and commodity styles, the display-commodity
valuation function, a journal whose acquisition costs have been converted to
historical display-commodity values, and the date-sorted transaction list.

The **timeline** for the selected scope is computed by `summariesAtPrepared`,
which for the whole period list runs:

- one periodic balance report for market-value holdings, read per column;
- one periodic balance report over the historical-cost journal; and
- one chronological sweep (`scopeScansAt`) that accumulates account discovery,
  explicit-cost status, retained performance, attributed income/gains/fees, and
  external cash flows, snapshotting them at each period end.

Only the XIRR solve then runs per period.

The **drill-down** summaries — the current scope, each immediate child, and each
configured top-level allocation scope, all at the latest date — still go through
`summaryAtPrepared` one scope at a time. Each of those pays two single-date
balance reports and one full sweep. For the 50,000-transaction fixture that is
nine such summaries, and it now dominates the endpoint.

### Net worth

One net-worth request performs:

- an asset periodic multi-balance report;
- a liability periodic multi-balance report;
- a gain periodic multi-balance report;
- a current asset balance report; and
- a current liability balance report.

The periodic reports provide timeline totals and selected-period breakdown
rows. The two current reports recalculate the latest values used to override the
KPI values.

hledger's `reportSpanHelper` derives the report span from the whole journal's
date span and only reads *date* terms out of the query, never account terms. All
three periodic reports therefore share an identical window, which makes the
`zipWith3` that combines their columns safe and removes the main alignment risk
from consolidating them.

## Remaining work

### 1. Share one pass across the latest-date scopes

This is the largest remaining item and the same low-risk shape as the timeline
fix that already landed.

Holdings are additive over account prefixes. One *flat* balance report at the
latest date yields per-account rows, and each scope's holdings is the sum of the
rows beneath its prefix. So a single pair of reports — market and historical
cost — can serve the root, every immediate child, and every allocation scope,
instead of two reports per scope.

The non-holdings work is not additive across scopes, and this is the constraint
that shapes the design. Scope enters the predicate, not just the filter: for
`scope = Just "assets:equity"` a transfer from `assets:liquid` is an external
contribution, whereas for the root scope `assets:liquid` is itself an
asset-class account so the same transfer cancels to zero. The same asymmetry
appears in `transactionMatchesScope` and `mappedAssetScope`. Per-scope results
therefore cannot be derived by summing child aggregates.

The sweep must instead carry one accumulator set per requested scope and update
all of them in a single traversal. Cost becomes one pass times the number of
scopes' predicate evaluations, rather than one full traversal per scope.

Note that the timeline sweep does not have this problem: it only ever needs the
single selected scope.

### 2. Net-worth report consolidation

Net-worth optimization should proceed conservatively because hledger balance
calculation and valuation modes encode accounting semantics.

#### The two current reports are not redundant

An earlier version of this plan proposed deriving the current KPI values from
the final periodic column and deleting the two standalone balance reports.
Reading the code shows they are not equivalent, in two independent ways:

- **Valuation date.** `periodicBalanceReport` uses `AtEnd`, valuing each column
  at that period's end — the last day of the calendar month or year containing
  the latest transaction. `currentReportValue` uses `AtDate` at
  `latestJournalDate`. Any price directive between those dates changes the
  result.
- **Completeness semantics.** `currentReportValue` ends in `requireComplete`, so
  it yields `Nothing` when any commodity is unconvertible. `reportTotals` uses
  plain `conversionFromMixed`, which returns a partial sum plus a `missing` set.
  `Analytics.useCurrent` then deliberately falls back to the lenient
  periodic-derived KPI only when the strict value is absent. Substituting the
  periodic total collapses strict-KPI into lenient-KPI, which is an observable
  change and contradicts the completeness goal above.

The two removed reports are also single-column non-interval reports, materially
cheaper than a 24-column periodic report, so the saving was overstated.

The useful version of this task is therefore narrower: make the periodic window
end at `latestJournalDate`, and keep `requireComplete` as an application-layer
projection, rather than deleting the reports.

#### Share assets and liabilities where possible

Investigate running one periodic report over assets, cash, and liabilities, then
partitioning rows using hledger account types. The shared-window finding above
removes one obstacle. This is worthwhile only if the combined report preserves
correct asset and liability signs, the different requested breakdown depths,
missing-commodity reporting, and the exact row data both breakdowns need.

The gain report uses a different balance calculation mode and is likely to
remain separate unless hledger exposes enough data to derive both views from a
single report without reproducing its logic.

#### Cache the base report, not the selected view

Selected asset/liability periods and display grouping are projections over a
periodic report. Cache the base rows and totals by:

```text
snapshot revision + parsed query + granularity + commodity + required depth
```

Changing the selected period should then select another column and regroup its
rows rather than rerun hledger. Where practical, cache at the maximum supported
depth and collapse to shallower depths in the application layer.

### 3. Revision-scoped derived data

Add a derived-data store associated with the immutable snapshot revision. A
possible shape is:

```text
DerivedSnapshot
  revision
  account/date indexes
  investment hierarchy and mapping indexes
  per-commodity investment contexts
  common net-worth base reports
  bounded memoized request results
```

Per the repository's layering rules, the mutable store and its worker pool
belong in `Hedger.Infrastructure.*`; the values it holds are computed by pure
`Hedger.Domain.*` functions. Domain values continue to carry no
`SnapshotMetadata`.

#### Eager and lazy work

Use a hybrid policy:

- Eagerly rebuild small, broadly reused indexes after every successful snapshot
  reload.
- Eagerly calculate only the most common configured views if their rebuild cost
  and memory use are acceptable, such as the default commodity's root monthly
  investment history and root monthly net worth.
- Lazily calculate uncommon commodities, granularities, account scopes, depths,
  and free-form queries.
- Store lazy results in a bounded LRU or size-aware cache.

Arbitrary hledger queries must not be eagerly precomputed because the request
space is unbounded.

#### Cache keys

Cache keys must include every input that affects output: snapshot revision,
display commodity, granularity, normalized account scope, parsed or canonically
represented hledger query, positive and negative breakdown depths,
valuation/configuration version, and any future calculation-mode option.

Selected periods should not be part of the expensive base-report key when they
only select data already present in the cached report.

#### Atomic publication and freshness

Every response must combine a snapshot and derived data from the same revision.
There are two reasonable serving policies while revision `N + 1` is being
prepared:

1. Continue serving the complete revision `N` view as stale until the new
   derived view is ready, then atomically publish revision `N + 1`.
2. Publish snapshot `N + 1` immediately and calculate missing derived data on
   demand, while common background work proceeds.

The first policy gives predictable latency and natural stale-while-revalidate
behavior but delays visible freshness. The second exposes new data sooner but
allows the first request after a change to be slow. For the expensive default
dashboards, the first policy is preferable if response metadata clearly reports
staleness and rebuild failures.

Choosing the first policy requires a change to event publication.
`Application.Events` waits on `awaitRevisionChange`, and `SnapshotStore` writes
the revision `TVar` the instant the snapshot is published, so clients would be
told to refetch before the derived view exists — precisely the stampede this
section aims to avoid. Either move revision publication behind derived
readiness, or carry a separate derived revision in the event and in response
metadata.

Failed journal reloads should continue serving the last successful snapshot.
Failed derived builds should likewise retain the last complete derived view and
surface an operational error rather than publish partial data as current.

#### Single-flight and bounded concurrency

When several requests miss the same key, exactly one computation should run and
the others should await its result. This prevents route changes, browser
retries, or several clients from multiplying a costly report.

Background and cache-miss calculations should use a bounded worker pool. The
server already runs with the threaded runtime and `-N`, but available
capabilities are not a reason to start an unbounded number of CPU-heavy reports.
The limit should leave capacity for health checks, event streams, cached
responses, and ordinary transaction/account queries.

### 4. API decomposition and progressive rendering

Splitting endpoints is useful when the resulting pieces have different user
priority or can reuse cached derived state. It should not cause each endpoint to
perform its own full scan.

#### Investments

A decomposition into an overview, a value history, and a return history is
still worth considering, but **the obvious section boundary is the wrong one**.
`purchaseCost` is plotted on the default Value chart, as both a line and one
bound of the area series. In the domain it is `reportedCost - retained`, and
`retained` is gated on mapping completeness. So a "value history" section that
includes purchase cost still pays account discovery, explicit-cost discovery,
and the retained accumulation.

Only `attributed`, the external cash flows, and the XIRR solve are exclusive to
the Returns view. A genuinely cheap initial section is therefore market value
and gain/loss; purchase cost belongs with the deferred data, or `retained` has
to be made cheap first.

Since the timeline now costs 2.4 seconds at 50,000 transactions rather than
exceeding 45, this is a smaller win than it was, and it should be sequenced
after item 1.

The frontend should support partial success: an overview can remain visible if
history fails, and value history can remain visible if return history fails.
Every part must carry or be checked against a revision so results from different
journal snapshots are not combined silently.

Whether this becomes multiple HTTP endpoints or one endpoint with selectable
sections is an API design choice.

#### Net worth

The net-worth timeline, KPI trends, and performance chart share the same base
periodic data and should normally stay together. Selected-period breakdowns are
better candidates for separation because they can be projected cheaply from a
cached base report and change independently during interaction.

A suitable split is:

- base report: KPIs, timeline, performance, completeness, and warnings;
- selected breakdown: asset or liability rows for a period and depth.

This split should be made only after the base report can be reused; otherwise a
breakdown endpoint risks rerunning the same hledger report.

### 5. XIRR solver, if it becomes material again

After item 1, XIRR is roughly 43 ms per solve at 50,000 transactions. Sharing
grid evaluations across the 24 nested prefixes would remove most of the
remaining ~1 second, but the clean formulation is **not** numerically safe and
should not be attempted casually.

`npv` subtracts a per-prefix `reference` from each year fraction *before*
exponentiating, which keeps every exponent at or below zero. Factoring that
reference out so prefix sums can be shared reintroduces intermediates on the
order of `exp(690)` for long histories at the -99.9999% end of the search
interval, which overflows to infinity where the current code stays finite. That
would change `AmbiguousRates` and `NoConvergentRate` outcomes on exactly the
cases `backend/test/fixtures/investment-ambiguous-xirr.journal` exists to pin.

Only the `logRate >= 0` half of the grid, where the reference is zero for every
prefix, shares exactly and safely. That is worth about half the available
saving.

### 6. Backend parallelism

After consolidation and caching, remaining independent calculations may run in
parallel: the asset and liability net-worth reports if they cannot be combined,
the gain report alongside the balance report, independent already-indexed
investment scopes, and eager default views after a new revision.

Parallelism should be controlled by the backend rather than emerging from an
arbitrary number of frontend HTTP calls. Backend control makes it possible to
share prepared data, cap concurrency, cancel work for obsolete revisions,
deduplicate identical work, and measure one logical operation coherently.

Parallelism reduces wall time but not total work. It is a final multiplier on an
efficient calculation, not a replacement for one.

## Task and impact estimate

| Priority | Task | Expected gain | Estimated effort |
|---:|---|---|---|
| 1 | Share one flat report and one multi-scope sweep across the latest-date scopes | Very high; the dominant remaining endpoint cost | Medium–high, 4–7 days |
| 2 | Add an endpoint-level investments benchmark | No direct gain; the current estimate for item 1 is unverified | Low, under a day |
| 3 | Add net-worth component benchmarks | No direct gain; prerequisite for items 4 and 5 | Low, 1–2 days |
| 4 | Cache net-worth base reports separately from period selection | High for interaction and repeated requests | Medium, 3–5 days |
| 5 | Align the periodic window to the latest journal date | Medium; keeps completeness semantics while removing a pass | Low–medium, 1–3 days |
| 6 | Add revision-scoped derived-data rebuilds after filesystem events | Very high for common views after rebuild | High, 1–2 weeks |
| 7 | Add bounded memoization and single-flight cache misses | High for repeated and concurrent identical requests | Medium, 3–5 days |
| 8 | Split investment overview, value history, and return history | Medium perceived latency, reduced by the timeline fix | Medium–high, 4–7 days |
| 9 | Split selected net-worth breakdowns from the base report | Medium perceived and interaction improvement | Medium, 3–5 days |
| 10 | Consolidate asset and liability net-worth passes | Medium–high if hledger semantics allow it | Medium–high, 4–8 days |
| 11 | Share the safe half of the XIRR grid across prefixes | Low; ~0.5 s at 50,000 transactions | Low, 1–2 days |
| 12 | Run remaining independent calculations concurrently | Low–medium wall-time improvement, no CPU reduction | Medium, 2–4 days |

Effort values describe focused implementation time, not calendar promises.

Items 2 and 3 are listed high deliberately. Component measurement corrected the
priority order of this document twice; the estimate for item 1 is currently
derived from component numbers rather than measured end to end, and no benchmark
covers the multi-scope endpoint path at all.

## Correctness and verification

Performance work must preserve exact observable behavior. The approach that
worked for the timeline was to leave the existing per-day functions in place as
the reference implementation and assert field-by-field agreement against them
across the fixtures and the edge-case journals in `backend/test/fixtures/`.
Repeat that for item 1.

Verification should cover:

- period cutoff dates and transactions on boundaries;
- nested scopes above and below mapping scopes;
- transfers wholly inside a scope;
- cash-paid and reinvested income;
- fees and realised gains;
- missing prices, missing cost, and foreign cost;
- explicit-cost accounts without performance mappings;
- empty portfolios and portfolios that become empty historically;
- ambiguous or non-convergent XIRR;
- monthly truncation to 24 periods and full yearly history;
- net-worth queries, valuation dates, signs, breakdown depths, and incomplete
  periods; and
- snapshot revision changes during calculation.

Property tests would be valuable for equivalence between single-scope and
multi-scope calculations and for prefix-aggregation laws, but require user
go-ahead before they are added.

One deliberate, unobservable divergence is already in place and should be
preserved rather than rediscovered: the sweep sorts transactions by date, so
external cash flows now come out date-ordered rather than in journal parse
order. hledger's `journalFinalise` restores parse order and never sorts. The
reordering cannot be observed because the only consumer is `Xirr.xirr`, whose
`normalise` groups flows by day before solving.

Performance verification should report median or mean time with variance,
allocated bytes and maximum residency, cold calculation and warm cache hit and
concurrent identical miss, both fixtures, and monthly and yearly reports.

## Operational observability

Add metrics or structured timings for:

- snapshot reload duration and result;
- derived rebuild duration and revision;
- queued, running, cancelled, and failed background jobs;
- cache hits, misses, evictions, and single-flight waiters by report family;
- calculation duration by report stage;
- response revision and whether it was served stale; and
- worker-pool saturation.

Avoid logging free-form journal queries or sensitive account data. Use report
family, normalized non-sensitive dimensions, request ID, revision, and timing.

## Risks and mitigations

| Risk | Consequence | Mitigation |
|---|---|---|
| Multi-scope aggregation subtly differs from per-scope semantics | Incorrect balances or returns | Keep the per-scope functions as the reference implementation and assert agreement across fixtures and edge-case journals |
| Cache entries combine different revisions | Internally inconsistent screen | Revision in every key and atomic publication of snapshot-derived pairs |
| Lazy derived data shifts rebuild work into a request | Unexpected cold latency | Explicitly force all fields used by queries before publication |
| Eager rebuilding takes too much CPU after every save | Slow reloads and poor interactive response | Debounce, bounded workers, cancel obsolete generations, and eagerly build only common views |
| Cache grows with query variants | Unbounded memory use | Bounded LRU or size-aware eviction; do not eagerly cache arbitrary query space |
| Frontend endpoint split increases duplicate work | Higher load despite faster first paint | Split only over shared indexed/cached data and measure total CPU |
| Parallel reports saturate CPU or memory bandwidth | Worse tail latency | Central concurrency limit and load testing, not unbounded request fan-out |
| Old derived results remain after config changes | Incorrect classifications or targets | Include relevant configuration identity/version in derived generations and cache keys |
| Background failure hides new journal data indefinitely | Permanently stale dashboard | Expose stale/error metadata, retry safely, and log/measure derived rebuild failures |
| Optimizing from a reading of the code rather than a measurement | Effort spent on minor terms | Add the component benchmark first; this has already produced two wrong priority orders |

## Decisions to make during design

1. Whether a prepared derived revision is published together with the snapshot,
   or whether the snapshot is published immediately and reports fall back to
   on-demand calculation — and correspondingly whether revision events are
   deferred behind derived readiness.
2. Which views are common enough to rebuild eagerly: likely root monthly reports
   in the default display commodity, but production usage should confirm this.
3. Whether API decomposition uses separate routes or one route with requested
   response sections.
4. The cache memory budget and whether it is count-, size-, or cost-based.
5. The worker limit on the target NixOS host and whether reload work has lower
   priority than interactive cache misses.
6. Whether a combined net-worth balance report can preserve all current hledger
   semantics.
7. The acceptable freshness delay while common derived reports rebuild.

## Recommended next slice

1. Add an endpoint-level investments benchmark covering the current scope,
   children, and allocations together, so item 1 is measured rather than
   estimated.
2. Extend the flat holdings report and the sweep to serve every latest-date
   scope from one pass, validated by field-by-field equivalence against the
   existing per-scope path.
3. Rebenchmark before moving on to the net-worth work.
