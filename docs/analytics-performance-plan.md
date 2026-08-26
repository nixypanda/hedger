# Analytics performance improvement plan

_Status: in progress · Last updated: 2026-08-26_

## Summary

The `net-worth/query` and `investments/query` endpoints are CPU-bound after the
journal has already been loaded into memory. Journal parsing is not part of the
normal request path: Hedger keeps an immutable `Snapshot` in memory and reloads
it after filesystem events. The performance problem is repeated derivation of
reports from that snapshot.

The investment **timeline and latest-date drill-down** have been fixed. The
timeline previously recomputed a full summary independently for every period;
it now runs two periodic balance reports and one chronological sweep for the
whole timeline. The drill-down previously did the same work independently for
the current scope, every immediate child, and every configured allocation
scope; it now runs two flat balance reports and updates one accumulator per
scope in a single chronological traversal.

At 50,000 transactions, the monthly history went from exceeding a 45-second
benchmark timeout to 2.4 seconds. The latest-date drill-down went from 6.01 to
3.28 seconds, with allocation falling from 16 GB to 10 GB. This is a material
improvement, but it also shows that the remaining per-scope predicate,
conversion, XIRR, and allocation work is still much too expensive for the
200 ms cold-request target.

What remains, in order of measured cost:

1. Further request-local investment indexing, predicate sharing, conversion,
   XIRR, and allocation reduction. The shared traversal removed repeated list
   walks and balance reports, but still performs scope-specific work for every
   transaction and requested scope.
2. Net-worth and cashflow report consolidation, now guided by component
   measurements of their production report paths.
3. API decomposition and bounded backend concurrency, only where they improve
   the uncached request path after duplicated work has been removed.
4. Revision-scoped derived data, bounded memoization, a database, or other
   cross-request storage only after the compute-first phase is complete and has
   been remeasured.

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
- Target less than 200 ms for each cold analytics request against the
  50,000-transaction fixture, with the report computed on every request during
  the first optimization phase.
- Preserve the existing valuation, cost, completeness, warning, and XIRR
  semantics.
- Keep journal reloads atomic: a response must be internally consistent with a
  single snapshot revision.
- Bound memory use and background CPU so caching does not make the service less
  predictable.

## Non-goals

- Introducing a database or cross-request in-memory derived-report cache before
  the compute-on-every-request optimization phase is complete and remeasured.
- Introducing a database as the source of truth.
- Persisting derived reports across server restarts in the first iteration that
  considers cross-request storage.
- Precomputing every possible free-form hledger query.
- Changing accounting semantics to obtain a faster result.
- Hiding slow calculations behind unbounded worker or request concurrency.

## Optimization sequencing decision

The first phase optimizes the cold request path while deliberately computing the
answer on every request. The target is less than 200 ms for each analytics
request on the 50,000-transaction fixture. This is a target to drive the work,
not a claim that the current hledger-based implementation can already meet it.

In this phase, improvements must reduce the intrinsic cost of one request:

- consolidate compatible hledger reports;
- share journal traversals across periods and scopes;
- build request-local prepared data and indexes once, then reuse them within the
  request;
- remove repeated conversion, grouping, sorting, and allocation;
- avoid calculating response sections that the request does not need; and
- add component and endpoint benchmarks before and after each material change.

Cross-request in-memory caching, revision-scoped memoization, eager background
rebuilds, and adding a database are explicitly deferred. They should be
considered only after the request-local improvements have been implemented and
the cold paths have been rebenchmarked. If the 200 ms target remains unmet at
that point, the measurements will show the residual work that storage or
precomputation must address rather than allowing a cache to conceal avoidable
computation.

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
| 5,000 | Investment latest-date drill-down | ~582 ms | **~321 ms** |
| 5,000 | Net worth | ~132 ms | ~137 ms |
| 50,000 | Investment current summary | ~1.98 s | ~1.47 s |
| 50,000 | Investment monthly history, 24 periods | >45 s, timed out | **~2.38 s** |
| 50,000 | Investment latest-date drill-down | ~6.01 s | **~3.28 s** |
| 50,000 | Net worth | ~698 ms | ~752 ms |

The drill-down baseline is the retained `summaryAtPrepared` reference path on
the current code, not commit `b424580`. Both sides prepare one shared context
and calculate the same deduplicated set of root, immediate-child, and configured
allocation scopes. This isolates the change made by item 1.

| Fixture | Drill-down path | Allocated | Copied | Peak memory |
|---|---|---:|---:|---:|
| 5,000 | Singular reference | 1.6 GB | 46 MB | 93 MB |
| 5,000 | Shared reports and traversal | **1.0 GB** | **28 MB** | 93 MB |
| 50,000 | Singular reference | 16 GB | 465 MB | 858 MB |
| 50,000 | Shared reports and traversal | **10 GB** | **253 MB** | 858 MB |

The earlier net-worth total remained unchanged because the optimization work did
not alter its calculation; the small deltas are machine noise.

The benchmark suite now reports allocation, copy, and peak-memory figures
alongside timings. It has component groups for one investment summary, all five
net-worth reports, and both cashflow reports, plus an application-level
investment benchmark that combines preparation, timeline, latest-date
drill-down, children, allocations, and application projection.

Fresh local measurements of the newly added benchmarks are below. They use the
same Cabal development build with `-O1`; the relaxed benchmark variance target
keeps this broad attribution pass short, so these figures are directional.

| Fixture | Operation | Time | Allocated | Copied | Peak memory |
|---|---|---:|---:|---:|---:|
| 5,000 | Complete investments application use case | ~622 ms | 1.3 GB | 44 MB | 79 MB |
| 50,000 | Complete investments application use case | ~5.65 s | 13 GB | 421 MB | 720 MB |
| 5,000 | Net worth: asset/cash periodic | ~89 ms | 90 MB | 7.8 MB | 70 MB |
| 5,000 | Net worth: liability periodic | ~16 ms | 10 MB | 1.1 MB | 70 MB |
| 5,000 | Net worth: gain periodic | ~109 ms | 102 MB | 8.6 MB | 70 MB |
| 5,000 | Net worth: current asset/cash | ~73 ms | 73 MB | 10 MB | 70 MB |
| 5,000 | Net worth: current liability | ~14 ms | 9.2 MB | 1.3 MB | 70 MB |
| 50,000 | Net worth: asset/cash periodic | ~346 ms | 314 MB | 42 MB | 669 MB |
| 50,000 | Net worth: liability periodic | ~157 ms | 92 MB | 14 MB | 669 MB |
| 50,000 | Net worth: gain periodic | ~377 ms | 332 MB | 45 MB | 669 MB |
| 50,000 | Net worth: current asset/cash | ~765 ms | 694 MB | 107 MB | 669 MB |
| 50,000 | Net worth: current liability | ~149 ms | 92 MB | 19 MB | 669 MB |
| 5,000 | Cashflow: income/gain periodic | ~9.8 ms | 8.2 MB | 992 KB | 72 MB |
| 5,000 | Cashflow: expense periodic | ~32 ms | 27 MB | 9.4 MB | 75 MB |
| 50,000 | Cashflow: income/gain periodic | ~105 ms | 69 MB | 12 MB | 669 MB |
| 50,000 | Cashflow: expense periodic | ~139 ms | 105 MB | 27 MB | 669 MB |

Peak memory is process-level rather than uniquely attributable to a component,
and independently measured component times are not additive. The results still
establish where to begin: the 50,000-transaction strict current asset/cash
balance is the largest measured net-worth component, while both cashflow passes
are material enough for consolidation to remain worthwhile.

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
configured top-level allocation scope, all at the latest date — are computed by
`summariesAtScopesPrepared`. It runs one flat market balance report and one flat
historical-cost report, then projects every requested prefix from their account
rows. One traversal of the date-sorted transactions carries a separate
accumulator for each scope because performance attribution and external-flow
classification are scope-dependent. The selected scope's final timeline point
is reused when it already represents the latest date.

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

## Work items

### 1. Completed: share one pass across the latest-date scopes

This landed using the same low-risk shape as the timeline fix.

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

The implemented sweep carries one accumulator set per requested scope and
updates all of them in a single traversal. Cost is now one pass times the number
of scopes' predicate evaluations, rather than one full traversal per scope.

Note that the timeline sweep does not have this problem: it only ever needs the
single selected scope.

The retained singular path remains as a benchmark and correctness oracle.
Fixture regressions compare every summary field and warning across root, nested,
duplicate, missing-price, missing-cost, foreign-cost, mapping, fee/gain, and
ambiguous-XIRR cases. No accounting-semantic divergence was found. The measured
45% time reduction and 38% allocation reduction at 50,000 transactions are
material, but the 3.28-second result leaves request-local scope work as an
active optimization target.

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

#### Future phase: cache the base report, not the selected view

This is a cross-request optimization and is deferred until the compute-first
phase described above is complete. During that first phase the same separation
is still useful inside one request: calculate the base report once and project
selected periods and depths from it without rerunning hledger.

Selected asset/liability periods and display grouping are projections over a
periodic report. Cache the base rows and totals by:

```text
snapshot revision + parsed query + granularity + commodity + required depth
```

Changing the selected period should then select another column and regroup its
rows rather than rerun hledger. Where practical, cache at the maximum supported
depth and collapse to shallower depths in the application layer.

### 3. Future phase: revision-scoped derived data

Do not begin this phase until the compute-on-every-request improvements have
been implemented and rebenchmarked against the 200 ms target.

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

During the compute-first phase, decomposition is worthwhile only when it avoids
work that the caller did not request or when request-local prepared data can be
shared. Do not split an endpoint merely to make each HTTP response finish sooner
while leaving the total ledger work unchanged. Decomposition that depends on a
cross-request base-report cache belongs to the later caching phase.

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
| 1 | **Complete:** share one flat report and one multi-scope sweep across the latest-date scopes | 45% faster and 38% less allocation at 50,000 transactions | Completed |
| 2 | **Complete:** add an endpoint-level investments benchmark | Measured the combined 50,000-transaction cold application path at ~5.65 s and 13 GB allocated | Completed |
| 3 | **Complete:** add net-worth component benchmarks | Identified the strict current asset/cash balance as the largest measured 50,000-transaction component | Completed |
| 4 | **Complete:** add cashflow component benchmarks | Measured the two 50,000-transaction periodic passes at ~105 ms and ~139 ms | Completed |
| 5 | Align the net-worth periodic window to the latest journal date | Medium; preserves valuation and completeness semantics while reducing work | Low–medium, 1–3 days |
| 6 | Consolidate asset and liability net-worth passes | Medium–high if hledger semantics allow it | Medium–high, 4–8 days |
| 7 | Consolidate income and expense cashflow passes | Potentially high; both currently use `CalcChange` and `PerPeriod` | Medium, 3–5 days |
| 8 | Share the safe half of the XIRR grid across prefixes | Low; ~0.5 s at 50,000 transactions | Low, 1–2 days |
| 9 | Profile and reduce remaining request-local indexing, conversion, grouping, sorting, and allocation costs | Required to determine whether cold requests can reach 200 ms | Open-ended, measurement-led |
| 10 | Run remaining independent calculations concurrently with a request-local bound | Low–medium wall-time improvement, no CPU reduction | Medium, 2–4 days |
| 11 | Split endpoints only where an uncached request can omit material work | Medium perceived latency when sections have genuinely different costs | Medium–high, 4–7 days |
| 12 | Rebenchmark every cold analytics path and decide whether the compute-first phase is complete | Decision point for the 200 ms target and any storage work | Low, 1–2 days |
| 13 | Future: cache base reports separately from selected projections | High for repeated requests, but deliberately deferred | Medium, 3–5 days |
| 14 | Future: add revision-scoped derived rebuilds, bounded memoization, and single-flight | Very high for common and concurrent views, but deliberately deferred | High, 1–2 weeks |
| 15 | Future: evaluate a database or persistent derived store if measurements justify it | Unknown until the compute-first phase is complete | High |

Effort values describe focused implementation time, not calendar promises.

Items 2–4 were listed high deliberately and are now complete. Component
measurement corrected the priority order of this document twice; the complete
investment application path and every production net-worth and cashflow report
now have direct coverage before consolidation work begins.

## Correctness and verification

Performance work must preserve exact observable behavior. The approach that
worked for the timeline was to leave the existing per-day functions in place as
the reference implementation and assert field-by-field agreement against them
across the fixtures and the edge-case journals in `backend/test/fixtures/`.
Item 1 follows the same pattern: the singular implementation remains in place,
and deterministic fixture regressions compare it with the multi-scope path.

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

1. Align the net-worth periodic window to `latestJournalDate`, preserving the
   distinct strict current-value and lenient historical completeness semantics.
2. Use the new component benchmarks to investigate whether asset/cash and
   liability periodic rows can come from one production report without changing
   signs, breakdown depths, or missing-commodity behavior.
3. Consolidate the compatible income/gain and expense cashflow passes, then
   rebenchmark the component and whole-report paths on both fixtures.
