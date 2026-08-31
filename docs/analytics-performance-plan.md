# Analytics performance improvement plan

_Status: flow optimization complete; API splitting rejected; caching remains · Last updated: 2026-08-31_

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
benchmark timeout to 2.72 seconds. The latest-date drill-down went from 6.01 to
2.86 seconds, with allocation falling from 16 GB to 7.1 GB. The complete
application request is 5.27 seconds and allocates 9.5 GB. These are serial,
threaded `-N1` measurements; the benchmark runs one selected case at a time so
thermal throttling and cross-benchmark contention do not become inputs.

The compute-on-every-request work is complete. The resulting state is:

1. **Flow optimization is complete.** Investment timeline and drill-down flows,
   net worth, and cashflow now share the useful request-local work identified by
   measurement.
2. **API-level splitting is rejected.** Progressive Net Worth and Investments
   APIs were implemented and measured. They did not reduce the ledger work
   needed to render the screens, and in practice only moved the same expensive
   work between requests or repeated it. The added routes, revision coordination,
   partial-loading states, and frontend assembly bought no meaningful latency
   improvement.
3. **Only caching optimization remains.** Revision-scoped derived data and
   bounded memoization are required. The rebaseline has now
   established that the 200 ms cold target is unreachable through the remaining
   request-local work: each of the two timeline holdings reports independently
   exceeds 300 ms, before the 1.20-second scope scan and roughly one second of
   XIRR work.
4. Neither parallelism nor further API decomposition is a lever. Running
   the two net-worth passes concurrently was implemented and measured as a 45%
   regression at the server's actual `-N`, because the passes are
   allocation-bound rather than CPU-bound.
5. A database or persistent derived store remains deferred until the simpler
   revision-scoped in-memory design has been measured.

Net-worth and cashflow report consolidation is complete. Net worth is now two
hledger passes (one consolidated asset/liability report and the gain report) and
cashflow is one, down from five and two.

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
- Further splitting the Net Worth or Investments APIs as a performance
  optimization. Both experiments were ineffective; projections may be separated
  later for product reasons only if they read shared cached data.

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
computation. The 2026-08-28 audit and rebaseline satisfy that condition for
in-memory revision-scoped derived data; a database remains deferred.

## Measured results

Cabal development build with `-O1`, run against the repository's 5,000- and
50,000-transaction portfolio fixtures. The audited measurements use the
threaded RTS at `-N1`, wall-clock mode, and force every application response
field. Every benchmark was selected and executed in its own process with
Tasty's worker count fixed to one. Baseline is commit `b424580` (`Speed up
investment performance reports`). These are local measurements and should be
treated as directional, not as portable service-level objectives.

| Fixture | Operation | Baseline | Audited now |
|---|---|---:|---:|
| 5,000 | Investment current summary | ~193 ms | ~179 ms |
| 5,000 | Investment monthly history, 24 periods | ~3.85 s | **~340 ms** |
| 5,000 | Investment latest-date drill-down | ~582 ms | **~291 ms** |
| 5,000 | Complete investments application use case | n/a | **~677 ms** |
| 5,000 | Net worth | ~315 ms | **~201 ms** |
| 5,000 | Cashflow | ~39 ms | **~32 ms** |
| 50,000 | Investment current summary | ~1.98 s | ~1.73 s |
| 50,000 | Investment monthly history, 24 periods | >45 s, timed out | **~2.72 s** |
| 50,000 | Investment latest-date drill-down | ~6.01 s | **~2.86 s** |
| 50,000 | Complete investments application use case | n/a | **~5.27 s** |
| 50,000 | Net worth | ~1.63 s | **~741 ms** |
| 50,000 | Cashflow | ~283 ms | **~196 ms** |

### Audited investment forcing and request attribution

`investmentsReportWeight` was checked field by field against both
`Application.Investments.InvestmentsReport` and the HTTP projection. It forces
metadata, request dimensions, the current summary, every timeline point,
breadcrumb, child summary, allocation and warning. `summaryWeight` forces all
eight fields of `InvestmentSummary`. The 5.27-second application result above is
therefore a complete in-memory application use case rather than another partial
lazy result.

The benchmark executable now uses the threaded RTS but defaults to `-N1`; the
server deliberately remains at unrestricted `-N`, since it must serve concurrent
requests. Measurements use an exact Tasty pattern and `-j1`, so cases execute one
at a time rather than heating the machine with a parallel benchmark group.

New stage benchmarks attribute the 50,000-transaction investment path:

| Stage | Time | Allocated |
|---|---:|---:|
| Prepare historical-cost context and sorted transactions | ~272 ms | 527 MB |
| Timeline market holdings report | ~314 ms | 769 MB |
| Timeline historical-cost holdings report | ~393 ms | 769 MB |
| Timeline scope scan | ~1.20 s | 3.6 GB |
| One XIRR solve | ~42 ms | 15 MB |
| Latest drill-down multi-scope scan, before relevance guard | ~2.73 s | 8.4 GB |
| Latest drill-down multi-scope scan, after relevance guard | **~1.64 s** | **5.1 GB** |

The relevance guard skips the posting, amount, conversion and accumulation work
for a transaction that neither touches a scope's portfolio nor matches one of
its performance mappings. The retained singular implementation and all 125
investment-performance regression cases agree with the optimized path. At the
application level the guard reduces allocation from 13 GB to 9.5 GB; serial
wall time is unchanged within variance. This is still useful for production's
allocation pressure, but it is not evidence of progress toward 200 ms.

The compute-first decision is now made. Even removing the scope scan and every
XIRR solve would leave preparation and two independently measured holdings
reports, each already above the target or close to twice it. The request-local
flow phase is closed; revision-scoped derived data is required for the
interactive target.

### Every net-worth figure in earlier revisions of this document was wrong

The net-worth rows above are not comparable with the ones this document carried
before 2026-08-27, which reported ~132 ms and ~698 ms. Those came from a
benchmark that did not measure the whole request.

`netWorthWeight` in `backend/benchmark/Main.hs` forced the timeline, both
breakdowns, the performance series and the incomplete-period count, but never
touched `currentAssets`, `currentLiabilities` or `currentNetWorth`. Those are
lazy `Maybe Decimal` fields, so the two strict current balance reports — 75% of
the request at 50,000 transactions — were never executed by the benchmark at
all. The tell was already visible in the component table: one component, the
strict current asset/cash balance, was reported at ~765 ms, which is *slower
than the whole ~752 ms request that supposedly contained it*. A component that
costs more than its own parent is a measurement bug, not a finding.

Every weight function now forces every field of the record it returns. The
honest baseline for this round of work is ~315 ms and ~1.63 s.

This is the third time measurement has corrected this document's priorities, and
the second time it has invalidated a reported result. The risk table entry about
optimizing from a reading of the code rather than a measurement should be read as
also covering *trusting a measurement without checking that it measures the whole
operation*.

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
| 5,000 | Complete investments application use case | ~677 ms | 991 MB | 42 MB | 78 MB |
| 50,000 | Complete investments application use case | ~5.27 s | 9.5 GB | 395 MB | 660 MB |
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
and independently measured component times are not additive. Component figures
also move by 10-20% with run order and heap state, so treat them as attribution
rather than as results.

These figures are superseded. They identified the strict current asset/cash
balance as the largest net-worth component, which was correct and led directly to
removing it from the request path entirely; it now survives only as a test oracle.
The current per-component figures are in the two completed sections below.

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

- one consolidated asset/cash/liability periodic multi-balance report; and
- one gain periodic multi-balance report.

The periodic reports provide timeline totals, selected-period breakdown rows and
the latest column from which strict current KPI values are projected.

hledger's `reportSpanHelper` derives the report span from the whole journal's
date span and only reads *date* terms out of the query, never account terms. All
two periodic reports therefore share an identical window.

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
shared traversal remains material, and the later relevance guard reduces the
drill-down result further to 2.86 seconds and 7.1 GB allocated. This completes
the request-local scope optimization work; it cannot by itself reach the
interactive target.

#### Completed: skip transactions irrelevant to an individual scope

The multi-scope sweep formerly called the complete accumulator step for every
transaction and every scope. Most asset transactions touch the root and one
immediate child, yet the other child scopes still folded every posting and
inspected every amount before contributing nothing.

`stepAccumulator` now returns immediately when a transaction neither touches the
scope's investment portfolio nor matches one of its selected performance
mappings. This is an exact semantic guard: in that case account discovery,
explicit-cost discovery, retained performance, attribution and external cash
flows are all unchanged. All 125 investment-performance tests, including the
multi-scope-to-singular oracle, pass.

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

#### Completed: derive the current KPIs from the final periodic column

This is the version of the task the section above prescribed — keep
`requireComplete` as an application-layer projection — carried to its
conclusion. Both standalone balance reports are gone from the request path.

Once the window alignment below truncates price directives at
`latestJournalDate`, the two valuation modes converge: `AtEnd` of the final
interval and `AtDate latestJournalDate` resolve the same price, because no price
exists between those dates to distinguish them. The final periodic column *is*
the latest balance. The only thing separating it from a KPI is the completeness
rule, and that is a projection over an already-computed column:

```haskell
strictLatest = maybe (complete 0) requireComplete . lastMaybe
```

Depth is not a confounder. hledger derives a report's totals row from the root
account's inclusive balance (`MultiBalanceReport.hs`, `generatePeriodicReport`),
so the totals are identical whether the report is clipped to depth 2 or not
clipped at all.

`currentAssetCashBalance` and `currentLiabilityBalance` remain in
`Internal/NetWorth.hs` as the correctness oracle and as component benchmarks,
following the same pattern as the retained singular investment path.
`NetWorthSpec` asserts the projected KPIs equal those functions across nine
journals x two granularities x two commodities, covering missing prices, foreign
cash, dust residuals, gapped price histories and nested mappings.

At 50,000 transactions this removed ~1.2 s and ~0.2 s of report work for a
measured **1.63 s -> 733 ms** on the request, with allocation halved from 1.6 GB
to 818 MB.

#### Completed: align the periodic window to the latest journal date

The monthly report-window query now carries the same explicit end date as the
yearly query. The report journal also excludes price directives after
`latestJournalDate`: hledger expands the final interval to its calendar
boundary, so narrowing only the query still allowed `AtEnd` valuation to see a
later price inside that month or year.

At this stage of the work the distinct strict current reports remained in place;
the KPI projection change in the preceding completed section later removed them
from the request path. A regression fixture pins
the boundary by providing one price on the final transaction date and another
later in the month; the periodic value and strict current value both use the
former.

Paired warm-build wall-clock measurements with `-O1` and a 10% benchmark
variance target were:

| Fixture | Before | After | Time reduction | Allocation reduction |
|---|---:|---:|---:|---:|
| 5,000 | ~133 ms | **~119 ms** | **10.5%** | **5.5%** (146 MB → 138 MB) |
| 50,000 | ~698 ms | **~662 ms** | **5.2%** | **3.3%** (779 MB → 753 MB) |

The timing deltas are directional and overlap the reported variance; the
allocation reductions are deterministic. Note that both figures were measured
with the incomplete `netWorthWeight` described above, so they describe the
periodic portion of the request rather than the whole request. The direction is
sound — this change only touches the periodic reports, which that benchmark did
force — but the percentages are not comparable with the table at the top.

#### Completed: share assets and liabilities in one report

Assets/cash and liabilities differed only by the account-type filter and the
requested display depth: both use `CalcChange`, `Historical` accumulation,
`AtEnd` valuation and `ALFlat`. They are now one report over
`[Asset, Cash, Liability]`, split back apart by account type.

Two facts from hledger's source shape the implementation, and both are the
opposite of what a reading of the report options suggests:

- **The totals row is not the sum of the displayed rows.** It is the root
  account's inclusive balance. A consolidated report's totals row therefore spans
  both sides and is useless per side, so `reportSide` sums the retained rows
  instead.
- **Depth clips the account names on the postings**, before aggregation and
  before valuation — it is not a display filter. So a clipped row's balance
  already includes everything beneath it, which is what makes summing rows exact
  at any depth; but it also means clipping genuinely reduces work.

The consolidated report is nonetheless built *unclipped*, because a clipped name
spanning both sides could not be attributed to either. `breakdownItemsFor`
re-collapses rows with `atDepth` itself, so it yields identical output from an
unclipped report.

`PeriodReportSpec` pins the equivalence: for nine journals x two granularities x
two commodities x three depths, each of the four sides must match its separately
filtered report on column totals, missing-commodity set, and breakdown items.
All 108 cases agree exactly.

The gain report stays separate. `CalcGain` is not a different accumulation but a
different valuation function — with `Historical` accumulation the balance
calculation is consulted inside `mixedAmountApplyValuationAfterSum`, not in the
accumulation step — so it cannot share a pass with `CalcChange`. Deriving it as
market value minus cost basis would need two reports anyway, so there is nothing
to win.

#### Why net worth costs about 3.8x cashflow, and why that is mostly inherent

At 50,000 transactions the audited threaded serial baseline is ~741 ms for net
worth and ~196 ms for cashflow. A 2x2 probe
over {balance accounts, flow accounts} x {`Historical`+`AtEnd`,
`PerPeriod`+`AtThen`} attributes the gap:

| Accounts | `Historical`+`AtEnd` | `PerPeriod`+`AtThen` |
|---|---:|---:|
| Asset/cash/liability | 268 ms, 324 MB | 147 ms, 109 MB |
| Revenue/gain/expense | 257 ms, 257 MB | 132 ms, 93 MB |

- **The account set is irrelevant** — 268 vs 257 ms at a fixed mode. The
  plausible story that asset accounts cost more because they hold many
  commodities while income and expenses are single-commodity is wrong.
- **The mode is the whole story** — ~1.9x time and ~3x allocation.
- **Net worth runs two passes to cashflow's one**, since `CalcGain` cannot share
  a pass. 2 x 1.9 = 3.8x, matching the audited ratio.

The mode difference is stock versus flow. Net worth accumulates from the start of
the journal, so it aggregates every posting ever recorded; a `PerPeriod` cashflow
report only aggregates postings inside the 24-month window. Both still *scan* the
whole journal to select and to build the price oracle, which is why the ratio is
1.9x rather than proportional to the window.

`AtEnd` is already the cheap choice and must not be "optimized" to a per-posting
valuation. Measured: `Historical` with `AtThen` costs **727 ms and 759 MB**, 2.4x
worse than `AtEnd`, because valuing every posting individually is far more work
than valuing (accounts x 24) accumulated balances after the sum.

One caveat on how these two numbers should be compared: the 50,000-transaction
fixture spans **601 months** and only **3.8% of its transactions fall in the last
24 months**. That is not a realistic personal journal, and it flatters cashflow.
On a five-year journal the monthly window would cover a large fraction of the
ledger and the gap would narrow considerably.

The practical consequence is that net worth has no valuation or consolidation
win left. Its remaining cost is two full-journal cumulative passes, so reaching
200 ms requires revision-scoped derived data.

#### Measured but deliberately declined: clipping the consolidated report

Clipping the consolidated report to the larger of the two requested depths is
faster: **362 ms unclipped vs 300 ms at depth 2** at 50,000 transactions, with
identical allocation and 24 MB less GC copying.

It is not implemented. Correct attribution would need a guard proving that no
clipped prefix spans both sides (a liability declared beneath `assets:` would be
silently merged into the asset row), plus a fallback path when the guard fails,
plus a synthetic fixture to test the mixed case. That is a branch, a second code
path and a new fixture for ~10% of one request. Recorded here so the option is
not rediscovered from scratch; take it if the 200 ms target makes 62 ms matter.

#### Measured and reverted: sharing the report window across reports

`periodicBalanceReport` derives its own report window, so a net-worth request
derives it twice (consolidated report, gain report). Hoisting it to be computed
once per request looked free — hledger reads only the date terms of a query when
deriving a report span, never the account-type terms, so one window is provably
valid for all of them.

It is a **regression**, measured twice and reverted both times:

| Variant | 50,000 net worth | Allocated |
|---|---:|---:|
| Per-report window (kept) | ~570-627 ms | 749 MB |
| Shared window | ~743-821 ms | 742 MB |
| Shared window and shared filtered journal | ~615-695 ms | 741 MB |

Allocation falls slightly in both variants while time rises, so this is not
extra work — it is that hoisting materializes the filtered journal and its
truncated price-directive list into a value retained across both reports, where
building it per report lets GHC fuse it with its consumer and collect it
promptly. The window derivation itself is too cheap to pay for that.

The general lesson is worth keeping: in this codebase, sharing a large
intermediate across report passes is not automatically cheaper than recomputing
it, and the difference does not show up in allocation figures.

#### Next phase: cache the base report, not the selected view

The compute-first audit has now met the entry condition for this cross-request
optimization. The same separation already applies inside one request: calculate
the base report once and project selected periods and depths from it without
rerunning hledger.

Selected asset/liability periods and display grouping are projections over a
periodic report. Cache the base rows and totals by:

```text
snapshot revision + parsed query + granularity + commodity + required depth
```

Changing the selected period should then select another column and regroup its
rows rather than rerun hledger. Where practical, cache at the maximum supported
depth and collapse to shallower depths in the application layer.

### 3. Next phase: revision-scoped derived data

The compute-on-every-request improvements have now been rebenchmarked against
the 200 ms target. The stage attribution shows that this phase is required.

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

### 4. Rejected: API decomposition and progressive rendering

Progressive API experiments were carried out for both analytics screens and
were completely ineffective as performance optimizations.

The Investments experiment split the response into structure, value, and
performance requests. The Net Worth experiment split the base timeline/KPI data
from selected-period breakdowns. Neither changed the underlying dependency
graph: the visible sections share the same expensive reports and investment
flows. Producing all data for a usable screen therefore still performed the
same total ledger work, while separate requests lost request-local sharing or
repeated report calculation.

The split also added costs outside the benchmarks: more HTTP and application
surface, revision matching across partial responses, partial-success and
loading states, and frontend response assembly. A faster empty or incomplete
first response was not a meaningful improvement to time-to-usable-screen.

Do not pursue another Net Worth or Investments endpoint split for performance.
If cached base reports later make a projection independently cheap, a route may
be separated for product or interaction reasons, but that is a consumer of the
cache rather than an optimization track of its own.

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
saving, but the flow-optimization phase is now closed because this cannot make
the cold request interactive.

### 6. Rejected: report-level backend parallelism

Parallel report execution is closed as an optimization track. The caching phase
still needs bounded worker concurrency for independent cache keys and background
rebuilds, but that is capacity control, not an attempt to accelerate one
allocation-heavy request.

#### Measured and rejected: running the two net-worth passes in parallel

The obvious candidate above — "the gain report alongside the balance report" —
was implemented and measured. It does not work, and it would have regressed
production.

The implementation was sound: `Control.Parallel.Strategies` inside the pure
`computeNetWorth`, the function split into independent balance and gain parts so
neither branch depends on the other, `rdeepseq` on the two small derived records
rather than on the reports themselves, and the join left sequential. Sparks
genuinely converted (`SPARKS: 6 (3 converted, 0 GC'd, 3 fizzled)`).

Measured on one binary, varying only the RTS capability count, 50,000
transactions:

| RTS | Net worth |
|---|---:|
| `-N1` | 713 ms |
| `-N2` | 684 ms |
| `-N4 -qg` | 757 ms |
| `-N -qg` (12 caps) | 777 ms |
| `-N` (12 caps) | **1.032 s** |

The best case is ~4% at exactly two capabilities, against a predicted ~2x on the
report passes. The control explains why: **cashflow, which contains no
parallelism at all, also slowed from 205 ms to 247 ms under `-N`.** The penalty
is garbage collection and scheduling, not the sparks.

These reports are allocation-bound rather than CPU-bound — 749 MB per net-worth
request. Running two concurrently doubles the allocation rate, and GHC's
stop-the-world collections then serialize precisely the two threads the change
was trying to overlap. Any estimate that models these passes as CPU-bound will
predict a speedup that does not exist.

The decisive fact is the last row. `hedger-server` is built with
`-threaded -rtsopts -with-rtsopts=-N`, so production uses every logical core —
exactly the 1.032 s configuration. Landing this would have made the net-worth
endpoint roughly 45% slower while every component benchmark still looked healthy.

Before attempting parallelism anywhere else in this codebase, measure the
allocation rate of the work being overlapped. Two allocation-heavy hledger passes
do not overlap.

### 7. Completed: use the threaded runtime without parallel benchmarks

`snapshot-bench` now links the threaded RTS and defaults to `-N1 -T`. Every
reported rebaseline selects one exact case, fixes Tasty to `-j1`, and executes it
in a separate process. This preserves the threaded runtime machinery while
preventing benchmark cases or capabilities from heating the machine together
and distorting later measurements.

`hedger-server` deliberately remains at `-threaded -rtsopts -with-rtsopts=-N`.
It is a concurrent web server, so unrestricted capabilities are preferred even
though a single allocation-heavy request can benchmark faster at `-N1`. The
serial benchmark is an intrinsic-work comparison baseline, not a claim about
production latency under concurrent load. Production capacity and tail latency
need a separate load test rather than a parallel microbenchmark suite.

## Task and impact estimate

| Priority | Task | Expected gain | Estimated effort |
|---:|---|---|---|
| 1 | **Complete:** share one flat report and one multi-scope sweep across the latest-date scopes | Latest drill-down is now 2.86 s and 7.1 GB allocated at 50,000 transactions | Completed |
| 2 | **Complete:** add an endpoint-level investments benchmark | Audited combined cold application path: ~5.27 s and 9.5 GB allocated | Completed |
| 3 | **Complete:** add net-worth component benchmarks | Identified the strict current asset/cash balance as the largest measured 50,000-transaction component | Completed |
| 4 | **Complete:** add cashflow component benchmarks | Measured the two 50,000-transaction periodic passes at ~105 ms and ~139 ms | Completed |
| 5 | **Complete:** align the net-worth periodic window to the latest journal date | 10.5% faster at 5,000 and 5.2% faster at 50,000 transactions in paired measurements | Completed |
| 6 | **Complete:** fix the benchmark forcing so a net-worth measurement covers the whole request | Corrected a 2.5x understatement; established the real baseline | Completed |
| 7 | **Complete:** derive the current KPIs from the final periodic column | Removed both strict balance reports: 1.63 s → 733 ms at 50,000 | Completed |
| 8 | **Complete:** consolidate asset and liability net-worth passes | Contributed to 733 ms → 670 ms; 406 ms → 287 ms on the reports alone | Completed |
| 9 | **Complete:** consolidate income and expense cashflow passes | 283 ms → 191 ms at 50,000; 258 ms → 139 ms on the reports alone | Completed |
| 10 | **Complete:** link the benchmark with the threaded RTS, pin it to `-N1`, run cases individually, and rebaseline | Reproducible serial baseline without cross-case thermal contention | Completed |
| 11 | **Decided:** retain unrestricted `-N` for the production web server | Prefer concurrent serving capacity; assess it with a separate load test | Completed |
| 12 | **Complete:** audit the investment benchmark weight functions | Every application and nested summary field is forced; 5.27-second result is verified | Completed |
| 13 | **Closed:** further XIRR/request-local flow optimization | Flow optimization phase is complete; this does not make the cold path interactive | No longer planned |
| 14 | **Complete:** add investment stage benchmarks and skip scope-irrelevant transactions | Multi-scope scan 2.73 s/8.4 GB → 1.64 s/5.1 GB; application allocation 13 GB → 9.5 GB | Completed |
| 15 | **Closed:** clip the consolidated report behind a type-preservation guard | 62 ms at 50,000, but adds a branch, fallback, and fixture without making the request interactive | No longer planned |
| 16 | **Rejected:** share the report window across reports | Measured regression | Not viable |
| 17 | **Rejected:** run the two net-worth passes in parallel | Measured 45% regression at the server's actual `-N`; the passes are allocation-bound, so they do not overlap. Any other parallelism candidate must first be shown allocation-light | Not viable |
| 18 | **Rejected:** split the Net Worth and Investments APIs | Implemented and measured as useless: no meaningful improvement to time-to-usable-screen, with duplicated/shared work and substantially more API/frontend complexity | Not viable |
| 19 | **Complete:** rebenchmark every cold analytics path and decide whether storage is required | The independently measured holdings reports already exceed 200 ms; derived data is required | Completed |
| 20 | **Next:** cache base reports separately from selected projections | High for repeated requests | Medium, 3–5 days |
| 21 | Add revision-scoped derived rebuilds, bounded memoization, and single-flight | Very high for common and concurrent views | High, 1–2 weeks |
| 22 | Future: evaluate a database or persistent derived store if the in-memory derived phase is insufficient | Unknown until that phase is measured | High |

Effort values describe focused implementation time, not calendar promises.

Items 2–4 were listed high deliberately and are now complete. Component
measurement corrected the priority order of this document twice; the complete
investment application path and every production net-worth and cashflow report
now have direct benchmark coverage.

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
| Frontend endpoint split increases duplicate work | Higher load without a meaningfully faster usable screen | Rejected as an optimization; do not revisit unless cached projections make routes independently cheap |
| Parallel reports saturate CPU or memory bandwidth | Worse tail latency | Central concurrency limit and load testing, not unbounded request fan-out |
| Old derived results remain after config changes | Incorrect classifications or targets | Include relevant configuration identity/version in derived generations and cache keys |
| Background failure hides new journal data indefinitely | Permanently stale dashboard | Expose stale/error metadata, retry safely, and log/measure derived rebuild failures |
| Optimizing from a reading of the code rather than a measurement | Effort spent on minor terms | Add the component benchmark first; this has already produced two wrong priority orders |
| Trusting a benchmark without checking that it forces the whole result | A reported result that is silently measuring a subset of the work | Force every field of the returned record; treat any component slower than its own parent as a measurement bug |
| Treating a serial microbenchmark as production concurrency evidence | Conclusions that do not transfer to the web server | Use threaded `-N1`, `-j1` for intrinsic comparisons and a separate unrestricted-`-N` load test for production capacity and tail latency |
| Assuming independent pure computations can be overlapped profitably | Wall-clock regressions under the server's `-N` | Measure the allocation rate first; allocation-bound passes serialize on GC regardless of capability count |

## Decisions to make during design

1. Whether a prepared derived revision is published together with the snapshot,
   or whether the snapshot is published immediately and reports fall back to
   on-demand calculation — and correspondingly whether revision events are
   deferred behind derived readiness.
2. Which views are common enough to rebuild eagerly: likely root monthly reports
   in the default display commodity, but production usage should confirm this.
3. The cache memory budget and whether it is count-, size-, or cost-based.
4. The worker limit on the target NixOS host and whether reload work has lower
   priority than interactive cache misses.
5. The acceptable freshness delay while common derived reports rebuild.

## Recommended next slice

The benchmark audit, flow optimization, serial threaded rebaseline, request
attribution, and progressive API experiments are complete. API splitting was
rejected for both Net Worth and Investments: it did not reduce the work required
for a usable screen. At 50,000 transactions net worth is ~741 ms and the complete
investments request is ~5.27 seconds. Independently measured hledger passes
already exceed the 200 ms target, so further request-local work cannot make every
cold request interactive.

The next slice is therefore the first revision-scoped derived-data iteration:

1. Choose atomic publication semantics for snapshot and derived revisions.
2. Cache expensive base reports rather than selected-period or selected-depth
   projections, keyed by every semantic input listed above.
3. Add single-flight calculation and a bounded cache before any eager rebuilds.
4. Benchmark cold miss, warm hit and concurrent identical miss one case at a
   time; use a separate unrestricted-`-N` server load test for concurrency.
5. Add derived-revision, duration, hit/miss and waiter observability.

Caching is the only remaining optimization phase. Further flow tuning, API
splitting, and parallel report execution are closed as separate tracks.
