# Analytics flow optimization: completed work

_Status: complete and closed · Last updated: 2026-08-31_

Part of the [analytics performance index](analytics-performance-plan.md). This
document records the request-local optimization phase: what was changed, what it
measured, and the facts about hledger and about measurement that the work
established. Rejected and closed approaches are in
[rejected approaches](analytics-performance-rejected.md); the remaining work is
in the [caching plan](analytics-performance-caching-plan.md).

## Scope of this phase

The `net-worth/query` and `investments/query` endpoints are CPU-bound after the
journal has been loaded into memory. Journal parsing is not part of the normal
request path: Hedger keeps an immutable `Snapshot` in memory and reloads it
after filesystem events. The performance problem was repeated derivation of
reports from that snapshot.

The phase deliberately computed the answer on every request. Improvements had to
reduce the intrinsic cost of one request:

- consolidate compatible hledger reports;
- share journal traversals across periods and scopes;
- build request-local prepared data and indexes once, then reuse them within the
  request;
- remove repeated conversion, grouping, sorting, and allocation;
- avoid calculating response sections that the request does not need; and
- add component and endpoint benchmarks before and after each material change.

Cross-request caching was deferred until this was done, so that measurements
would show the residual work storage must address rather than allowing a cache
to conceal avoidable computation. That condition is now satisfied.

## Result

At 50,000 transactions the monthly investment history went from exceeding a
45-second benchmark timeout to 2.72 seconds. The latest-date drill-down went
from 6.01 to 2.86 seconds, with allocation falling from 16 GB to 7.1 GB. The
complete investments application request is 5.27 seconds and allocates 9.5 GB.
Net worth went from 1.63 s to 741 ms and cashflow from 283 ms to 196 ms.

The 200 ms cold target was not reached and cannot be reached by this kind of
work. Each of the two timeline holdings reports independently exceeds 300 ms,
before the 1.20-second scope scan and roughly one second of XIRR work. That
finding is what closed this phase and opened the caching phase.

## Measured results

Cabal development build with `-O1`, run against the repository's 5,000- and
50,000-transaction portfolio fixtures. The audited measurements use the threaded
RTS at `-N1`, wall-clock mode, and force every application response field. Every
benchmark was selected and executed in its own process with Tasty's worker count
fixed to one. Baseline is commit `b424580` (`Speed up investment performance
reports`). These are local measurements and should be treated as directional,
not as portable service-level objectives.

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

### Stage attribution for the 50,000-transaction investment path

| Stage | Time | Allocated |
|---|---:|---:|
| Prepare historical-cost context and sorted transactions | ~272 ms | 527 MB |
| Timeline market holdings report | ~314 ms | 769 MB |
| Timeline historical-cost holdings report | ~393 ms | 769 MB |
| Timeline scope scan | ~1.20 s | 3.6 GB |
| One XIRR solve | ~42 ms | 15 MB |
| Latest drill-down multi-scope scan, before relevance guard | ~2.73 s | 8.4 GB |
| Latest drill-down multi-scope scan, after relevance guard | **~1.64 s** | **5.1 GB** |

This table is the input to the caching plan: it is the list of folds that plan
proposes to checkpoint.

### Net-worth and cashflow component attribution

Measured before the consolidation work below removed several of these from the
request path. Retained because they attribute where the cost sits, and because
`currentAssetCashBalance` and `currentLiabilityBalance` still exist as
correctness oracles and component benchmarks.

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

### Drill-down allocation

The drill-down baseline is the retained `summaryAtPrepared` reference path on the
current code, not commit `b424580`. Both sides prepare one shared context and
calculate the same deduplicated set of root, immediate-child, and configured
allocation scopes. This isolates the change made by the shared-traversal work.

| Fixture | Drill-down path | Allocated | Copied | Peak memory |
|---|---|---:|---:|---:|
| 5,000 | Singular reference | 1.6 GB | 46 MB | 93 MB |
| 5,000 | Shared reports and traversal | **1.0 GB** | **28 MB** | 93 MB |
| 50,000 | Singular reference | 16 GB | 465 MB | 858 MB |
| 50,000 | Shared reports and traversal | **10 GB** | **253 MB** | 858 MB |

## Completed work items

### Share one pass across the latest-date scopes

Holdings are additive over account prefixes. One *flat* balance report at the
latest date yields per-account rows, and each scope's holdings is the sum of the
rows beneath its prefix. So a single pair of reports — market and historical
cost — serves the root, every immediate child, and every allocation scope,
instead of two reports per scope.

The non-holdings work is not additive across scopes, and this is the constraint
that shapes the design. Scope enters the predicate, not just the filter: for
`scope = Just "assets:equity"` a transfer from `assets:liquid` is an external
contribution, whereas for the root scope `assets:liquid` is itself an
asset-class account so the same transfer cancels to zero. The same asymmetry
appears in `transactionMatchesScope` and `mappedAssetScope`. Per-scope results
therefore cannot be derived by summing child aggregates.

The implemented sweep carries one accumulator set per requested scope and
updates all of them in a single traversal. Cost is one pass times the number of
scopes' predicate evaluations, rather than one full traversal per scope.

The timeline sweep does not have this problem: it only ever needs the single
selected scope.

The retained singular path remains as a benchmark and correctness oracle.
Fixture regressions compare every summary field and warning across root, nested,
duplicate, missing-price, missing-cost, foreign-cost, mapping, fee/gain, and
ambiguous-XIRR cases. No accounting-semantic divergence was found.

### Skip transactions irrelevant to an individual scope

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

At the application level the guard reduced allocation from 13 GB to 9.5 GB;
serial wall time was unchanged within variance. Useful for allocation pressure,
but not evidence of progress toward 200 ms.

The relevance predicate is reused by the caching plan, as the basis for scoping
invalidation to the report families a change can actually affect.

### Derive the current KPIs from the final periodic column

An earlier version of the plan proposed deriving current KPI values from the
final periodic column and deleting the two standalone balance reports. Reading
the code showed they were not equivalent, in two independent ways:

- **Valuation date.** `periodicBalanceReport` uses `AtEnd`, valuing each column
  at that period's end — the last day of the calendar month or year containing
  the latest transaction. `currentReportValue` uses `AtDate` at
  `latestJournalDate`. Any price directive between those dates changes the
  result.
- **Completeness semantics.** `currentReportValue` ends in `requireComplete`, so
  it yields `Nothing` when any commodity is unconvertible. `reportTotals` uses
  plain `conversionFromMixed`, which returns a partial sum plus a `missing` set.
  `Analytics.useCurrent` then deliberately falls back to the lenient
  periodic-derived KPI only when the strict value is absent.

The window alignment below removes the first objection: once price directives are
truncated at `latestJournalDate`, `AtEnd` of the final interval and `AtDate
latestJournalDate` resolve the same price, because no price exists between those
dates to distinguish them. The final periodic column *is* the latest balance.
The only thing separating it from a KPI is the completeness rule, which is a
projection over an already-computed column:

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

### Align the periodic window to the latest journal date

The monthly report-window query now carries the same explicit end date as the
yearly query. The report journal also excludes price directives after
`latestJournalDate`: hledger expands the final interval to its calendar
boundary, so narrowing only the query still allowed `AtEnd` valuation to see a
later price inside that month or year.

A regression fixture pins the boundary by providing one price on the final
transaction date and another later in the month; the periodic value and strict
current value both use the former.

Paired warm-build wall-clock measurements with `-O1` and a 10% benchmark
variance target were:

| Fixture | Before | After | Time reduction | Allocation reduction |
|---|---:|---:|---:|---:|
| 5,000 | ~133 ms | **~119 ms** | **10.5%** | **5.5%** (146 MB → 138 MB) |
| 50,000 | ~698 ms | **~662 ms** | **5.2%** | **3.3%** (779 MB → 753 MB) |

The timing deltas are directional and overlap the reported variance; the
allocation reductions are deterministic. Both figures were measured with the
incomplete `netWorthWeight` described below, so they describe the periodic
portion of the request rather than the whole request. The direction is sound —
this change only touches the periodic reports, which that benchmark did force —
but the percentages are not comparable with the table at the top.

This truncation is a load-bearing precondition for the caching plan's claim that
historical valuations are stable under appended price directives.

### Share assets and liabilities in one report

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

Contribution: 733 ms → 670 ms on the request; 406 ms → 287 ms on the reports
alone.

### Consolidate income and expense cashflow passes

283 ms → 191 ms at 50,000 transactions; 258 ms → 139 ms on the reports alone.

Note for the caching plan: unlike net worth, the cashflow reports are built
*depth-clipped* (`Just depth` in `Internal/Cashflow.hs`), so any cache over them
must either key on depth or cache unclipped and collapse in the application
layer.

### Use the threaded runtime without parallel benchmarks

`snapshot-bench` now links the threaded RTS and defaults to `-N1 -T`. Every
reported rebaseline selects one exact case, fixes Tasty to `-j1`, and executes it
in a separate process. This preserves the threaded runtime machinery while
preventing benchmark cases or capabilities from heating the machine together and
distorting later measurements.

`hedger-server` deliberately remains at `-threaded -rtsopts -with-rtsopts=-N`.
It is a concurrent web server, so unrestricted capabilities are preferred even
though a single allocation-heavy request can benchmark faster at `-N1`. The
serial benchmark is an intrinsic-work comparison baseline, not a claim about
production latency under concurrent load. Production capacity and tail latency
need a separate load test rather than a parallel microbenchmark suite.

### Audit the investment benchmark weight functions

`investmentsReportWeight` was checked field by field against both
`Application.Investments.InvestmentsReport` and the HTTP projection. It forces
metadata, request dimensions, the current summary, every timeline point,
breadcrumb, child summary, allocation and warning. `summaryWeight` forces all
eight fields of `InvestmentSummary`. The 5.27-second application result is
therefore a complete in-memory application use case rather than a partial lazy
result.

## Why net worth costs about 3.8x cashflow, and why that is mostly inherent

At 50,000 transactions the audited threaded serial baseline is ~741 ms for net
worth and ~196 ms for cashflow. A 2x2 probe over {balance accounts, flow
accounts} x {`Historical`+`AtEnd`, `PerPeriod`+`AtThen`} attributes the gap:

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
win left. Its remaining cost is two full-journal cumulative passes.

## Every net-worth figure in earlier revisions was wrong

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

This is the third time measurement corrected this document's priorities, and the
second time it invalidated a reported result. The risk about optimizing from a
reading of the code rather than a measurement should be read as also covering
*trusting a measurement without checking that it measures the whole operation*.

## Lessons to carry forward

1. **Measure the component before optimizing it.** Three rounds of prior
   reasoning about which parts were expensive were wrong. The dominant per-day
   cost was not either hledger balance report; it was the hand-rolled
   `retainedPerformanceAtWith` scan, at 632 ms and 2.3 GB allocated per call,
   because it iterated mappings in the outer generator and so rescanned the
   journal once per mapping.
2. **Force every field of a benchmarked record**, and treat any component slower
   than its own parent as a measurement bug.
3. **Sharing a large intermediate across report passes is not automatically
   cheaper than recomputing it**, and the difference does not show up in
   allocation figures. See the reverted shared-window experiment in
   [rejected approaches](analytics-performance-rejected.md).
4. **Depth clipping is not a display filter** in hledger; it changes the
   postings before aggregation and valuation.
5. **The totals row of a multi-balance report is the root account's inclusive
   balance**, not the sum of displayed rows.
6. The XIRR solver evaluates NPV at 1025 fixed grid points per solve, so it cost
   394 ms at 50,000 flows before its per-evaluation constants were hoisted, and
   43 ms after.

## Deliberate unobservable divergence

The sweep sorts transactions by date, so external cash flows come out
date-ordered rather than in journal parse order. hledger's `journalFinalise`
restores parse order and never sorts. The reordering cannot be observed because
the only consumer is `Xirr.xirr`, whose `normalise` groups flows by day before
solving. Preserve this rather than rediscovering it.
