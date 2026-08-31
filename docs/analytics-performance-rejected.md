# Analytics performance: rejected and closed approaches

_Status: closed · Last updated: 2026-08-31_

Part of the [analytics performance index](analytics-performance-plan.md).

Every approach below was either implemented and measured, or measured as a
probe, and then rejected. This document exists so none of them is rediscovered
and re-attempted from scratch. Each entry records what was tried, what it
measured, and the reason it does not work — the reason matters more than the
verdict, because several of these fail for reasons that also constrain the
[caching plan](analytics-performance-caching-plan.md).

## Summary

| Approach | Verdict | Reason in one line |
|---|---|---|
| Split the Net Worth and Investments APIs | **Rejected** | Did not reduce the work a usable screen needs; moved or duplicated it |
| Progressive rendering on top of that split | **Rejected** | A faster empty first response is not a faster time-to-usable-screen |
| Run the two net-worth passes in parallel | **Rejected** | Measured 45% regression at the server's actual `-N`; the passes are allocation-bound |
| Share the report window across reports | **Rejected** | Measured regression; retaining the intermediate defeats fusion |
| Frontend parallelism as the optimization | **Rejected** | Reduces idle wall-clock only; raises total CPU, allocation and GC pressure |
| Clip the consolidated report | **Declined** | Real 62 ms win, but needs a guard, a fallback and a fixture |
| Further XIRR / request-local flow tuning | **Closed** | Cannot make the cold path interactive; the safe sharing is only half the win |
| Substitute the periodic total for the strict current KPI | **Rejected** | Collapses strict-KPI into lenient-KPI; observable semantic change |
| Another raw-journal cache layer | **Rejected** | Parsing is already not on the request path |
| Eagerly precompute arbitrary hledger queries | **Rejected** | Unbounded request space |
| Incremental recompute keyed on the journal revision | **Superseded** | Correct objection, wrong conclusion — see the note at the end |

## Rejected: API decomposition and progressive rendering

Progressive API experiments were carried out for both analytics screens and were
completely ineffective as performance optimizations.

The Investments experiment split the response into structure, value, and
performance requests. The Net Worth experiment split the base timeline/KPI data
from selected-period breakdowns. Neither changed the underlying dependency
graph: the visible sections share the same expensive reports and investment
flows. Producing all data for a usable screen therefore still performed the same
total ledger work, while separate requests lost request-local sharing or
repeated report calculation.

The split also added costs outside the benchmarks: more HTTP and application
surface, revision matching across partial responses, partial-success and loading
states, and frontend response assembly. A faster empty or incomplete first
response was not a meaningful improvement to time-to-usable-screen.

**Do not pursue another Net Worth or Investments endpoint split for
performance.** If cached base reports later make a projection independently
cheap, a route may be separated for product or interaction reasons, but that is a
consumer of the cache rather than an optimization track of its own.

## Rejected: report-level backend parallelism

Parallel report execution is closed as an optimization track. The caching phase
still needs bounded worker concurrency for independent cache keys and background
rebuilds, but that is capacity control, not an attempt to accelerate one
allocation-heavy request.

The obvious candidate — the gain report alongside the balance report — was
implemented and measured. It does not work, and it would have regressed
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

The decisive fact is the last row. `hedger-server` is built with `-threaded
-rtsopts -with-rtsopts=-N`, so production uses every logical core — exactly the
1.032 s configuration. Landing this would have made the net-worth endpoint
roughly 45% slower while every component benchmark still looked healthy.

**Before attempting parallelism anywhere else in this codebase, measure the
allocation rate of the work being overlapped.** Two allocation-heavy hledger
passes do not overlap. This constrains the caching plan directly: background
rebuilds must be bounded and are capacity control, not latency reduction.

## Rejected: sharing the report window across reports

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

Allocation falls slightly in both variants while time rises, so this is not extra
work — it is that hoisting materializes the filtered journal and its truncated
price-directive list into a value retained across both reports, where building it
per report lets GHC fuse it with its consumer and collect it promptly. The window
derivation itself is too cheap to pay for that.

**The general lesson is the most important one in this document: in this
codebase, sharing a large intermediate across report passes is not automatically
cheaper than recomputing it, and the difference does not show up in allocation
figures.** The caching plan is therefore required to cache compact projected
values — `Decimal`, `Set Text`, small strict records — and never to retain an
hledger `MultiBalanceReport`, a filtered `Journal`, or anything closing over one.

## Rejected: frontend parallelism as the optimization

Frontend parallelism by itself is not a sufficient optimization. It can reduce
wall-clock latency on an idle multicore server, but the remaining computations
would still repeat full journal scans. Under concurrent use this raises total
CPU, allocation, garbage collection, and memory-bandwidth pressure.

A frontend response cache is a different and useful thing; it is in the caching
plan rather than here.

## Measured but deliberately declined: clipping the consolidated report

Clipping the consolidated report to the larger of the two requested depths is
faster: **362 ms unclipped vs 300 ms at depth 2** at 50,000 transactions, with
identical allocation and 24 MB less GC copying.

It is not implemented. Correct attribution would need a guard proving that no
clipped prefix spans both sides (a liability declared beneath `assets:` would be
silently merged into the asset row), plus a fallback path when the guard fails,
plus a synthetic fixture to test the mixed case. That is a branch, a second code
path and a new fixture for ~10% of one request.

Recorded so the option is not rediscovered from scratch. Take it if 62 ms ever
matters; it does not while the request is 741 ms and the target is 200 ms.

## Closed: further XIRR and request-local flow optimization

After the shared-traversal work, XIRR is roughly 43 ms per solve at 50,000
transactions. Sharing grid evaluations across the 24 nested prefixes would remove
most of the remaining ~1 second, but the clean formulation is **not numerically
safe** and should not be attempted casually.

`npv` subtracts a per-prefix `reference` from each year fraction *before*
exponentiating, which keeps every exponent at or below zero. Factoring that
reference out so prefix sums can be shared reintroduces intermediates on the
order of `exp(690)` for long histories at the -99.9999% end of the search
interval, which overflows to infinity where the current code stays finite. That
would change `AmbiguousRates` and `NoConvergentRate` outcomes on exactly the
cases `backend/test/fixtures/investment-ambiguous-xirr.journal` exists to pin.

Only the `logRate >= 0` half of the grid, where the reference is zero for every
prefix, shares exactly and safely — about half the available saving.

This track is closed because the caching plan removes the same cost without
touching the solver: a solved rate is cached per checkpointed period, so an
append re-solves one period instead of twenty-four. Do not reopen the algebraic
sharing.

## Rejected: substituting the periodic total for the strict current KPI

Deriving the current KPI from the final periodic column *is* implemented, but
only alongside an application-layer `requireComplete` projection. Substituting
the periodic total directly collapses strict-KPI into lenient-KPI, because
`reportTotals` returns a partial sum plus a `missing` set where
`currentReportValue` returns `Nothing` for any unconvertible commodity. That is
an observable change and contradicts the completeness goal. See the completed
work in [flow optimization](analytics-performance-flow-optimization.md).

## Rejected: another raw-journal cache layer

The backend already caches the parsed journal: `SnapshotStore` holds an immutable
`Snapshot` in an `IORef`, `JournalWatcher` reloads it on debounced filesystem
events, and a failed reload retains the last successful snapshot. Parsing is not
on the request path, so another raw-journal cache would not improve these
endpoints at all. The useful cache layer contains data *derived* from a snapshot.

## Rejected: eagerly precomputing arbitrary hledger queries

The free-form query space is unbounded, so it must never be eagerly
precomputed. Query-keyed results may be cached lazily, bounded, and evicted.

## Superseded: incremental recompute keyed on the journal revision

An earlier assessment rejected incremental recomputation outright, on the grounds
that the snapshot revision is an MD5 over journal file contents and so offers no
delta to work from, and that a single added price directive invalidates
valuation globally.

The first half of that objection is correct and the conclusion drawn from it was
wrong. The fix is to stop using a whole-journal hash as the invalidation key and
use a per-period prefix hash chain instead. The second half survives as a real
constraint that the design must handle: a price directive inserted with a
*historical* date does invalidate every column from that date forward, so the
chain must hash price directives by directive date alongside transactions.

The corrected design is the [caching plan](analytics-performance-caching-plan.md).
