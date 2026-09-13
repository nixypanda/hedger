# Analytics performance

_Status: flow optimization complete; API splitting rejected; caching designed and
not started · Last updated: 2026-08-31_

Index for the analytics performance work. The detail lives in three documents:

| Document | Covers | Status |
|---|---|---|
| [Flow optimization](analytics-performance-flow-optimization.md) | The completed request-local optimization phase: what changed, what it measured, and the hledger and measurement facts it established | Complete and closed |
| [Rejected approaches](analytics-performance-rejected.md) | Everything implemented or probed and then rejected, and why — API splitting, backend parallelism, shared report windows, and more | Closed |
| [Caching plan](analytics-performance-caching-plan.md) | The remaining phase: checkpointed folds over an immutable journal history, invalidated by a prefix hash chain. Includes the implementation checklist | Designed, not started |

## The problem

The `net-worth/query` and `investments/query` endpoints are CPU-bound after the
journal has been loaded into memory. Journal parsing is not on the request path:
Hedger keeps an immutable `Snapshot` in memory (`Infrastructure/SnapshotStore.hs`)
and reloads it after debounced filesystem events. The cost is repeated derivation
of reports from that snapshot — and, after a journal edit, repeated derivation of
*all of history* when only the last month changed.

## Goals

- Make the default Net Worth screen feel immediate for a 50,000-transaction
  journal.
- Make the whole Investments screen, not just its timeline, usable at 50,000
  transactions.
- Make calculation cost scale primarily with journal size, rather than journal
  size multiplied by the number of displayed periods and scopes — and, after an
  edit, with the recency of the edit rather than with journal size at all.
- Target less than 200 ms for each analytics request against the
  50,000-transaction fixture.
- Preserve the existing valuation, cost, completeness, warning, and XIRR
  semantics exactly.
- Keep journal reloads atomic: a response must be internally consistent with a
  single revision.
- Bound memory use and background CPU so caching does not make the service less
  predictable.

## Non-goals

- Introducing a database as the source of truth.
- Precomputing every possible free-form hledger query.
- Changing accounting semantics to obtain a faster result.
- Hiding slow calculations behind unbounded worker or request concurrency.
- Further splitting the Net Worth or Investments APIs as a performance
  optimization — both experiments were measured as ineffective.

## Where things stand at 50,000 transactions

Threaded RTS at `-N1`, one selected benchmark case per process, every response
field forced. Local and directional, not portable service-level objectives.

| Operation | Baseline | Now |
|---|---:|---:|
| Net worth | ~1.63 s | **~741 ms** |
| Cashflow | ~283 ms | **~196 ms** |
| Investment monthly history, 24 periods | >45 s, timed out | **~2.72 s** |
| Investment latest-date drill-down | ~6.01 s | **~2.86 s** |
| Complete investments application use case | n/a | **~5.27 s** |

Full tables, stage attribution and component figures are in
[flow optimization](analytics-performance-flow-optimization.md).

## Why the remaining phase is caching

The compute-on-every-request phase is finished and was rebenchmarked against the
200 ms target. It cannot get there: each of the two investment timeline holdings
reports independently exceeds 300 ms, before the 1.20-second scope scan and
roughly one second of XIRR work. Net worth's remaining cost is two full-journal
cumulative passes with no valuation or consolidation win left.

Three other tracks were closed by measurement rather than by opinion — API
decomposition, report-level parallelism, and sharing intermediates across report
passes. See [rejected approaches](analytics-performance-rejected.md).

What remains is that the app recomputes immutable history on every request and on
every journal edit. The [caching plan](analytics-performance-caching-plan.md)
exploits that: every expensive computation is a left fold over date-sorted
entries snapshotted at period ends, so historical snapshots are immutable under
append. Checkpoint the folds, key the checkpoints on a per-period prefix hash
chain, and a journal append recomputes one month instead of 601.

## Operational observability

Wanted across all of it — metrics or structured timings for:

- snapshot reload duration and result;
- derived rebuild duration, revision, and abandoned rebuilds;
- queued, running, cancelled, and failed background jobs;
- cache hits, misses, evictions, and single-flight waiters by report family;
- calculation duration by report stage;
- response revision and whether it was served stale or refreshing; and
- worker-pool saturation.

Keep to report family, normalized non-sensitive dimensions, request ID,
revision, and timing.

## Standing rules for anyone working in this area

These were each learned the hard way; the supporting measurements are in the
linked documents.

1. **Measure the component before optimizing it.** Reasoning from a reading of
   the code has produced a wrong priority order twice.
2. **Force every field of a benchmarked record.** A benchmark that skipped lazy
   fields hid 75% of the net-worth request and reported a 2.5x understatement for
   weeks. Treat any component slower than its own parent as a measurement bug.
3. **Measure the allocation rate before attempting parallelism.** These reports
   are allocation-bound, not CPU-bound; two hledger passes do not overlap, and
   the server runs at unrestricted `-N`.
4. **Sharing a large intermediate across passes is not automatically cheaper
   than recomputing it**, and the difference does not appear in allocation
   figures.
5. **A serial microbenchmark is not concurrency evidence.** Use `-N1`, `-j1` for
   intrinsic comparisons and a separate unrestricted-`-N` load test for
   production capacity and tail latency.
