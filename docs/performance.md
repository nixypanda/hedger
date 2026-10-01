# Performance and benchmarking

This document records how Hedger measures ledger performance, what the numbers
currently are, the hledger facts the work established, and the approaches that
were already measured and rejected. It exists so that none of those approaches
is rediscovered and re-attempted from scratch.

Performance numbers here are local, directional measurements, not portable
service-level objectives. They move by 10–20% with run order and heap state and
will drift with toolchain and fixture changes.

## The benchmark suite

Benchmarks live in `backend/benchmark/Main.hs` as the Cabal suite
`snapshot-bench`, written with `tasty-bench`. Every generation and parse step is
forced in `env` setup, outside the timed action, so a benchmark measures the
ledger operation and not fixture construction.

The suite reports at two sizes:

| Fixture | Path | Transactions |
|---------|------|--------------|
| `medium-5000` | `examples/performance/india-us-portfolio-medium-5000.journal` | 5,000 |
| `large-50000` | `examples/performance/india-us-portfolio-large-50000.journal` | 50,000 |

Large journals are external fixture files rather than embedded Haskell.

Benchmark groups cover:

- `parse` — journal load and force.
- `query` — account and boolean hledger-query matching with pagination.
- `investments` — current summary, drill-down (current and retained singular
  oracle), and 24-period monthly summaries.
- `application` — the complete in-memory investments use case.
- `investment-parts` — context preparation, timeline holdings, scope scans,
  holdings, retained performance, attribution, external cash flows, and one XIRR
  solve.
- `analytics` — net worth, cashflow, and asset/liability activity.
- `cashflow-parts` and `networth-parts` — the individual reports, including the
  retained strict-current-balance oracles.
- `accounts` — account tree and register.
- `xirr` — the solver on journal-derived flows.
- `transactions` — DTO projection and JSON encoding of a 200-row page.

Correctness is covered by properties and fixtures, not by the benchmarks; a
benchmark is not a correctness test.

## Running benchmarks

Inside the dev shell:

```console
cd backend && cabal bench all
```

The suite links the threaded runtime and defaults to `-N1 -T` with Tasty fixed
to `-j1` (see the `snapshot-bench` stanza in `hedger-backend.cabal`). To compare
one case honestly, select it and run it in its own process.

## Measurement discipline

These rules were each learned the hard way.

1. **Measure the component before optimizing it.** Reasoning from a reading of
   the code produced the wrong priority order three times.
2. **Force every field of a benchmarked record.** `netWorthWeight` once forced
   the timeline and breakdowns but never the lazy `currentAssets`,
   `currentLiabilities`, or `currentNetWorth` fields, so the two strict current
   balance reports — about 75% of the request at 50,000 transactions — were
   never executed. The tell was a component reported as slower than the whole
   request that supposedly contained it. Treat any component slower than its own
   parent as a measurement bug.
3. **Measure the allocation rate before attempting parallelism.** These reports
   are allocation-bound, not CPU-bound; two hledger passes do not overlap, and
   the production server runs at unrestricted `-N`.
4. **A serial microbenchmark is not concurrency evidence.** Use `-N1`, `-j1`
   for intrinsic comparisons and a separate unrestricted-`-N` load test for
   capacity and tail latency.
5. **Sharing a large intermediate across passes is not automatically cheaper
   than recomputing it**, and the difference does not show up in allocation
   figures.

## Reference numbers

Directed before/after of the request-local optimization phase, at the two
fixture sizes:

| Fixture | Operation | Baseline | Now |
|---------|-----------|---------:|----:|
| 5,000 | Net worth | ~315 ms | ~201 ms |
| 5,000 | Cashflow | ~39 ms | ~32 ms |
| 5,000 | Investment monthly history, 24 periods | ~3.85 s | ~340 ms |
| 5,000 | Investment latest-date drill-down | ~582 ms | ~291 ms |
| 5,000 | Complete investments use case | — | ~677 ms |
| 50,000 | Net worth | ~1.63 s | ~741 ms |
| 50,000 | Cashflow | ~283 ms | ~196 ms |
| 50,000 | Investment monthly history, 24 periods | >45 s, timed out | ~2.72 s |
| 50,000 | Investment latest-date drill-down | ~6.01 s | ~2.86 s |
| 50,000 | Complete investments use case | — | ~5.27 s |

### Stage attribution at 50,000 transactions

| Stage | Time | Allocated |
|-------|-----:|----------:|
| Prepare historical-cost context and sorted transactions | ~272 ms | 527 MB |
| Timeline market holdings report | ~314 ms | 769 MB |
| Timeline historical-cost holdings report | ~393 ms | 769 MB |
| Timeline scope scan | ~1.20 s | 3.6 GB |
| One XIRR solve | ~42 ms | 15 MB |
| Latest drill-down multi-scope scan, before relevance guard | ~2.73 s | 8.4 GB |
| Latest drill-down multi-scope scan, after relevance guard | ~1.64 s | 5.1 GB |

This table is the input to the caching design: it lists the folds a cache would
checkpoint.

### Why net worth costs about 3.8x cashflow

A 2x2 probe over `{balance accounts, flow accounts} x {Historical+AtEnd,
PerPeriod+AtThen}` shows the account set is irrelevant (268 vs 257 ms at fixed
mode) while the mode is the whole story (~1.9x time). Net worth runs two passes
to cashflow's one because `CalcGain` cannot share a pass, giving 2 x 1.9 = 3.8x.
Net worth has no valuation or consolidation win left; its remaining cost is two
full-journal cumulative passes.

## hledger facts learned the hard way

- **Depth clipping is not a display filter.** It clips account names on the
  postings *before* aggregation and valuation. A clipped row's balance already
  includes everything beneath it, which is what makes summing rows exact at any
  depth — and clipping genuinely reduces work.
- **A multi-balance totals row is the root account's inclusive balance**, not
  the sum of the displayed rows. On a consolidated report it spans both sides and
  is useless per side, so per-side totals are the sum of retained rows.
- **`CalcGain` is not a different accumulation but a different valuation
  function**, so it cannot share a pass with `CalcChange`. Deriving it as market
  value minus cost would need two reports anyway.
- **`AtEnd` is much cheaper than `AtThen`.** Valuing each accumulated column
  after the sum beat valuing every posting individually by 2.4x (727 ms vs
  ~303 ms at 50,000).
- **The periodic window is truncated at the latest journal date**, and price
  directives after that date are excluded from the report journal, so `AtEnd` of
  the final interval and `AtDate latestJournalDate` resolve the same price.
- **The XIRR solver evaluates NPV at 1025 fixed grid points per solve.** Hoisting
  per-evaluation constants took one solve from ~394 ms to ~43 ms at 50,000 flows.
- **A deliberate date-ordering divergence.** The multi-scope sweep sorts
  transactions by date, so external cash flows come out date-ordered rather than
  in journal parse order. This is unobservable because the only consumer,
  `Xirr.xirr`, groups flows by day before solving.

## What did not work

Each entry was implemented or measured and then rejected. The reason matters
more than the verdict, because several constrain the caching design.

| Approach | Verdict | Reason in one line |
|----------|---------|--------------------|
| Split the Net Worth and Investments APIs | Rejected | Did not reduce the work a usable screen needs; moved or duplicated it |
| Progressive rendering on top of that split | Rejected | A faster empty first response is not a faster time-to-usable-screen |
| Run the two net-worth passes in parallel | Rejected | Measured ~45% regression at the server's actual `-N`; allocation-bound |
| Share the report window across reports | Rejected | Measured regression; retaining the intermediate defeats fusion |
| Frontend parallelism as the optimization | Rejected | Reduces idle wall-clock only; raises total CPU, allocation, and GC pressure |
| Clip the consolidated report | Declined | Real 62 ms win, but needs a guard, a fallback, and a fixture |
| Further XIRR / request-local flow tuning | Closed | Cannot make the cold path interactive; safe sharing is only half the win |
| Substitute the periodic total for the strict current KPI | Rejected | Collapses strict-KPI into lenient-KPI; an observable semantic change |
| Another raw-journal cache layer | Rejected | Parsing is already off the request path |
| Eagerly precompute arbitrary hledger queries | Rejected | The query space is unbounded |
| Incremental recompute keyed on the whole journal revision | Superseded | Correct objection, wrong conclusion — see below |

### Parallel report execution regresses

Parallelizing the gain report against the balance report was implemented and
measured on one binary, varying only the RTS capability count at 50,000
transactions:

| RTS | Net worth |
|-----|----------:|
| `-N1` | 713 ms |
| `-N2` | 684 ms |
| `-N4 -qg` | 757 ms |
| `-N -qg` (12 caps) | 777 ms |
| `-N` (12 caps) | **1.032 s** |

The best case is ~4% at exactly two capabilities. The control explains why:
cashflow, which contains no parallelism, also slowed from 205 ms to 247 ms under
`-N`. These reports are allocation-bound (749 MB per net-worth request), so
concurrent execution doubles the allocation rate and stop-the-world collections
serialize the two threads. `hedger-server` is built with `-with-rtsopts=-N`, so
production runs the 1.032 s configuration — landing this would have made the
endpoint roughly 45% slower while every component benchmark still looked
healthy.

### Sharing the report window regresses

Hoisting the report window so it is derived once per request instead of twice
looked free, but it is a regression, measured twice and reverted both times:

| Variant | 50,000 net worth | Allocated |
|---------|-----------------:|----------:|
| Per-report window (kept) | ~570–627 ms | 749 MB |
| Shared window | ~743–821 ms | 742 MB |
| Shared window and shared filtered journal | ~615–695 ms | 741 MB |

Allocation falls slightly while time rises, so this is not extra work:
materializing the filtered journal and its truncated price directives into a
value retained across both reports defeats GHC's fusion and prompt collection.
The general lesson is the most important one here: **sharing a large
intermediate across report passes is not automatically cheaper than recomputing
it, and the difference does not show up in allocation figures.** A cache must
hold compact projected values — `Decimal`, `Set Text`, small strict records —
never an hledger `MultiBalanceReport`, a filtered `Journal`, or anything closing
over one.

### Substituting the periodic total for the strict KPI is an observable change

Deriving the current KPI from the final periodic column is implemented, but only
alongside an application-layer `requireComplete` projection. Substituting the
periodic total directly collapses strict-KPI into lenient-KPI, because
`reportTotals` returns a partial sum plus a `missing` set where
`currentReportValue` returns `Nothing` for any unconvertible commodity.

### Algebraic XIRR grid sharing is not numerically safe

Sharing grid evaluations across the 24 nested prefixes would remove most of the
remaining ~1 s of XIRR work, but the clean formulation reintroduces intermediates
on the order of `exp(690)`, which overflow to infinity where the current code
stays finite. That would change `AmbiguousRates` and `NoConvergentRate` outcomes
on exactly the cases `investment-ambiguous-xirr.journal` exists to pin. Only the
`logRate >= 0` half of the grid shares safely. Caching a solved rate per
checkpointed period removes the same cost without touching the solver.

### The whole-revision invalidation objection was wrong

An earlier assessment rejected incremental recomputation because the snapshot
revision is an MD5 over journal file contents and so offers no delta. That
premise is right but the conclusion is wrong: the fix is to stop using a
whole-journal hash as the invalidation key and use a per-period prefix hash chain
instead. The surviving constraint is that a price directive inserted with a
historical date invalidates every column from that date forward, so the chain
must hash price directives by directive date alongside transactions.

## The 200 ms target and caching

The request-local phase is complete and cannot reach a 200 ms cold target: each
of the two investment timeline holdings reports independently exceeds 300 ms,
before the 1.20 s scope scan and roughly one second of XIRR work, and net worth
has two full-journal passes left. What remains is that the app recomputes
immutable history on every request and every journal edit.

Every expensive computation is a left fold over date-sorted entries snapshotted
at period ends, and historical snapshots are immutable under append of both
transactions and price directives. The planned caching design checkpoints those
folds and keys them on a per-period prefix hash chain, so a journal append
recomputes one month instead of hundreds. The full design lives in issue
[#17](https://github.com/nixypanda/hedger/issues/17).

## Operational observability

The caching work wants metrics or structured timings for:

- snapshot reload duration and result;
- derived rebuild duration, revision, and abandoned rebuilds;
- queued, running, cancelled, and failed background jobs;
- cache hits, misses, evictions, and single-flight waiters by report family;
- calculation duration by report stage;
- response revision and whether it was served stale or refreshing;
- worker-pool saturation.

Keep to report family, normalized non-sensitive dimensions, request ID,
revision, and timing.

## Standing rules

1. Measure the component before optimizing it.
2. Force every field of a benchmarked record; a component slower than its parent
   is a measurement bug.
3. Measure the allocation rate before attempting parallelism; these reports are
   allocation-bound.
4. Sharing a large intermediate across passes is not automatically cheaper than
   recomputing it.
5. A serial microbenchmark is not concurrency evidence.
