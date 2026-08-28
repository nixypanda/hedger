# Pure hledger-compatible performance optimizations

_Status: orchestration audit complete; local candidates measured where applicable · Last updated: 2026-08-28_

## Scope

This document lists compute-only performance optimizations that preserve hledger
as the accounting and report engine. It deliberately excludes cross-request
caching, revision-scoped derived data, memoization, persistent storage, and a
Hedger-specific replacement for hledger's report calculations.

The original estimates below are directional hypotheses, retained only to guide
future upstream work. Actual measurements, where available, take precedence.
The earlier 50,000-transaction plan references were:

- Cashflow endpoint: approximately 196 ms.
- Net-worth endpoint: approximately 741 ms (current measurement: 711 ms / 716
  MB).
- Consolidated cashflow hledger report alone: approximately 139 ms.

The estimated gains overlap and must not be added together. Each optimization
needs an isolated before-and-after benchmark with allocation and GC measurements.
Effort estimates are focused engineering time and include implementation,
correctness tests, and benchmarks, but not upstream review or release time.

## Audit results

Items 1–7 require changes in upstream `hledger-lib`: Hedger consumes its report
implementation as an external dependency, so those candidates were not
attempted in this repository.

| Item | Status | Actual disposition |
|---:|---|---|
| 1–7 | Deferred upstream | Require an `hledger-lib` change; not attempted locally. |
| 8 | Rejected | 219 ms / 200 MB before and after; reverted because there was no measured benefit. |
| 9 | Completed | Isolated: 48.4 ms / 107 MB → 37.5 ms / 74 MB. Full cashflow: 219 ms / 200 MB → 193 ms / 167 MB. |
| 10 | Rejected | Existing list: 41.0 ms / 15 MB; unboxed vector: 45.2 ms / 15 MB. Reverted. |
| 11 | Completed | Production-shaped XIRR: 41.0 ms / 15 MB → 39.7 ms / 5 MB. Journal-derived XIRR: about 163 ms / 57 MB → 156 ms / 17 MB. |

## Options and original estimates

| Priority | Optimization | Status | Original estimated gain | Effort | Confidence |
|---:|---|---|---|---:|---|
| 1 | Compile and specialize hledger query predicates | Deferred upstream | 5–20% of report time; roughly 7–28 ms for cashflow | 3–7 days | Medium |
| 2 | Direct integer period indexing | Deferred upstream | 3–12% of periodic-report time; roughly 4–17 ms for cashflow | 2–4 days | Medium–high |
| 3 | Strict single-pass report aggregation | Deferred upstream | 10–30% time and 15–40% allocation; roughly 14–42 ms for cashflow | 5–10 days | Medium |
| 4 | Contiguous periodic-column storage | Deferred upstream | 5–20% time and 10–30% allocation; roughly 7–28 ms for cashflow | 4–8 days | Medium |
| 5 | Intern account names and hierarchy nodes | Deferred upstream | 10–30% on large journals; roughly 14–42 ms for cashflow and potentially 40–120 ms for net worth | 7–14 days | Medium–low |
| 6 | Replace repeated account-hierarchy operations with ID traversal | Deferred upstream | 5–15% where depth clipping or grouping is significant | 3–6 days after interning | Medium |
| 7 | Compact account-row × period representation | Deferred upstream | 10–25% time and 20–40% report allocation | 7–12 days | Medium–low |
| 8 | Fuse Hedger's post-report projection | Rejected | 1–5% of endpoint time; roughly 2–10 ms for cashflow | 1–3 days | High |
| 9 | Use integer month membership instead of `Set Text` | Completed | Under 1–2%; likely below 3 ms | 0.5–1 day | High |
| 10 | Use unboxed XIRR numerical terms | Rejected | 15–40% per solve; roughly 6–17 ms from the measured 42 ms solve | 2–4 days | Medium–high |
| 11 | Aggregate same-day XIRR flows without `Map Day` | Completed | 3–15% per solve, depending on duplicate dates | 1–2 days | Medium |

## 1. Compile and specialize hledger query predicates

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

Compile the general `Query` value into a streamlined predicate before scanning
transactions and postings:

- separate transaction-level predicates from posting-level predicates;
- recognize account-prefix queries and use prefix comparison rather than general
  regular-expression machinery;
- evaluate date and status predicates once per transaction;
- avoid repeatedly interpreting the `Query` algebraic structure; and
- combine account-type and account-prefix checks where practical.

This is per-report preparation and retains no result beyond the calculation.

**Estimated gain:** 5–20% of hledger report construction. The range is wide
because the benefit depends on how much current time is query interpretation
rather than valuation and amount aggregation.

**Effort:** 3–7 focused days.

## 2. Direct integer period indexing

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

Once hledger has established the report interval, map each transaction date
directly to a zero-based period index:

```text
transaction date → integer period index → accumulator column
```

Monthly reports can derive this index from the year and month; yearly reports
are simpler. The public report can continue to expose normal `Day` and date-span
values.

**Estimated gain:** 3–12% of periodic-report time. This removes period and
date-span traversal but does not reduce valuation work.

**Effort:** 2–4 days.

This is probably the safest hledger-internal experiment.

## 3. Strict single-pass report aggregation

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

Fuse the hot report pipeline:

```text
query match
→ account clipping and type classification
→ period selection
→ amount accumulation
```

Avoid constructing intermediate lists of matched or transformed postings. Use
strict folds or mutable arrays internally, and construct hledger's public
immutable report only at the boundary.

**Estimated gain:** 10–30% of report time and 15–40% of allocation. Reducing
allocation may also improve production latency under `-N`, where GC and memory
bandwidth are already known constraints.

**Effort:** 5–10 days. Correctness risk is moderate because the fused path must
preserve all existing report semantics.

## 4. Contiguous periodic-column storage

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

Replace list-shaped, per-period accumulators with strict indexed storage, such
as a `SmallMutableArray MixedAmount` or boxed mutable vector.

`MixedAmount` remains boxed and exact. Only the container becomes contiguous,
avoiding repeated list allocation, `zipWith`, and pointer chasing. Unboxed
storage is not appropriate for `MixedAmount`, which is intrinsically
variable-sized.

**Estimated gain:** 5–20% of report time and 10–30% of allocation.

**Effort:** 4–8 days.

This overlaps with strict single-pass aggregation and the compact row-matrix
option. These should be measured as stages of one prototype rather than treated
as additive changes.

## 5. Intern account names and hierarchy nodes

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

Assign a temporary integer ID to each distinct account during one report:

```text
Text account name ↔ AccountId
```

Use account IDs for aggregation, account-type lookup, hierarchy traversal, and
intermediate row keys. Convert IDs back to hledger account names only while
constructing the public report.

**Estimated gain:** 10–30% on large journals, especially when many postings
repeatedly reference a modest set of accounts. The benefit may be smaller when
valuation dominates.

**Effort:** 7–14 days. This is invasive because hledger's public types and
semantics must remain unchanged.

## 6. Integer account-hierarchy traversal

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

After account interning, represent parentage explicitly:

```text
AccountId → ParentAccountId
```

Depth clipping and parent traversal can then follow IDs instead of repeatedly
splitting and rebuilding `Text` account names. A clipped ancestor at a requested
depth can also be determined once per distinct account.

**Estimated gain:** 5–15% for reports where depth clipping or hierarchy grouping
is significant. The benefit will be small for entirely flat reports.

**Effort:** 3–6 additional days after account interning. This is not worthwhile
as a separate change without the interned representation.

## 7. Compact account-row × period representation

**Status: deferred upstream.** This requires changing `hledger-lib` and was not
attempted in Hedger. The following gain and effort remain hypotheses.

A periodic report naturally has two dimensions:

```text
account row × period column
```

A suitable sparse-row representation could be:

```text
IntMap AccountId (SmallMutableArray MixedAmount)
```

This keeps the selected account set sparse while making every active row's
periods contiguous. A flat matrix may be appropriate only if measurement shows
that the account universe is sufficiently small and dense.

**Estimated gain:** 10–25% of report time and 20–40% of report-related
allocation.

**Effort:** 7–12 days.

This is partly an alternative packaging of account interning, contiguous
columns, and strict aggregation. It is not an independent source of additive
gains.

## 8. Fuse Hedger's post-report projection

**Status: rejected and reverted.** The benchmark was 219 ms / 200 MB both
before and after, so the change delivered no measurable benefit. The original
estimate below is superseded by this result.

Hedger can traverse the completed hledger report once to derive:

- income totals;
- expense totals;
- missing commodities;
- the selected income breakdown; and
- the selected expense breakdown.

The current path splits report sides, calculates totals, scans missing
commodities, and constructs breakdowns in separate traversals.

**Estimated gain:** 1–5% of endpoint latency, or roughly 2–10 ms for cashflow.

**Effort:** 1–3 days.

This is localized and low risk, but report construction should remain the
higher priority unless profiling attributes unexpected cost to projection.

## 9. Integer month membership

**Status: completed.** Replacing `Set Text` membership with integer month
membership improved the isolated operation from 48.4 ms / 107 MB to 37.5 ms /
74 MB. Full cashflow improved from 219 ms / 200 MB to 193 ms / 167 MB. The
original estimate below is superseded by these results.

Replace formatted `Text` month keys and `Set Text` membership with integer month
indices or a bounded bitset when determining which report periods contain data.

**Original estimated gain:** under 1–2%, likely below 3 ms. The estimate was
too conservative for the measured workload; use the completed measurements
above instead.

**Effort:** 0.5–1 day.

This is best included in a related cleanup rather than pursued alone.

## 10. Unboxed XIRR numerical terms

**Status: rejected and reverted.** The existing list implementation measured
41.0 ms / 15 MB, while the unboxed-vector implementation measured 45.2 ms / 15
MB. The original estimate below is superseded by this result.

After hledger has produced and valued exact `Decimal` cashflows, the XIRR solver
already crosses an explicit numerical boundary into `Double`. Replace its
boxed list of pairs with parallel unboxed vectors:

```haskell
data NpvTerms = NpvTerms
    { fractions :: U.Vector Double
    , amounts :: U.Vector Double
    , lastFraction :: Double
    }
```

Evaluate NPV with a strict indexed loop. Every flow is revisited for each of the
1,025 initial search points and again while refining roots. This was the
original rationale for testing unboxed storage without changing accounting
semantics; the measurement above rejected it for this implementation.

**Estimated gain:** 15–40% of one XIRR solve, approximately 6–17 ms from the
measured 42 ms. An investment request that performs many solves could save
approximately 0.1–0.3 seconds.

**Effort:** 2–4 days.

## 11. Aggregate same-day XIRR flows directly

**Status: completed.** Production-shaped XIRR improved from 41.0 ms / 15 MB to
39.7 ms / 5 MB. Journal-derived XIRR improved from about 163 ms / 57 MB to 156
ms / 17 MB. The original estimate below is superseded by these results.

Investment cashflows are already chronological. Combine adjacent flows sharing
the same date while producing the solver input instead of rebuilding them with
`Map Day Decimal` during normalization.

**Estimated gain:** 3–15% of XIRR preparation, depending on how many flows share
a date. This is likely smaller than improving repeated NPV evaluation.

**Effort:** 1–2 days.

## Follow-up order

The local experiments are complete: retain items 9 and 11; leave items 8 and 10
reverted. Future pure-compute work belongs upstream in `hledger-lib`:

1. Profile periodic report construction by allocation site.
2. Add direct integer period indexing and measure it in isolation.
3. Compile and specialize query predicates.
4. Fuse report construction into one strict traversal and prototype contiguous
   periodic columns as stages of the same design.
5. Reprofile before considering account interning, hierarchy IDs, or a compact
   row matrix.

The prior 20–40% combined report-time target for the first four upstream
changes remains a hypothesis, not a forecast. It must not be added to the
completed local measurements.

## Representation constraints

`Decimal` is `DecimalRaw Integer`, and `Day` wraps an `Integer`. Neither is
naturally an unboxed fixed-width payload. Therefore these optimizations must not:

- convert ledger quantities to `Double`;
- assume every quantity fits in `Int64`;
- normalize monetary values to a fixed scale without an explicit range and
  overflow proof; or
- attempt to unbox `MixedAmount`.

Safe compact representations include checked integer period indices, temporary
account IDs, hierarchy IDs, and unboxed `Double` arrays only after XIRR's
existing numerical conversion boundary.
