# Test granularity and `Internal` module exposure

**Question.** If tests are the right granularity, they should only need the
public API of the main modules — not `Hedger.**.Internal.*`. Where we reach into
`Internal`, are we testing too granularly?

This note inventories the actual exposure, separates the legitimate cases from
the smell, and proposes concrete fixes.

## Verdict

The heuristic is directionally right but too strong as stated. Not every
`Internal` import means over-granular testing. The useful distinction is:

- If a test imports `Internal` only because a symbol is **re-exported
  elsewhere**, that is unnecessary coupling and should be fixed.
- If a test imports `Internal` to assert an **implementation-equivalence law**
  between an optimized path and a simple reference, the import is justified —
  but these are the tests most likely to be over-granular and should be few,
  property-shaped, and named as invariants.
- If a test imports `Internal` for a **security or wiring seam** (cursor codec,
  WAI application), `Internal` is the natural seam. Keep it, or promote the seam
  to the public module if the seam is genuinely part of the app's contract.

A better diagnostic than "does the test import `Internal`?" is:

> Does the test assert something the public API does not promise, and is it not
> an implementation-equivalence law with a reference implementation?

`AGENTS.md` already codifies the exception: *"Shared plumbing and testable guts
live under `Domain.Internal.*` ... exposed for property tests."* So `Internal`
exposure is by design; the question is whether each use is a "testable gut" or
an accident.

## Inventory

Eight test modules import a `Hedger.**.Internal.*` module:

| Test module | Internal module(s) imported |
| --- | --- |
| `Domain/InvestmentPerformanceSpec.hs` | `Domain.Internal.InvestmentPerformance` |
| `Domain/NetWorthSpec.hs` | `Domain.Internal.NetWorth` |
| `Domain/PeriodReportSpec.hs` | `Domain.Internal.Cashflow`, `Domain.Internal.NetWorth`, `Domain.Internal.PeriodReport` |
| `Domain/TransactionSpec.hs` | `Domain.Internal.Account`, `Domain.Internal.Transaction` |
| `TestHelpers.hs` | `Domain.Internal.PeriodReport` |
| `Application/TransactionQuerySpec.hs` | `Domain.Internal.Transaction` |
| `Infrastructure/Internal/CursorSpec.hs` | `Application.Internal.Cursor` |
| `Interface/HTTP/APISpec.hs` | `Runtime.Internal.Server` |

`Domain/XirrSpec.hs` is the counter-example worth copying: `Hedger.Domain.Xirr`
is a thin public re-export of `Domain.Internal.Xirr`, and the spec imports only
the public module.

### Symbols used by tests but not re-exported publicly

| Internal module | Public module | Symbols tests use that are Internal-only |
| --- | --- | --- |
| `Domain.Internal.Transaction` | `Domain.Transaction` | `dominantMovement` (everything else is public) |
| `Domain.Internal.Account` | `Domain.Account` | `isAssetOnly`, `isLiabilities`, `isRevenue` (`isCashOnly` is public) |
| `Domain.Internal.InvestmentPerformance` | `Domain.InvestmentPerformance` | `holdingsAtWithJournal`, `holdingsSeriesAtWithJournal`, `ScopeScan(..)`, `scopeScansAt`, `attributedAtWith`, `retainedPerformanceAtWith`, `externalCashFlowsAtWith`, `hasAttributablePerformance` |
| `Domain.Internal.PeriodReport` | (none; re-exported piecemeal) | `ReportSide`, `wholeReport`, `reportSide`, `reportTotals`, `reportMissing`, `reportPeriodsFrom`, `breakdownItemsFor` (`BreakdownItem` is public via `Domain.NetWorth`/`Domain.Cashflow`) |
| `Domain.Internal.NetWorth` | `Domain.NetWorth` | `assetLiabilityPeriodicReport`, `assetCashPeriodicReport`, `liabilityPeriodicReport`, `gainPeriodicReport`, `currentAssetCashBalance`, `currentLiabilityBalance` |
| `Domain.Internal.Cashflow` | `Domain.Cashflow` | `incomeExpensePeriodicReport`, `incomeGainPeriodicReport`, `expensePeriodicReport` |
| `Application.Internal.Cursor` | (none) | `encodeTransactionCursor`, `decodeTransactionCursor`, `encodeRegisterCursor`, `decodeRegisterCursor` |
| `Runtime.Internal.Server` | `Runtime.Server` | `makeApplication` |

## Classification of each test

### 1. Unnecessary coupling — fix by importing the public symbol

- **`TestHelpers.hs`**: imports `BreakdownItem` from
  `Domain.Internal.PeriodReport`. The same type is re-exported by
  `Domain.NetWorth` and `Domain.Cashflow`. Import it from a public module.
- **`TransactionQuerySpec.hs`**: imports `newestFirst` from
  `Domain.Internal.Transaction`. It is public in `Domain.Transaction`.
- **`TransactionSpec.hs`**: `TransactionClassification(..)`,
  `classificationFor`, `isOpening`, `newestFirst` are all public. Only
  `dominantMovement` is Internal-only. Two options: import the public symbols
  from `Domain.Transaction`, or promote `dominantMovement` to the public module
  (it is a pure domain query already used by `classificationFor`).

These three are unambiguous and cheap to fix; together they remove two of the
eight Internal imports entirely.

### 2. Implementation-equivalence laws — justified, but keep them few

- **`InvestmentPerformanceSpec.hs`**: asserts that the optimized bulk paths
  (`holdingsSeriesAtWithJournal`, `summariesAtPrepared`, `scopeScansAt`,
  `summariesAtScopesPrepared`) agree with the simple per-day functions, and that
  attribution/retained components compose. This is exactly the "testable guts
  exposed for property tests" case. The generated bulk-equivalence properties
  now carry this load; the fixture law matrices were trimmed accordingly.
  Verdict: keep, but the legitimate surface is the eight building blocks above;
  nothing else should creep in.
- **`PeriodReportSpec.hs`**: asserts a consolidated report side equals the
  separately filtered report (`reportSide`, `reportTotals`, `reportMissing`,
  `breakdownItemsFor`). This is a report-kernel law not expressible through
  `computeNetWorth`/`computeCashflow` because those also apply period selection
  and breakdown shaping. Verdict: keep.
- **`NetWorthSpec.hs` reference matrix**: compares `computeNetWorth`'s current
  KPIs against `currentAssetCashBalance`/`currentLiabilityBalance`. That is an
  internal-vs-internal oracle — one implementation checked against another
  implementation, neither of which is the public contract. The new property
  `current net worth is assets minus liabilities` and the existing fixture tests
  pin the public behaviour. Verdict: this is the clearest "over-granular" case;
  consider deleting it or rebasing it on public KPI values.

### 3. Security / wiring seams — keep, or promote

- **`CursorSpec.hs`**: the cursor codec is a security boundary
  (tamper-resistance, revision/query binding). Its round-trip and tamper tests
  are genuinely about the codec, not about a screen. Verdict: keep the intent,
  but note two smells: (a) it could be rewritten through the public
  `transactionPage`/`registerPage` cursor flow (as `bindsCursorsCorrectly`
  already is), and (b) the test path
  `test/Hedger/Infrastructure/Internal/CursorSpec.hs` does not mirror the source
  module `Hedger.Application.Internal.Cursor`, violating AGENTS' "tests mimic
  src layout". At minimum, move it to
  `test/Hedger/Application/Internal/CursorSpec.hs`.
- **`APISpec.hs`**: needs a WAI `Application`; `makeApplication` is the wiring
  function. The public `Runtime.Server` exports only `runServer`, which blocks a
  test harness. Verdict: keep, or export `makeApplication` from
  `Runtime.Server` so HTTP tests depend on a public seam.

## Refined rule of thumb

1. **Public contract tests** must import only public modules. Any Internal
   import that exists solely because a symbol is re-exported elsewhere is a bug
   in the test's imports — fix immediately.
2. **Implementation-equivalence laws** may import Internal, must be
   property-shaped over generated inputs, and must declare the reference
   implementation they compare against. If a law can be phrased against the
   public API, prefer that.
3. **Never use one Internal implementation as the oracle for another.** That is
   the strongest signal a test is too granular.
4. A public module that hides a concept tests need (e.g. `dominantMovement`) is
   a prompt to decide whether the concept is public: if it is part of the
   ubiquitous language, promote it; if not, tests should not assert it directly.

## Concrete follow-ups

| # | Change | Effect |
| --- | --- | --- |
| 1 | Import `BreakdownItem` from `Domain.NetWorth`/`Domain.Cashflow` in `TestHelpers` | removes 1 Internal import |
| 2 | Import `newestFirst` from `Domain.Transaction` in `TransactionQuerySpec` | removes 1 Internal import |
| 3 | Import public classification symbols in `TransactionSpec`; keep only `dominantMovement` internal (or promote it) | narrows 1 Internal import |
| 4 | Rename/move `Infrastructure/Internal/CursorSpec.hs` to `Application/Internal/CursorSpec.hs` | fixes layout violation |
| 5 | Decide fate of the `NetWorthSpec` strict-reference matrix (internal oracle) | removes the main over-granular case |
| 6 | Decide whether `makeApplication` is a public seam | removes `Runtime.Internal` from tests |

After 1–3, four of the eight Internal imports disappear for free. 4–6 are
judgement calls about what the public contract should be.
