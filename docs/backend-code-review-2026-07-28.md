# Hedger backend — remaining review work

_Original review: 2026-07-28 · Reconciled with source: 2026-08-23 · Scope: `backend/`_

Only unresolved, actionable findings remain. Items are ordered by priority.

## P1 — correctness and robustness

- [x] **Handle missing prices in net-worth history**

  `Domain/NetWorth.hs:154-162` converts missing asset or liability values to zero. Historical
  `positive`, `negative`, and `net` values can therefore look complete while being understated.
  Incomplete points remain visible with the known display-commodity values. Each timeline point now
  carries a structured completeness status and the commodities whose conversions are missing.

- [ ] **Force application results inside the `AppError` boundary**

  `Application/AppM.hs:32-42` catches exceptions around `evaluate`, which forces only weak head
  normal form. A lazy exception can occur later during JSON serialization and escape the normal
  error envelope. Choose an explicit forcing boundary, preserving asynchronous exceptions; hledger
  types generally lack `NFData`. Add a delayed-exception regression test.

- [ ] **Make investment performance roles configurable**

  `Config.hs:154-174` maps every YAML gain account to `OtherIncome`, while
  `Domain/Internal/InvestmentPerformance.hs:340-363` supports interest, dividend, realised gain,
  fee, and other income. Expose and parse a role for each configured performance account, or remove
  the role-specific domain/API output. Prefer exposing roles if the decomposition remains useful.

## P2 — state and reporting semantics

- [ ] **Decide the investment gain/cost identity**

  In `Domain/Internal/InvestmentPerformance.hs:154-167,199-205`, `purchaseCost` subtracts retained
  performance but `gainLoss` uses the unadjusted reported cost. Thus `gainLoss` is not generally
  `marketValue - purchaseCost`. Either document/name this semantic explicitly or reconcile the
  calculation, then pin the decision with focused tests.

## P3 — boundaries and operations

- [ ] **Protect the OpenAPI contract from drift**

  `contracts/openapi.json` is maintained independently from the Servant API and DTOs. Generate it
  with `servant-openapi3`, or add CI contract tests for representative endpoint and error encodings,
  not only the already-pinned error-envelope key.

## P4 — maintainability

- [ ] **Parse configuration into valid types**

  `Config.hs:20-35,121-148,261-280` stores validated values as raw `Text` and `Int`; `validate`
  returns the same type, so invalid states remain representable. Introduce types such as `Port`,
  `FiscalMonth`, and non-empty/token text. Also implement the documented environment fallback or
  remove that promise from the project instructions.
