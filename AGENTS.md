# Agent instructions

These instructions apply to the entire repository. More specific `AGENTS.md`
files may add to or override them within their directory.

## Build and test workflow

- Enter `nix develop` once and keep that shell or command session alive while
  working. Do not wrap every build, test, formatter, or check command in a new
  `nix develop -c` invocation; repeated flake evaluation adds substantial
  latency.
- During iteration, run the narrowest relevant command inside that development
  shell so Cabal can reuse `backend/dist-newstyle`. If your changes are
  restricted to a single project (e.g. only `backend/` or only `frontend-web/`),
  use that project's specific commands instead of the full `just check`:
  - Backend only: `just check-backend` (or narrower subsets like
    `check-backend-tests`, `check-backend-format`).
  - Frontend only: `just check-frontend` (or narrower subsets like
    `check-frontend-format`, `check-frontend-line-length`).
  Run the full repository-mandated `just check` before handing work back, or
  when changes span multiple projects.
- Do not use `nix flake check` as the inner development loop. Its checks run in
  clean Nix derivations, and every source edit changes their input hash, so the
  previous local build does not make the next edited revision incremental. The
  workspace check also compiles the backend independently of the package
  derivation. Reserve `nix flake check` for final Nix/CI verification when the
  task changes Nix configuration or otherwise calls for it.

## Haskell

### Tooling and language

- Run `just format` after changing Haskell and run `just check` before handing
  work back. Fourmolu is the repository Haskell formatter; do not hand-format
  around it.
- Use `GHC2024` as the Cabal `default-language` for every Haskell component.
- Enable shared language extensions in the Cabal common stanza rather than with
  per-module pragmas. The baseline is:
  - `DataKinds` and `TypeOperators`, which Servant APIs use heavily.
  - `ImportQualifiedPost`.
  - `MultiWayIf`.
  - `OverloadedRecordDot`.
  - `OverloadedStrings`.
  - `RecordWildCards`.
  - `DeriveAnyClass` for Aeson derivation.
- Add another extension to the shared Cabal list only when it is used broadly.
  Keep a genuinely local extension in the module that needs it.

### Imports

- Write qualified imports in postpositive form: `import Data.Text qualified as
  Text`.
- Prefer qualified imports for ordinary functions and names, using a short,
  conventional alias.
- Import operators unqualified and explicitly when that makes expressions more
  readable, for example `import Control.Applicative ((<|>))`. Do not force
  operators behind qualified names.
- Keep import lists explicit where practical. An open import is acceptable for
  a DSL such as Servant when an explicit list would obscure the API definition.

### Text, bytes, and literals

- Use strict `Text` for application strings, configuration, domain data, and
  user-facing messages.
- Use strict `ByteString` only at binary, encoding, HTTP, or library boundaries.
  Convert to or from `Text` at the boundary; do not let byte strings leak into
  the domain model. Use lazy text or bytes only when required for streaming or
  by a library API.
- Rely on `OverloadedStrings` so literals acquire the efficient string type
  required by their context. Avoid `String` and unnecessary `pack`/`unpack`
  cycles.

### Configuration and JSON

- Define command-line configuration with `optparse-applicative`. Keep parsing,
  environment fallback, defaults, and validation testable without executing the
  process parser.
- Derive Aeson encoders and decoders with `deriving anyclass (ToJSON, FromJSON)`
  (via `Generic`) rather than handwritten instances or Template Haskell. Wire
  names follow the Haskell record field names, so when a wire name must differ
  from an idiomatic Haskell name, name the field to match the wire (for example
  the error envelope's `error` field). Cover shapes with golden or round-trip
  tests.
- Return complete transaction DTOs, including postings, in transaction list
  responses. Do not introduce transaction summary/detail endpoint pairs or
  Hedger-specific transaction identifiers; the frontend uses the selected
  object from the list it already loaded.

### Tests

- Use Hedgehog for property tests wherever behavior has laws, invariants,
  round trips, parsers, or meaningful generated input. Prefer properties over a
  long list of hand-picked examples; keep focused example and regression tests
  where a property would be less clear.
- Get user go-ahead for every property test before implementing it.
- No oracle-based property tests and no reimplementing the whole logic in the test.
- Add generators and shrinking appropriate to the domain. A property that only
  exercises a few constants is not a useful property test.
- Tests with more than 3 lines should be turned into separate functions.
- Follow Haskell internal module strategy when there are sufficient things to test.
- Tests should mimic src layout.
- Keep API endpoint tests selective. Test endpoints when they add value at the
  HTTP boundary: routing, status and headers, JSON shape, authentication or
  validation, and stable error envelopes. Do not duplicate all domain behavior
  through WAI tests.

### Design principles

- Avoid Text/String types.
- Parse, don't validate.
- Parse on boundaries or functional core, imperative shell.
- Make invalid states impossible to represent.
- Total functions.
- Newtypes for semantic distinction.

### Module layout and layering

The backend is a functional-core / imperative-shell design. Dependencies point
inward: `Interface` and `Runtime` → `Application` → `Domain`. `Infrastructure`
is the only place with real I/O and mutable state.

- `Hedger.Domain.*` — the functional core. hledger *is* the domain vocabulary;
  hledger types are used directly, not wrapped behind an anti-corruption layer.
  Every ledger calculation lives here as pure, total functions over a `Snapshot`,
  reachable by property tests without `AppM`. Concept modules: `Account`,
  `Transaction`, `Investment`, `InvestmentPerformance`, `NetWorth` (balance-sheet
  stock and its return decomposition), `Cashflow` (income/expense flows),
  `Activity` (the quantified counterpart of `Transaction` classification), plus
  supporting `Valuation` (display-commodity projection: `Conversion`,
  `ConversionWarning`), `Period` (granularity and period math), `Snapshot`, and
  `Error`. Shared plumbing and testable guts live under `Domain.Internal.*`
  (e.g. `Internal.PeriodReport`, `Internal.InvestmentPerformance`), re-exported by
  a thin public module — the `Domain.Account` / `Domain.Internal.Account` pattern.
- `Hedger.Application.*` — the imperative shell and query side. Thin use cases in
  `AppM` (`ReaderT Env (ExceptT AppError IO)`): resolve request inputs, read the
  snapshot and config, call the domain, attach `SnapshotMetadata`, and assemble
  UI-shaped read-models. It contains no ledger calculation. (`Application.Analytics`
  is an application-layer facade that bundles the net-worth/cashflow/activity
  report endpoints; it is not a domain concept.)
- `Hedger.Interface.HTTP.*` — Servant API, wire DTOs, and domain↔DTO mappers.
  User-facing prose (e.g. warning sentences) is rendered here from structured
  domain conditions; it is never baked into the domain.
- `Hedger.Infrastructure.*` — journal reading and the mutable `SnapshotStore`.
- `Hedger.Runtime.*` and `Hedger.Config` — composition root and configuration.

Rules that keep this boundary honest:

- **Pure is not the same as domain.** A concept belongs in `Domain` only if it
  exists in the ubiquitous language of ledger/portfolio accounting independent of
  the UI. Pure-but-UI-shaped projections — KPI sparkline trends, top-N/"Other"
  category grouping, breadcrumb and drill-down view-models, the
  metadata-carrying response bundles — are the application's read-side
  responsibility, not the domain's.
- **Domain values carry no `SnapshotMetadata`.** Freshness and reload state are
  application concerns; the application attaches them to its response records.
- **Name ledger concepts, not report screens.** Prefer nouns like `NetWorth`,
  `Cashflow`, and `Activity` over reporting/UI umbrella terms.
- **`NoFieldSelectors` caveat.** Domain records enable `NoFieldSelectors`, so
  cross-module `record.field` dot access requires importing the field labels
  (`Type(..)`), unlike hledger's own records where the instances are always in
  scope.

### Ledger benchmarks

- Every performance-sensitive ledger operation must have a benchmark in a
  Cabal benchmark suite using `tasty-bench`.
- Use Hedgehog generators to create valid varied ledger inputs for benchmark
  setup where generated data is useful. Generate and force inputs outside the
  timed action; benchmarks must measure the ledger operation, not generation or
  parsing unless parsing is explicitly the subject.
- Benchmark representative small inputs and the large journal fixtures supplied
  by the project. Keep large journals as external fixture files rather than
  embedding them in Haskell, and record enough context to reproduce a result.
- Add correctness properties for a ledger operation before or alongside its
  benchmarks. A benchmark is not a correctness test.
