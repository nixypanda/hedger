# Agent instructions

These instructions apply to the entire repository. More specific `AGENTS.md`
files may add to or override them within their directory.

## Build and test workflow

- Enter `nix develop` once and keep that shell or command session alive while
  working. Do not wrap every build, test, formatter, or check command in a new
  `nix develop -c` invocation; repeated flake evaluation adds substantial
  latency.
- During iteration, run the narrowest relevant command inside that development
  shell so Cabal can reuse `backend/dist-newstyle`. Run the repository-mandated
  `just check` before handing work back.
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
  - `TemplateHaskell` for Aeson derivation.
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
- Derive Aeson encoders and decoders with `Data.Aeson.TH` (`deriveJSON`) rather
  than handwritten instances. Put wire-name transformations in the derivation
  options and cover them with golden or round-trip tests.

### Tests

- Use Hedgehog for property tests wherever behavior has laws, invariants,
  round trips, parsers, or meaningful generated input. Prefer properties over a
  long list of hand-picked examples; keep focused example and regression tests
  where a property would be less clear.
- Add generators and shrinking appropriate to the domain. A property that only
  exercises a few constants is not a useful property test.
- Keep API endpoint tests selective. Test endpoints when they add value at the
  HTTP boundary: routing, status and headers, JSON shape, authentication or
  validation, and stable error envelopes. Do not duplicate all domain behavior
  through WAI tests.

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
