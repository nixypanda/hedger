# Local development

## Prerequisites

- [Nix](https://nixos.org/download) with flakes enabled.
- Optionally, [direnv](https://direnv.net/) — the repository has an `.envrc`
  that loads the dev shell automatically.

## Enter the development shell

```console
nix develop
```

Keep this shell open while working and run every command inside it. The shell
provides the Haskell toolchain (GHC, Cabal, fourmolu, HLS), the Elm toolchain
(`elm`, `elm-format`, `elm-test`), Node.js, `just`, `hledger`, and `python3`.

With direnv, run `direnv allow` once instead and the shell loads on entry.

## Run the example

Start the backend and frontend together:

```console
just dev
```

Then open <http://localhost:1234>.

- The frontend is a Vite dev server on port **1234** with a strict port. It
  proxies `/api` to the backend.
- The backend listens on `127.0.0.1:8080`.
- `just dev` uses [`hedger.example.yaml`](../hedger.example.yaml) and the
  synthetic journal at
  [`examples/india-us-portfolio-2023-2025.journal`](../examples/india-us-portfolio-2023-2025.journal).
  All data shown is synthetic.

To run the two halves in separate terminals instead:

```console
just dev-backend
just dev-frontend   # in another terminal
```

### Point the backend at a real journal

Supply your own configuration and journal through the environment:

```console
export HEDGER_CONFIG=/path/to/hedger.yaml
export HEDGER_JOURNAL=/path/to/main.journal
just dev-backend-real
```

CLI flags (`--config`, `--journal`, `--port`, and so on) override YAML values.
See [backend.md](backend.md) for every configuration option.

### Run a performance-sized journal

The repository ships generated 5,000- and 50,000-transaction journals for
performance work:

```console
just dev-backend-perf 5000
just dev-backend-perf 50000
```

These build an optimized backend (`-O2`) against the chosen fixture.

## Backend tasks

Run from the repository root (or use the underlying commands in `backend/`):

```console
just build-backend          # cabal build --offline all
just test-backend           # cabal test --offline all
just coverage-backend       # coverage report, opens HTML
just format-backend         # fourmolu in place
just check-backend          # format, line length, cabal check, tests, journal check
```

Or directly:

```console
cd backend && cabal build --offline all
cd backend && cabal test --offline all
cd backend && cabal bench all
cd backend && cabal run hedger-server -- --config ../hedger.example.yaml
```

Cabal reuses `backend/dist-newstyle`, so narrow commands stay incremental. See
[performance.md](performance.md) for the benchmark suite.

## Frontend tasks

```console
just build-frontend          # vite production build
just test-frontend           # elm-test
just format-frontend         # elm-format in place
just check-frontend          # UI boundary, format, line length, build, tests
```

Or directly:

```console
cd frontend-web && npm run dev      # Vite dev server
cd frontend-web && npm run build    # production build
cd frontend-web && npm test         # elm-test
cd frontend-web && npm run review   # elm-review
cd frontend-web && npm run format   # elm-format
```

## Checks and formatting

```console
just format        # format all Haskell, Elm, and Nix sources
just check         # backend checks + frontend checks
```

`just check` is the repository-mandated gate before handing work back. During
iteration, prefer the narrower `just check-backend`, `just check-frontend`, or
an even more specific recipe.

`nix flake check` runs the same checks in clean Nix derivations. Because every
source edit changes their input hash, it is not incremental; reserve it for
final Nix/CI verification, especially when Nix configuration changed.

Backend line length is 100 characters (`just check-backend-line-length`); frontend
Elm source is also checked at 100 characters, with `Main.elm` and
`TransactionsTest.elm` excluded.
