set shell := ["bash", "-euo", "pipefail", "-c"]

fixture := "backend/test/fixtures/shared.journal"

default:
    @just --list

# Enter the reproducible development environment.
shell:
    nix develop

# Run the backend and frontend development servers.
dev:
    @echo "Starting backend and frontend..."
    (just dev-backend) &
    (just dev-frontend) &
    wait

# Build all projects.
build: build-backend build-frontend

# Run all tests.
test: test-backend test-frontend

# Run available coverage reports.
coverage: coverage-backend

# Format all source files in place.
format: format-backend format-frontend format-nix

# Check both projects and the shared journal fixture.
check: check-backend check-frontend

# Build the backend.
[working-directory("backend")]
build-backend:
    cabal build --offline all

# Run backend tests.
[working-directory("backend")]
test-backend:
    cabal test --offline all

# Format backend Haskell sources.
format-backend:
    fourmolu --mode inplace backend/app backend/benchmark backend/src backend/test

# Run all backend checks.
check-backend: check-backend-format check-backend-line-length check-backend-manifest test-backend check-journal

check-backend-format:
    fourmolu --mode check backend/app backend/benchmark backend/src backend/test

check-backend-line-length:
    if rg '.{111,}' -g '*.hs' backend/app backend/benchmark backend/src backend/test; then exit 1; else test "$?" -eq 1; fi

check-journal:
    hledger -f {{fixture}} check

[private]
[working-directory("backend")]
check-backend-manifest:
    cabal check

# Run backend tests with coverage and open the HTML report.
[working-directory("backend")]
coverage-backend:
    cabal test hedger-backend-test --enable-coverage --open

# Run backend tests with coverage and print a text summary.
[working-directory("backend")]
coverage-backend-report:
    cabal test hedger-backend-test --enable-coverage

# Run the backend with the example configuration and journal.
[working-directory("backend")]
dev-backend:
    cabal run hedger-server -- --config ../hedger.example.yaml

# Run an optimized backend with a generated performance journal (5,000 or 50,000 transactions).
[working-directory("backend")]
dev-backend-perf size="5000":
    #!/usr/bin/env bash
    set -euo pipefail
    case "{{size}}" in
        5000) journal="../examples/performance/india-us-portfolio-medium-5000.journal" ;;
        50000) journal="../examples/performance/india-us-portfolio-large-50000.journal" ;;
        *) echo "size must be 5000 or 50000" >&2; exit 2 ;;
    esac
    cabal run --enable-optimization=2 hedger-server -- --config ../hedger.example.yaml --journal "$journal"

# Run the backend using paths supplied through the local environment.
[working-directory("backend")]
dev-backend-real:
    cabal run hedger-server -- --config "${HEDGER_CONFIG:?set HEDGER_CONFIG}" --journal "${HEDGER_JOURNAL:?set HEDGER_JOURNAL}"

# Build the frontend.
[working-directory("frontend-web")]
build-frontend:
    npm run build

# Run frontend tests.
[working-directory("frontend-web")]
test-frontend:
    npm test

# Format frontend Elm sources.
format-frontend:
    elm-format frontend-web/src frontend-web/tests --yes

# Run all frontend checks.
check-frontend: check-frontend-format check-frontend-line-length build-frontend test-frontend

check-frontend-format:
    elm-format frontend-web/src frontend-web/tests --validate

check-frontend-line-length:
    if rg '.{111,}' -g '*.elm' frontend-web/src -g '!**/Main.elm' frontend-web/tests -g '!**/TransactionsTest.elm'; then exit 1; else test "$?" -eq 1; fi

# Run the frontend development server.
[working-directory("frontend-web")]
dev-frontend:
    npm run dev

# Format Nix sources.
format-nix:
    nix fmt
