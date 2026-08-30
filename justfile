set shell := ["bash", "-euo", "pipefail", "-c"]

fixture := "backend/test/fixtures/shared.journal"

default:
    @just --list

# Enter the reproducible development environment.
dev:
    nix develop

# Format all source files in place.
format: format-backend format-frontend format-nix

format-backend:
    fourmolu --mode inplace backend/app backend/benchmark backend/src backend/test

format-frontend:
    elm-format frontend-web/src frontend-web/tests --yes

format-nix:
    nix fmt

# Check both projects and the shared journal fixture.
check: check-backend check-frontend

check-backend: check-backend-format check-backend-line-length check-backend-manifest check-backend-tests check-journal

check-backend-format:
    fourmolu --mode check backend/app backend/benchmark backend/src backend/test

check-backend-line-length:
    if rg '.{111,}' -g '*.hs' backend/app backend/benchmark backend/src backend/test; then exit 1; else test "$?" -eq 1; fi

check-journal:
    hledger -f {{fixture}} check

[private]
[working-directory("backend")]
check-backend-tests:
    cabal test --offline all

[private]
[working-directory("backend")]
check-backend-manifest:
    cabal check

# Run backend tests with coverage and open HTML report.
[working-directory("backend")]
coverage:
    cabal test hedger-backend-test --enable-coverage --open

# Run backend tests with coverage and print a text summary.
[working-directory("backend")]
coverage-report:
    cabal test hedger-backend-test --enable-coverage

check-frontend: check-frontend-format check-frontend-line-length
    cd frontend-web && elm make src/Main.elm --output=/dev/null
    cd frontend-web && elm-test

check-frontend-format:
    elm-format frontend-web/src frontend-web/tests --validate

check-frontend-line-length:
    if rg '.{111,}' -g '*.elm' frontend-web/src -g '!**/Main.elm' frontend-web/tests -g '!**/TransactionsTest.elm'; then exit 1; else test "$?" -eq 1; fi

# Run the backend with the example configuration and journal.
[working-directory("backend")]
serve-backend:
    cabal run hedger-server -- --config ../hedger.example.yaml

# Run an optimized backend with a generated performance journal (5,000 or 50,000 transactions).
[working-directory("backend")]
serve-backend-perf size="5000":
    #!/usr/bin/env bash
    set -euo pipefail
    case "{{size}}" in
        5000) journal="../examples/performance/india-us-portfolio-medium-5000.journal" ;;
        50000) journal="../examples/performance/india-us-portfolio-large-50000.journal" ;;
        *) echo "size must be 5000 or 50000" >&2; exit 2 ;;
    esac
    cabal run --enable-optimization=2 hedger-server -- --config ../hedger.example.yaml --journal "$journal"

[working-directory("backend")]
serve-backend-real:
    cabal run hedger-server -- --config '/Users/nixypanda/Library/Mobile Documents/com~apple~CloudDocs/Money/ledger/hedger/hedger-mine.yaml' --journal '/Users/nixypanda/Library/Mobile Documents/com~apple~CloudDocs/Money/ledger/journals/main-mine.journal'


[working-directory("frontend-web")]
serve-frontend:
    npm run dev
