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

# Check both projects, the fixture, and shared API contracts.
check: check-backend check-frontend check-contracts

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

check-frontend: check-frontend-format check-frontend-line-length
    cd frontend-web && elm make src/Main.elm --output=/dev/null
    cd frontend-web && elm-test
    python3 scripts/check_elm_project.py frontend-web/elm.json
    python3 scripts/check_frontend_assets.py

check-frontend-format:
    elm-format frontend-web/src frontend-web/tests --validate

check-frontend-line-length:
    if rg '.{111,}' -g '*.elm' frontend-web/src -g '!**/Main.elm' frontend-web/tests -g '!**/TransactionsTest.elm'; then exit 1; else test "$?" -eq 1; fi

check-contracts:
    python3 scripts/check_contracts.py contracts/openapi.json

# Run the backend with the example configuration and journal.
[working-directory("backend")]
serve-backend:
    cabal run hedger-server -- --config ../hedger.example.yaml

[working-directory("frontend-web")]
serve-frontend:
    npm run dev
