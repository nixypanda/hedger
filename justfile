set shell := ["bash", "-euo", "pipefail", "-c"]

fixture := "backend/test/fixtures/shared.journal"

default:
    @just --list

# Enter the reproducible development environment.
dev:
    nix develop

# Format all source files in place.
format:
    fourmolu --mode inplace backend/app backend/benchmark backend/src backend/test
    elm-format frontend-web/src frontend-web/tests --yes
    nix fmt

# Check both projects, the fixture, and shared API contracts.
check: check-backend check-frontend check-contracts

check-backend: check-cabal check-haskell-format check-backend-tests
    hledger -f {{fixture}} check

check-haskell-format:
    fourmolu --mode check backend/app backend/benchmark backend/src backend/test

[private]
[working-directory("backend")]
check-backend-tests:
    cabal test --offline all

[private]
[working-directory("backend")]
check-cabal:
    cabal check

check-frontend:
    elm-format frontend-web/src frontend-web/tests --validate
    python3 scripts/check_elm_project.py frontend-web/elm.json

check-contracts:
    python3 scripts/check_contracts.py contracts/openapi.json backend/test/golden

test: check

# Run the Phase 2 server against the synthetic fixture.
[working-directory("backend")]
serve:
    cabal run hedger-server -- --journal test/fixtures/shared.journal
