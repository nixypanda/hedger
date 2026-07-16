set shell := ["bash", "-euo", "pipefail", "-c"]

fixture := "backend/test/fixtures/shared.journal"

default:
    @just --list

# Enter the reproducible development environment.
dev:
    nix develop

# Format all source files in place.
format:
    elm-format frontend-web/src frontend-web/tests --yes
    nix fmt

# Check both projects, the fixture, and shared API contracts.
check: check-backend check-frontend check-contracts

check-backend: check-cabal
    ghc -Wall -Werror -fno-code -ibackend/src backend/src/Hedger/Phase0.hs
    hledger -f {{fixture}} check

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
