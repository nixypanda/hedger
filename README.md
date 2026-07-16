# Hedger

Hedger is a backend-backed, read-only replica of eLedger. The repository is in
Phase 0 of the implementation roadmap.

```sh
nix develop
just check
```

The API contract is in `contracts/openapi.json`; Release 1 decisions are in
`docs/api-decisions.md`; and the shared synthetic journal is in
`backend/test/fixtures/shared.journal`.
