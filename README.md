# Hedger

Hedger is a backend-backed, read-only replica of eLedger. The repository has
completed Phase 2 of the implementation roadmap.

```sh
nix develop
just check
just serve
```

The API contract is in `contracts/openapi.json`; Release 1 decisions are in
`docs/api-decisions.md`; and the shared synthetic journal is in
`backend/test/fixtures/shared.journal`.

The server accepts equivalent `HEDGER_*` environment variables for all runtime
configuration. Run `hedger-server --help` for the command-line options. The
journal is always supplied as a runtime path and is never embedded in the Nix
package.

Release 1 uses the stock WAI access logger. Possible future structured logging
requirements are recorded in `docs/future-logging.md`.

The backend parses the configured journal into an immutable hledger snapshot
containing complete transactions and their postings. Reload failures retain
the last usable snapshot.
Send the server `SIGHUP` to parse, validate, and atomically publish a replacement.
