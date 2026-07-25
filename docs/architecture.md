# Architecture

## System Overview

Hedger is a monorepo with a Haskell backend, Elm web frontend, and Nix-based
build/deploy. The backend is a read-only replica of an hledger journal.

```text
Elm frontend-web (PWA)
  -> HTTP JSON under /api/v1
Haskell backend (Servant + Warp)
  -> hledger-lib (journal parsing, queries, valuation, cost basis)
  -> in-memory immutable snapshot (IORef)

Nix flake: dev shell, packages, NixOS module
nginx reverse proxy (production)
```

There is no database. The journal file is the sole source of truth. The backend
parses it into an immutable in-memory snapshot at startup and re-reads it on
SIGHUP.

## Product Boundaries

Hedger is a read-only personal finance viewer:

- Display net worth, cashflow, investment performance, accounts, and
  transactions from an hledger journal.
- One deployment per ledger. No multi-tenancy or multi-ledger selection.
- Responsive phone and desktop PWA with offline app-shell caching.
- Nix-based development, builds, and NixOS deployment.

Out of current product scope:

- Ledger editing, transaction creation, or imports.
- Multi-ledger selection or multi-tenancy.
- Global search or generic report builders.
- Recurring/forecast screens.
- Authentication (deployment concern at the reverse proxy).
- Server-side preferences or sync.

## DDD Shape

The backend follows a functional-core / imperative-shell design. Dependencies
point inward.

```text
Interface.HTTP -> Application -> Domain
Runtime -> Application
Infrastructure -> Domain (snapshot boundary only)
```

The Domain layer owns ledger calculations as pure, total functions over a
`Snapshot`. It depends on `hledger-lib` types directly — hledger *is* the
domain vocabulary. The Application layer composes use cases in `AppM`. The
Interface layer translates HTTP requests into application calls and maps domain
DTOs to JSON. Infrastructure handles journal reading and mutable snapshot
storage. Runtime is the composition root.

## Layers

### Domain (`Hedger.Domain.*`)

The functional core. Every ledger calculation lives here as pure functions
over a `Snapshot`, reachable by property tests without `AppM`.

Key modules:

- `Account` — account tree, direct/subtree balances, valuation projection.
- `Transaction` — transaction classification and presentation.
- `Investment` / `InvestmentPerformance` — cost basis, return calculation,
  gain attribution.
- `NetWorth` — balance-sheet stock and its return decomposition.
- `Cashflow` — income/expense period reports.
- `Activity` — quantified counterpart of transaction classification.
- `Valuation` — display-commodity projection, conversion warnings.
- `Period` — granularity and period math.
- `Snapshot` — the immutable snapshot record.
- `Internal.*` — implementation details re-exported by thin public modules.

### Application (`Hedger.Application.*`)

The imperative shell and query side. Thin use cases in `AppM`:

```haskell
type AppM = ReaderT Env (ExceptT AppError IO)
```

Key modules:

- `AppM` — the monad stack, `Env`, error types.
- `Analytics` — application facade bundling net-worth, cashflow, and activity.
- `Investments` — investment query orchestration.
- `Transactions` / `TransactionQuery` — transaction listing and pagination.
- `Accounts` / `AccountRegisterQuery` — account tree and register.
- `System` — health, bootstrap, status.
- `Internal.Cursor` — cursor encoding/decoding.

### Interface (`Hedger.Interface.HTTP.*`)

Servant API definition, wire DTOs, and domain-to-DTO mappers. User-facing
prose (e.g. warning sentences) is rendered here from structured domain
conditions.

Key modules:

- `API` — combined Servant type.
- `Accounts`, `Transactions`, `Analytics`, `Investments`, `System` —
  per-feature endpoint handlers.
- `Common` — shared DTOs, `ResponseMetadata`, pagination helpers.
- `Errors` — error envelope construction.
- `Middleware` — request context, query redaction for logging.

### Infrastructure (`Hedger.Infrastructure.*`)

Real I/O. Journal reading via hledger-lib and the mutable `SnapshotStore`.

- `SnapshotStore` — loads the journal, builds a `Snapshot`, publishes
  atomically via `IORef`, retains the last successful snapshot on failure,
  exposes `reloadSnapshot` for SIGHUP-triggered reload.

### Runtime (`Hedger.Runtime.*`)

Composition root and server startup.

- `Server` — Warp server setup, signal handlers, middleware wiring.

### Config (`Hedger.Config`)

CLI and YAML configuration parsed once at startup. Fields: `journalPath`,
`listenAddress`, `listenPort`, `defaultDisplayCommodity`,
`fiscalYearStartMonth`, `investmentMappings`.

## Snapshot System

The snapshot is the central architectural concept:

1. On startup, `SnapshotStore.loadSnapshot` reads the journal and all includes
   via hledger-lib, builds an immutable `Snapshot` containing the journal,
   transactions, account tree, latest journal date, and revision hash.
2. The snapshot is published atomically through an `IORef`. Concurrent readers
   always see a consistent, complete snapshot.
3. On SIGHUP, `reloadSnapshot` re-reads the journal. If parsing succeeds, the
   new snapshot replaces the old one. If it fails, the last successful snapshot
   is retained and the error is exposed.
4. The `Snapshot` record carries a `JournalRevision` (MD5 of effective source
   content) so responses can report freshness and clients can detect changes.

There is no report cache, no ETags, and no file watcher in Release 1. Each
request computes from the snapshot directly.

## Key Design Decisions

1. **hledger is the domain vocabulary.** Hedger wraps hledger types directly
   in the domain layer. There is no anti-corruption layer.
2. **Read-only backend.** The backend never writes to the journal.
3. **No effect system.** Plain `ReaderT Env (ExceptT AppError IO)`.
4. **No database.** In-memory snapshots from the journal file.
5. **One deployment per ledger.** No multi-tenancy.
6. **Functional core, imperative shell.** Pure domain calculations, effects at
   the edges.
7. **API DTOs are Hedger-owned.** Raw hledger types never reach JSON.
8. **Quantities are decimal strings.** Never JSON floating point.
9. **Missing prices are unknown, not zero.**

## API Surface

All routes are under `/api/v1`. Major groups:

- `/health/*` — liveness and readiness probes
- `/bootstrap` — startup metadata and configuration
- `/accounts/*` — account tree and register
- `/transactions` — hledger query and paginated transaction list
- `/net-worth/query` — net worth analytics
- `/cashflow/query` — cashflow analytics
- `/activity/query` — income/expense/asset/liability activity calendars
- `/investments/query` — investment performance

See [backend.md](backend.md) for route details.

## Deployment

Production deployment uses a NixOS module (`deploy/module.nix`) that:

- Runs the backend as a dedicated unprivileged user per instance.
- Serves the Elm frontend via nginx with SPA fallback.
- Reverse proxies `/api/` to the backend.
- Applies immutable caching to hashed static assets.
- Supports ACME TLS, unique ports/hostnames, and `systemctl reload` for
  SIGHUP-triggered journal reload.
