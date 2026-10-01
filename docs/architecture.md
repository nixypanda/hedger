# Architecture

## System overview

Hedger is a monorepo with a Haskell backend, an Elm web frontend, and Nix-based
build and deployment. The backend is a read-only replica of an hledger journal.

```text
Elm frontend-web (PWA)
  -> HTTP JSON under /api/v1
  -> Server-Sent Events at /api/v1/events
Haskell backend (Servant + Warp)
  -> hledger-lib (journal parsing, queries, valuation, cost basis)
  -> in-memory immutable snapshot (STM TVar)
  <- fsnotify file watcher and SIGHUP

Nix flake: dev shell, packages, NixOS module
nginx reverse proxy (production)
```

There is no database. The journal file is the sole source of truth. The backend
parses it into an immutable in-memory snapshot at startup and reloads it
automatically when the underlying files change.

## Runtime data flow

1. **Startup.** `SnapshotStore` reads the root journal and every `include` via
   hledger-lib, builds an immutable `Snapshot`, and publishes it through an STM
   `TVar`.
2. **Reload.** `JournalWatcher` watches the files that made up the last snapshot
   and asks the store to reload after a debounced filesystem event. `SIGHUP`
   reloads the same store. A failed reload keeps the last successful snapshot
   and records the error.
3. **Requests.** Handlers read the current snapshot, run pure domain
   calculations, and respond. No request mutates shared state.
4. **Live updates.** `GET /api/v1/events` blocks on `awaitRevisionChange` and
   streams a `revision` event whenever a new snapshot is published.
5. **Client refresh.** The frontend opens an `EventSource` on that endpoint and
   refetches the visible screens when the revision changes.

## Product boundaries

Hedger is a read-only personal finance viewer:

- Display net worth, cashflow, investment performance, accounts, and
  transactions from an hledger journal.
- One deployment per ledger. No multi-tenancy or multi-ledger selection.
- A responsive phone and desktop PWA.
- Nix-based development, builds, and NixOS deployment.

Out of current product scope:

- Ledger editing, transaction creation, or imports.
- Multi-ledger selection or multi-tenancy.
- Global search or generic report builders.
- Recurring or forecasting screens.
- Authentication in the application (a deployment/reverse-proxy concern).
- Server-side preferences, accounts, or sync.

See [features.md](features.md) for the user-facing feature list and roadmap.

## Backend architecture

The backend follows a functional-core / imperative-shell design. Dependencies
point inward.

```text
Interface.HTTP -> Application -> Domain
Runtime -> Application
Infrastructure -> Domain (snapshot boundary only)
```

- **Domain** (`Hedger.Domain.*`) is the functional core. Every ledger
  calculation lives here as a pure, total function over a `Snapshot`, reachable
  by property tests without `AppM`. hledger *is* the domain vocabulary; its
  types are used directly rather than behind an anti-corruption layer.
- **Application** (`Hedger.Application.*`) is the imperative shell and query
  side. Thin use cases in `AppM` resolve request inputs, read the snapshot and
  config, call the domain, attach `SnapshotMetadata`, and assemble UI-shaped
  read-models. It contains no ledger calculation.
- **Interface** (`Hedger.Interface.HTTP.*`) is the Servant API, wire DTOs, and
  domain-to-DTO mappers. User-facing prose is rendered here from structured
  domain conditions, never baked into the domain.
- **Infrastructure** (`Hedger.Infrastructure.*`) holds the real I/O: journal
  reading, the mutable `SnapshotStore`, and the `JournalWatcher`.
- **Runtime** (`Hedger.Runtime.*`) and `Hedger.Config` are the composition root
  and configuration.

See [backend.md](backend.md) for module-by-module detail.

## Frontend architecture

The Elm app follows a strict ownership model:

```text
Main -> Route / Page
Page -> Data / Api / Ui / View
View -> Ui / Data
Api -> Client / Data decoders
Data -> other Data
Ui -> Theme / DesignTokens
```

`Main.elm` owns URL changes, bootstrap, page lifting, theme state, PWA
integration, and the live-refresh subscription. `Route.elm` is the single source
of truth for routes. `Page.*` modules own page-local TEA. `Api.*` modules own
request construction, decoding, and transport errors. `Data.*` modules own
domain-facing types and pure helpers. `Ui.*` and `View.*` own shared
presentation. See [frontend.md](frontend.md) for detail.

## Snapshot and freshness

The snapshot is the central architectural concept:

1. `SnapshotStore` reads the journal and all `include`s and builds an immutable
   `Snapshot` containing the journal, transactions, account tree, latest journal
   date, and revision.
2. Store state lives in a single STM `TVar` as
   `NotLoaded | LoadFailed error | Fresh snapshot | Stale snapshot error`, so a
   snapshot and its published revision can never be observed independently.
3. Concurrent readers always see a consistent, complete snapshot.
4. `awaitRevisionChange` blocks until a revision different from the caller's
   last-seen revision is published, which is what drives the SSE stream.
5. On a failed reload the previous snapshot is retained and the error is exposed
   as stale freshness.
6. The revision is an MD5 over effective source content (`read-options` plus
   each file as `(path, content)`), so identical content yields an identical
   revision.

There is **no report cache yet**. Every request derives its reports from the
snapshot. Because some of those derivations are expensive at large journal
sizes, caching is the planned next step; see [performance.md](performance.md)
and issue [#17](https://github.com/nixypanda/hedger/issues/17).

## Live updates over Server-Sent Events

`GET /api/v1/events` is a raw streaming endpoint:

- `Application.Events.subscribeToRevisions` records the current revision.
- Each round awaits either a revision change or a 15-second heartbeat.
- A change emits a `revision` event whose data is the new revision; otherwise a
  heartbeat comment keeps the connection alive through proxies.
- `main.js` opens the `EventSource` and forwards events to the `journalChanged`
  Elm port. `Main` then refreshes the current page. `EventSource` reconnects on
  its own, so no retry logic is needed in the app.

Freshness and revision mismatches travel in every response's metadata, so pages
can keep stale content visible while a refresh is in flight and reset paginated
windows when the revision changes.

## Key design decisions

1. **hledger is the domain vocabulary.** Hedger uses hledger types directly in
   the domain layer. There is no anti-corruption layer.
2. **Read-only backend.** The backend never writes to the journal.
3. **No effect system.** Plain `ReaderT Env (ExceptT AppError IO)`.
4. **No database.** In-memory snapshots from the journal file.
5. **One deployment per ledger.** No multi-tenancy.
6. **Functional core, imperative shell.** Pure domain calculations, effects at
   the edges.
7. **API DTOs are Hedger-owned.** Raw hledger types never reach JSON.
8. **Quantities are decimal strings.** Never JSON floating point.
9. **Missing prices are unknown, not zero.**
10. **Reloads are automatic.** A filesystem watcher reloads on change, with
    SIGHUP as an explicit alternative.

## API surface

All routes are under `/api/v1`. Every request optionally carries an
`X-Request-ID` header.

- `/health/*` — liveness and readiness probes
- `/bootstrap` — startup metadata and configuration
- `/accounts/*` — account tree and register
- `/transactions` — hledger query and paginated transaction list
- `/net-worth/query` — net worth analytics
- `/cashflow/query` — cashflow analytics
- `/activity/query` — income/expense/asset/liability activity calendars
- `/investments/query` — investment performance
- `/events` — Server-Sent Events stream of journal revision changes

See [backend.md](backend.md) for route details.

## Deployment

Production uses a NixOS module (`deploy/module.nix`) that runs the backend as a
dedicated unprivileged user per instance, serves the Elm frontend through nginx
with SPA fallback, reverse proxies `/api/` to the backend, applies immutable
caching to hashed static assets, and supports ACME TLS. See
[deployment.md](deployment.md).
