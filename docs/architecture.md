# Architecture

## System overview

Hedger is a monorepo with a Haskell backend, an Elm web frontend, and Nix-based
build and deployment. The backend is a read-only replica of an hledger journal.

```text
Elm frontend-web (PWA)
  -> HTTP JSON under /api/v1
  -> Server-Sent Events at /api/v1/ledgers/:id/events
Haskell backend (Servant + Warp)
  -> hledger-lib (journal parsing, queries, valuation, cost basis)
  -> one in-memory immutable snapshot per ledger (STM TVar)
  <- fsnotify file watcher and SIGHUP

Nix flake: dev shell, packages, NixOS module
nginx reverse proxy (production)
```

There is no database. Journal files are the sole source of truth. The backend
parses each configured journal into an immutable in-memory snapshot at startup
and reloads it automatically when the underlying files change.

## Runtime data flow

1. **Startup.** Each configured ledger gets a `SnapshotStore` that reads its root
   journal and every `include` via hledger-lib. Stores load independently.
2. **Reload.** `JournalWatcher` watches the files that made up the last snapshot
   and asks the store to reload after a debounced filesystem event. `SIGHUP`
   reloads all stores. A failed reload keeps the last successful snapshot
   and records the error.
3. **Requests.** Handlers read the current snapshot, run pure domain
   calculations, and respond. No request mutates shared state.
4. **Live updates.** `GET /api/v1/ledgers/:id/events` streams revision events
   and reload-state events for that ledger.
5. **Client refresh.** The frontend opens an `EventSource` on that endpoint and
   refetches the visible screens when the revision changes.

## Product boundaries

Hedger is a read-only personal finance viewer:

- Display net worth, cashflow, investment performance, accounts, and
  transactions from an hledger journal.
- One deployment can contain named ledgers that share the same access policy.
- A responsive phone and desktop PWA.
- Nix-based development, builds, and NixOS deployment.

Out of current product scope:

- Ledger editing, transaction creation, or imports.
- Per-ledger authorization within one deployment.
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
Main -> Chrome / Route / Page
Chrome -> View.Chrome / Pwa / Ui / Route
Page -> Component / Data / Api / Ui / View / Lib
Component -> Data / Api / Ui / View / Lib
View -> Ui / Data / Lib
Api -> Api.Client / Api.Json / Data decoders
Data -> other Data / Lib
Ui -> Theme / DesignTokens / Common / Viewport / Data
Lib -> no Data, Api, Page, or View
```

`Main.elm` is the composition root: URL changes, bootstrap, page lifting, the
`Ui.Env` environment (theme, device, viewport), PWA integration, and the
live-refresh subscription. `Chrome.elm` assembles navigation and PWA notices for
`View.Chrome`. `Route.elm` is the single source of truth for routes and document
titles, and `Page.elm` composes route-specific page models and messages.
`Page.*` modules own page-local TEA and receive `Ui.Env`. `Component.*` holds
reusable stateful hosts such as `Component.Analysis`. `Api.*` modules own
request construction, decoding, and transport errors. `Data.*` modules own
domain-facing types, pure helpers, and `Data.Formatting` for display strings.
`Lib.*` holds generic helpers only (`Lib.Number`, `Lib.ListExtra`,
`Lib.RemoteData`, `Lib.Time`) and never imports `Data`. `Ui.*` owns reusable
primitives, layout, theme, `Ui.Page`, `Ui.Workspace`, and `Ui.Env`; `View.*`
composes them into stateless presentation. See [frontend.md](frontend.md) for
detail.

## Snapshot and freshness

Each ledger has its own snapshot:

1. Its `SnapshotStore` reads the journal and all `include`s and builds an immutable
   `Snapshot` containing the journal, transactions, account tree, latest journal
   date, and revision.
2. Each store's state lives in an STM `TVar` as
   `NotLoaded | LoadFailed error | Fresh snapshot | Stale snapshot error`, so a
   snapshot and its published revision can never be observed independently.
3. Concurrent readers always see a consistent, complete snapshot.
4. A reload increments a per-store generation counter. The ledger's SSE stream
   waits for generation changes, including failed reloads.
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

`GET /api/v1/ledgers/:id/events` is a raw streaming endpoint:

- `Application.Events.subscribeToStates` tracks the selected store's state.
- Each round awaits either a reload or a 15-second heartbeat.
- A successful reload emits a `revision` event. A failed reload emits a `state`
  event; a heartbeat comment keeps idle connections alive through proxies.
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
5. **One access policy per deployment.** Separate deployments isolate audiences.
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
