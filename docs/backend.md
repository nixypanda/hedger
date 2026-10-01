# Backend

## Overview

The backend is a Haskell service built with Servant, Warp, and hledger-lib. It
reads an hledger journal into an immutable in-memory snapshot, reloads it
automatically when the files change, and serves JSON under `/api/v1`. It never
writes to the journal.

```text
backend/src/Hedger/
  Config.hs                  # CLI + YAML configuration
  Domain/
    Account.hs               # account tree, balances, valuation
    Activity.hs              # kind-specific activity reports
    ActivityEvent.hs         # typed movements (asset, liability, income, expense)
    Cashflow.hs              # income/expense period reports
    Error.hs                 # domain error types
    Investment.hs            # investment and asset-class mappings
    InvestmentPerformance.hs # cost basis, return, gain attribution
    NetWorth.hs              # balance-sheet stock and return decomposition
    Period.hs                # granularity and period math
    Snapshot.hs              # the immutable snapshot record
    Transaction.hs           # transaction classification and presentation
    Valuation.hs             # display-commodity projection, warnings
    Xirr.hs                  # XIRR solver
    Internal/
      Account.hs             # account tree internals
      Cashflow.hs            # cashflow report internals
      InvestmentPerformance.hs
      NetWorth.hs
      PeriodReport.hs
      Transaction.hs
  Application/
    AppM.hs                  # ReaderT Env (ExceptT AppError IO)
    AccountRegisterQuery.hs  # register query logic
    Accounts.hs              # account tree and register
    Analytics.hs             # net-worth/cashflow/activity facade
    Common.hs                # shared application helpers, metadata
    Events.hs                # revision subscription for SSE
    Investments.hs           # investment query orchestration
    PageRequest.hs           # shared pagination/date-range parsing
    System.hs                # health, bootstrap, status
    TransactionQuery.hs      # hledger query parsing and pagination
    Transactions.hs          # transaction listing
    Internal/
      Cursor.hs              # cursor encoding/decoding
  Interface/
    HTTP/
      API.hs                 # combined Servant type
      Core/
        Common.hs            # shared DTOs, ResponseMetadata, parsing helpers
        Errors.hs            # error envelope
        Middleware.hs        # request context, error normalization
      Endpoints/
        Accounts.hs
        Analytics.hs
        Events.hs            # Server-Sent Events
        Investments.hs
        System.hs
        Transactions.hs
  Infrastructure/
    JournalWatcher.hs        # fsnotify-based automatic reload
    SnapshotStore.hs         # journal loading, STM snapshot publication
  Runtime/
    Server.hs                # public re-export
    Internal/Server.hs       # Warp server, signal handlers, middleware
```

## Layering

```text
Interface.HTTP -> Application -> Domain
Runtime -> Application
Infrastructure -> Domain (snapshot boundary only)
```

The Domain layer has no `AppM` dependency and operates on plain `Snapshot`
values. The Application layer threads `AppM` for snapshot access and
configuration. The Interface layer translates HTTP to application calls.
Infrastructure handles I/O.

## The AppM monad stack

```haskell
type AppM = ReaderT Env (ExceptT AppError IO)

data Env = Env
    { config        :: Config
    , snapshotStore :: SnapshotStore
    }
```

`requireSnapshot` reads the store and returns a `SnapshotContext` carrying the
snapshot and its freshness (`Fresh` or `Stale error`). Handlers run in `AppM`
via `Interface.HTTP.Core.Common.runAction`, which converts `AppError` into the
JSON error envelope. `runAppM` catches synchronous exceptions and maps them to
`InternalError`, rethrowing asynchronous exceptions.

## Configuration

Configuration is parsed once at startup from CLI flags and an optional YAML
file. CLI flags override YAML values. Types are parsed into valid values at the
boundary: the port becomes `Port` (1–65535) and the fiscal-year start becomes a
`MonthOfYear` (1–12).

### YAML format

```yaml
version: 1

journal:
  path: "../examples/india-us-portfolio-2023-2025.journal"

server:
  listenAddress: "127.0.0.1"
  port: 8080

display:
  commodity: "INR"
  fiscalYearStartMonth: 4

investments:
  gainMappings:
    - assetAccount: "assets:debt:fd"
      gainAccounts:
        - "income:interest:fd"
    - assetAccount: "assets:debt:fd:excluded"
      gainAccounts: []
  assetClasses:
    - kind: debt
      accountPrefixes:
        - "assets:debt"
      targetPercent: 35
    - kind: equity
      accountPrefixes:
        - "assets:equity"
      targetPercent: 65
```

### Defaults

| Field                     | CLI flag                | YAML path                      | Default    |
|---------------------------|-------------------------|--------------------------------|------------|
| `journalPath`             | `--journal PATH`        | `journal.path`                 | required   |
| `listenAddress`           | `--listen-address ADDR` | `server.listenAddress`         | `127.0.0.1`|
| `listenPort`              | `--port PORT`           | `server.port`                  | `8080`     |
| `defaultDisplayCommodity` | `--display-commodity C` | `display.commodity`            | `INR`      |
| `fiscalYearStartMonth`    | `--fiscal-year-start M` | `display.fiscalYearStartMonth` | `4`        |
| `investmentMappings`      | (none)                  | `investments.gainMappings`     | empty      |
| `assetClassMappings`      | (none)                  | `investments.assetClasses`     | empty      |

`--config PATH` selects the YAML file.

### Validation

- Journal path must be non-blank.
- Listen address and display commodity must be non-blank, whitespace-free
  tokens.
- Port must be 1..65535.
- Fiscal-year start month must be 1..12.
- An empty `gainAccounts` list explicitly gives an asset account no gain
  accounts. Because the most specific asset-account mapping wins, this can opt a
  child account out of a broader parent mapping.
- Asset-class mappings use `equity`, `debt`, `cash`, `gold`, `hybrid`,
  `property`, or `other`.
- Asset-class account prefixes form the Investments portfolio allowlist.
  Configured prefixes include all descendants; omitted asset accounts do not
  contribute to holdings, performance, allocation, or drill-down. An empty list
  produces an empty Investments view.
- Every asset-class mapping requires a qualified `assets:` account prefix.
  Duplicate and component-prefix-overlapping mappings are rejected.
- `targetPercent` is optional. When any targets are configured, each must be
  between 0 and 100 and they must total 100.

## Domain modules

The domain layer uses hledger types directly. There is no anti-corruption layer —
hledger *is* the domain vocabulary.

- `Snapshot` — the immutable record containing the journal, transaction list,
  account tree, latest journal date, and revision.
- `Account` — account tree traversal, direct/subtree balances, valuation
  projection with completeness status.
- `Transaction` — transaction classification (`income`, `expense`, `transfer`,
  `opening_balance`, `borrowing`, `repayment`, `asset_acquisition`,
  `asset_disposal`, `asset_reallocation`, `unclassified`) and presentation.
- `Investment` — investment and asset-class mappings parsed from configuration.
- `InvestmentPerformance` — cost basis, return percentage, gain attribution
  (attributed income, realised gain, unrealised gain).
- `NetWorth` — period-by-period net worth, assets, liabilities, KPI
  comparisons, trend points, and performance decomposition.
- `Cashflow` — income/expense period reports with category breakdowns.
- `Activity` and `ActivityEvent` — posting-level movements and quantified
  calendars (see below).
- `Valuation` — display-commodity conversion using dated market prices. Missing
  prices produce a `Conversion` with `value = Nothing` and a populated `missing`
  set, plus a `ConversionWarning`.
- `Period` — granularity (monthly/yearly), period-window math, fiscal years.
- `Xirr` — the XIRR solver over dated cash flows.
- `Error` — domain error types.

### Activity model

Activity is a set of four projections over posting-level effects. A transaction
can contribute to several views; each view has its own typed payload.

| View | Movement | Meaning |
|------|----------|---------|
| Assets | `Acquired` | A non-cash asset increased. |
| Assets | `Disposed` | A non-cash asset decreased. |
| Assets | `Transferred` | Value moved between asset accounts; counted once, with source and destination. |
| Liabilities | `Borrowed` | Debt increased. |
| Liabilities | `Repaid` | Debt decreased through repayment. |
| Liabilities | `Refinanced` | One debt replaced another; linked legs, not an additive third total. |
| Liabilities | `Forgiven` | Debt decreased without repayment. |
| Liabilities | `BorrowingReversed` | A borrowing was corrected or reversed. |
| Income | `Posted` / `Reversed` | Income recognised / corrected. |
| Expense | `Posted` / `Reversed` | Expense recognised / corrected (for example a refund). |

Opening balances, equity-only movements, valuation changes without postings, and
unrecognised or ambiguous transactions are outside Activity; they remain visible
through balance, performance, or transaction views. Unknown movements are not
silently labelled transfers. Income and expense retain posted and reversed legs
separately rather than netting them before projection.

## Application modules

Thin use cases in `AppM`. No ledger calculations live here.

- `Analytics` — a facade that bundles the net-worth, cashflow, and activity
  report endpoints. It resolves request inputs, reads the snapshot, calls the
  domain, and attaches `SnapshotMetadata`.
- `Investments` — investment query orchestration with account-prefix drill-down.
- `Transactions` / `TransactionQuery` — hledger query parsing, cursor
  pagination, and transaction-page construction. Unsupported query options
  (`inacct:`, `inacctonly:`, `interval:`) are rejected; query text is capped at
  2048 characters.
- `Accounts` / `AccountRegisterQuery` — account-tree construction and register
  query with date-range filtering.
- `PageRequest` — shared pagination limits and date-range parsing.
- `Events` — `subscribeToRevisions` / `awaitNextRevision`, wrapping the store's
  revision waiter for the SSE endpoint.
- `System` — health probes and the bootstrap response.
- `Internal.Cursor` — cursor encoding (revision + query fingerprint + reference
  date + ordering string).

## Infrastructure

### SnapshotStore

- State lives in one STM `TVar`:
  `NotLoaded | LoadFailed error | Fresh snapshot | Stale snapshot error`.
- `loadSnapshot` reads the journal via hledger-lib and builds a `Snapshot`.
- `reloadSnapshot` atomically publishes the result; on failure it transitions to
  `Stale` and retains the last successful snapshot.
- `readCurrentSnapshot`, `readLastReloadError`, and `readSnapshotStore` expose
  state; `currentRevision` and `awaitRevisionChange` expose the revision for the
  SSE stream.
- `JournalRevision` is an MD5 over effective source content.

### JournalWatcher

The watcher listens for filesystem events (`fsnotify`: FSEvents on macOS,
inotify on Linux) on every file that made up the current snapshot — the root
journal plus any `include`s — and asks the store to reload when one changes. It
is event-driven, so an idle server does no work.

- `fsnotify` watches directories, not files, so each distinct parent directory is
  watched and events are filtered down to the exact journal-file set.
- A save that emits several events is coalesced by a 100 ms debounce.
- After every reload the watch set is re-derived from the fresh snapshot, so new
  includes start being watched and removed ones stop.
- `guardReloaderStep` logs synchronous exceptions and keeps the reloader loop
  alive while letting asynchronous exceptions (such as `ThreadKilled`) through.

## Runtime

`Runtime.Internal.Server`:

- Creates the `SnapshotStore` and performs an initial `reloadSnapshot`.
- Installs a `SIGHUP` handler that reloads the store.
- Configures Warp (host, port, graceful-shutdown timeout, `SIGTERM` shutdown
  handler).
- Runs the server inside `withJournalWatcher`, which owns the watcher thread.

## HTTP routes

All routes are under `/api/v1`. Every request optionally carries an
`X-Request-ID` header.

### System

```text
GET /api/v1/health/live     -> Health { status }
GET /api/v1/health/ready    -> Health { status }
GET /api/v1/bootstrap       -> Bootstrap { apiVersion, appName, fiscalYearStartMonth, metadata }
```

### Accounts

```text
GET /api/v1/accounts/tree?commodity=
    -> AccountsTreeResponse { metadata, displayCommodity, accounts }

GET /api/v1/accounts/{account}/register?commodity=&from=&to=&limit=&cursor=
    -> AccountRegisterResponse { metadata, displayCommodity, entries, nextCursor }
```

`account` is a URL-captured, percent-encoded account path. The register returns
postings for the account and all its descendants.

### Transactions

```text
GET /api/v1/transactions?query=&limit=&cursor=
    -> TransactionsResponse { metadata, transactions, nextCursor }
```

- `query` is an hledger query-language text value; absent or empty matches all.
- `limit` defaults to 50, min 1, max 200.
- `cursor` is an opaque continuation token.
- Query text is capped at 2048 characters.
- Every page contains complete transaction DTOs with all postings.

### Analytics

```text
POST /api/v1/net-worth/query   -> NetWorthResponse
POST /api/v1/cashflow/query    -> CashflowResponse
POST /api/v1/activity/query    -> ActivityResponse
```

Request bodies are JSON. Net worth and cashflow share an
`AnalyticsRequestDto` (`query`, `granularity`, `displayCommodity`,
`positivePeriod`, `negativePeriod`, `positiveDepth`, `negativeDepth`). Activity
uses `ActivityRequestDto` (`kind`, `range`, `period`, `query`,
`displayCommodity`, `depth`).

The Activity response selects its bucket shape by the requested `kind` and
carries typed `events` (the movements listed under "Activity model"). It also
retains the earlier additive `total`, `increase`, `decrease`, `reallocated`,
`borrowed`, `repaid`, `forgiven`, `borrowingReversed`, `posted`, `reversed`,
`categories`, and `directions` fields. Warnings include conversion gaps and
unpaired or unsupported movements. All four kinds apply the supplied transaction
query. Transaction presentation exposes an additive `effects` list alongside the
legacy single `classification` label.

### Investments

```text
POST /api/v1/investments/query -> InvestmentsResponse
```

Supports account-prefix drill-down, monthly/yearly granularity, and breadcrumb
navigation within the account prefixes configured under
`investments.assetClasses`.

### Events

```text
GET /api/v1/events -> text/event-stream
```

A Server-Sent Events stream. It emits a `revision` event when the journal
revision changes and a heartbeat comment every 15 seconds otherwise. See
[architecture.md](architecture.md) for the client side.

## Error shape

HTTP errors use a nested envelope:

```json
{
  "error": {
    "code": "INVALID_QUERY",
    "message": "Query contains unsupported option: interval:",
    "requestId": "a1b2c3",
    "details": {
      "reason": "unsupported option",
      "position": 15
    }
  }
}
```

The `requestId` is generated per request and included in the envelope. The
`query` parameter is never echoed in error responses.

## Known open issues

- **Lazy exceptions can escape the error envelope.** `runAppM` forces only weak
  head normal form around `evaluate`, so an exception thrown later during JSON
  serialization can bypass the `InternalError` envelope.
- **Performance-account roles are not configurable.** Every account in a YAML
  `gainAccounts` list is treated as `OtherIncome`, so the domain's interest,
  dividend, realised-gain, and fee roles cannot be selected from configuration.
- **`gainLoss` versus `purchaseCost` identity is unresolved.** `gainLoss` uses
  the unadjusted reported cost while `purchaseCost` subtracts retained
  performance, so `gainLoss` is not generally `marketValue - purchaseCost`. The
  intended semantic should be named explicitly or reconciled.

## Testing

Tests use `tasty`, `tasty-hedgehog`, `tasty-hunit`, and `hedgehog`.

```text
backend/test/
  Hedger/
    ConfigSpec.hs
    Generators.hs
    SyntheticTransactions.hs
    TestHelpers.hs
    Domain/
      AccountSpec.hs
      ActivitySpec.hs
      CashflowSpec.hs
      InvestmentPerformanceSpec.hs
      InvestmentSpec.hs
      NetWorthSpec.hs
      PeriodSpec.hs
      TransactionSpec.hs
      ValuationSpec.hs
      XirrSpec.hs
    Application/
      AppMSpec.hs
      EventsSpec.hs
      TransactionQuerySpec.hs
      Internal/CursorSpec.hs
    Infrastructure/
      JournalWatcherSpec.hs
      SnapshotStoreSpec.hs
    Interface/HTTP/
      APISpec.hs
      Core/CommonSpec.hs
      Endpoints/EventsSpec.hs
    Runtime/
      ServerSpec.hs
```

Run tests:

```bash
just test-backend
# or directly:
cd backend && cabal test --offline all
```

Hedgehog properties cover account traversal and balance aggregation,
transaction classification, cursor round-trips, period-report generation, and
investment performance with generated transactions.

Shared fixtures live in `backend/test/fixtures/` and include `shared.journal`
(multiple commodities, dated prices, unit/total costs, parent/child accounts),
investment and activity fixtures, boundary fixtures (future prices, gapped
prices, ambiguous XIRR), and `malformed.journal` for error handling.

## Benchmarks

Performance-sensitive operations are benchmarked with `tasty-bench` in
`backend/benchmark/Main.hs` (Cabal suite `snapshot-bench`). See
[performance.md](performance.md) for the case list, methodology, results, and
the approaches that were measured and rejected.

```bash
cd backend && cabal bench all
```

## Naming conventions

### Type suffixes

- `*Dto` — wire transport objects.
- `*Request` / `*Response` — top-level request/response bodies.
- `*Mappings` — configuration-derived mapping types.

### Module organization

- Domain modules are pure and total: no `IO`, no `AppM`.
- Application modules are thin orchestrators in `AppM`.
- Interface modules handle HTTP translation only.
- `Internal.*` modules contain implementation details re-exported by thin public
  modules.
