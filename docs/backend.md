# Backend

## Overview

The backend is a Haskell service built with Servant, Warp, and hledger-lib.
The code is organized by architectural layer:

```text
backend/src/Hedger/
  Config.hs                  # CLI + YAML configuration
  Domain/
    Account.hs               # account tree, balances, valuation
    Transaction.hs           # transaction classification and presentation
    Investment.hs            # investment account mappings
    InvestmentPerformance.hs # cost basis, return calculation
    NetWorth.hs              # balance-sheet stock and return decomposition
    Cashflow.hs              # income/expense period reports
    Activity.hs              # quantified transaction classification
    Valuation.hs             # display-commodity projection, warnings
    Period.hs                # granularity and period math
    Snapshot.hs              # the immutable snapshot record
    Error.hs                 # domain error types
    Internal/
      PeriodReport.hs        # period report internals
      Account.hs             # account tree internals
      InvestmentPerformance.hs
  Application/
    AppM.hs                  # ReaderT Env (ExceptT AppError IO)
    Analytics.hs             # net-worth/cashflow/activity facade
    Investments.hs           # investment query orchestration
    Transactions.hs          # transaction listing
    TransactionQuery.hs      # hledger query parsing and pagination
    Accounts.hs              # account tree and register
    AccountRegisterQuery.hs  # register query logic
    System.hs                # health, bootstrap, status
    Common.hs                # shared application helpers
    Internal/
      Cursor.hs              # cursor encoding/decoding
  Interface/
    HTTP/
      API.hs                 # combined Servant type
      Accounts.hs            # account endpoints
      Transactions.hs        # transaction endpoints
      Analytics.hs           # analytics endpoints
      Investments.hs         # investment endpoints
      System.hs              # system endpoints
      Common.hs              # shared DTOs, ResponseMetadata
      Errors.hs              # error envelope
      Middleware.hs          # request context, error normalization
  Infrastructure/
    SnapshotStore.hs         # journal loading, IORef snapshot publication
  Runtime/
    Server.hs                # Warp server, signal handlers, middleware
```

## DDD Layering

The backend follows a functional-core / imperative-shell design:

```text
Interface.HTTP -> Application -> Domain
Runtime -> Application
Infrastructure -> Domain (snapshot boundary only)
```

The Domain layer has no `AppM` dependency. It operates on plain `Snapshot`
values. The Application layer threads `AppM` for snapshot access and
configuration. The Interface layer translates HTTP to application calls.
Infrastructure handles I/O.

## The AppM Monad Stack

```haskell
type AppM = ReaderT Env (ExceptT AppError IO)

data Env = Env
    { config     :: Config
    , store      :: SnapshotStore
    }

data AppError
    = BadRequest Text
    | NotFound Text
    | ServerError Text
    | ...
```

`Env` carries the parsed configuration and the snapshot store. Handlers run
in `AppM` via `Interface.HTTP.Common.runAction`, which converts `AppError`
into the JSON error envelope.

## Configuration

Configuration is parsed once at startup from CLI flags and an optional YAML
file. CLI flags override YAML values.

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

| Field                    | CLI flag                | YAML path                     | Default   |
|--------------------------|-------------------------|-------------------------------|-----------|
| `journalPath`            | `--journal PATH`        | `journal.path`                | required  |
| `listenAddress`          | `--listen-address ADDR` | `server.listenAddress`        | `127.0.0.1` |
| `listenPort`             | `--port PORT`           | `server.port`                 | `8080`    |
| `defaultDisplayCommodity`| `--display-commodity C` | `display.commodity`           | `INR`     |
| `fiscalYearStartMonth`   | `--fiscal-year-start M` | `display.fiscalYearStartMonth`| `4`       |
| `investmentMappings`     | (none)                  | `investments.gainMappings`    | empty     |
| `assetClassMappings`     | (none)                  | `investments.assetClasses`    | empty portfolio |

### Validation

- Journal path must be non-blank.
- Listen address and display commodity must be non-blank, non-space tokens.
- Port must be 1..65535.
- Fiscal year start month must be 1..12.
- An empty `gainAccounts` list explicitly gives an asset account no gain
  accounts. Because the most specific asset-account mapping wins, this can opt
  a child account out of a broader parent mapping.
- Asset-class mappings use `equity`, `debt`, `cash`, `gold`, `hybrid`,
  `property`, or `other`.
- Asset-class account prefixes form the Investments portfolio allowlist.
  Configured prefixes include all descendants; omitted asset accounts do not
  contribute to investment holdings, performance, allocation, or drilldown.
  An empty list therefore produces an empty Investments view.
- Every asset-class mapping requires a qualified `assets:` account prefix.
  Duplicate and component-prefix-overlapping mappings are rejected.
- `targetPercent` is optional. When any asset-class targets are configured,
  their percentages must each be between 0 and 100 and must total 100.

## Domain Modules

The domain layer uses hledger types directly. There is no anti-corruption
layer — hledger *is* the domain vocabulary.

- `Snapshot` — the immutable record containing the journal, transaction list,
  account tree, latest journal date, and revision.
- `Account` — account tree traversal, direct/subtree balances, valuation
  projection with completeness status.
- `Transaction` — transaction classification (`income`, `expense`, `transfer`,
  `opening_balance`, `borrowing`, `repayment`, `asset_acquisition`,
  `asset_disposal`, `asset_reallocation`, `unclassified`) and presentation.
- `Investment` — investment account mappings from configuration.
- `InvestmentPerformance` — cost basis calculation, return percentage, gain
  attribution (attributed income, realised gain, unrealised gain).
- `NetWorth` — period-by-period net worth, assets, liabilities, KPI
  comparisons, trend points, performance decomposition.
- `Cashflow` — income/expense period reports with category breakdowns.
- `Activity` and `ActivityEvent` — posting-level effects and quantified
  calendars for income, expense, asset, and liability categories. A transaction
  can contribute to multiple views; the domain returns kind-specific buckets.
- `Valuation` — display-commodity conversion using dated market prices. Missing
  prices produce `Conversion` with `value = Nothing` and populated `missing`
  set, plus `ConversionWarning`.
- `Period` — granularity (monthly/yearly), period window math, fiscal year
  support.
- `Error` — domain error types.

## Application Modules

Thin use cases in `AppM`. No ledger calculations live here.

- `Analytics` — facade that bundles net-worth, cashflow, and activity report
  endpoints. Resolves request inputs, reads the snapshot, calls the domain,
  attaches `ResponseMetadata`.
- `Investments` — investment query orchestration with account-prefix
  drilldown.
- `Transactions` / `TransactionQuery` — hledger query parsing, cursor
  pagination, transaction page construction. Rejects unsupported query options
  (`inacct:`, `inacctonly:`, `interval:`). Caps query length at 2048 chars.
- `Accounts` / `AccountRegisterQuery` — account tree construction and
  register query with date range filtering.
- `System` — health probes and bootstrap response.
- `Internal.Cursor` — cursor encoding (revision + query fingerprint +
  reference date + ordering string).

## Infrastructure

- `SnapshotStore` — IORef-based mutable snapshot container.
  - `loadSnapshot` — reads journal via hledger-lib, builds `Snapshot`.
  - `reloadSnapshot` — atomically publishes new snapshot, retains last
    successful on failure, exposes `readLastReloadError`.
  - `readCurrentSnapshot` — returns the current snapshot.
  - MD5-based `JournalRevision` from effective source content.

There is no STM, no fsnotify file watcher, and no supervised reload worker.
Reloading is triggered by SIGHUP (`Runtime.Server` installs a signal handler).

## HTTP Routes

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

`account` is a URL-captured account path (percent-encoded). The register
returns postings for the account and all its descendants.

### Transactions

```text
GET /api/v1/transactions?query=&limit=&cursor=
    -> TransactionsResponse { metadata, transactions, nextCursor }
```

- `query` is an hledger query-language text value. Absent or empty matches all.
- `limit` defaults to 50, min 1, max 200.
- `cursor` is an opaque continuation token.
- Query text capped at 2048 characters.
- Complete transaction DTOs with every posting in every page.

### Analytics

```text
POST /api/v1/net-worth/query   -> NetWorthResponse
POST /api/v1/cashflow/query    -> CashflowResponse
POST /api/v1/activity/query    -> ActivityResponse
```

Request bodies are JSON. See [api-decisions.md](api-decisions.md) for field
details.

The Activity response retains the legacy `total`, `increase`, `decrease`,
`reallocated`, `categories`, and `directions` fields while the Elm frontend is
being migrated. Buckets additionally expose `events` with explicit movement
names and linked source/destination amounts, flow `posted`/`reversed` totals,
and debt `borrowed`/`repaid`/`forgiven`/`borrowingReversed` totals. `warnings`
includes conversion gaps and unpaired or unsupported movements. Transaction
presentation also exposes an additive `effects` list alongside the legacy
single `classification` label. All four Activity kinds apply the supplied
transaction query.

### Investments

```text
POST /api/v1/investments/query -> InvestmentsResponse
```

Supports account-prefix drilldown, monthly/yearly granularity, and breadcrumb
navigation within the account prefixes configured under
`investments.assetClasses`.

## Error Shape

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

## Testing

Tests use `tasty`, `tasty-hedgehog`, `tasty-hunit`, and `hedgehog`.

```text
backend/test/
  Hedger/
    ConfigSpec.hs
    Domain/
      AccountSpec.hs
      TransactionSpec.hs
      InvestmentPerformanceSpec.hs
      NetWorthSpec.hs
      CashflowSpec.hs
      ActivitySpec.hs
      ValuationSpec.hs
      PeriodSpec.hs
    Application/
      TransactionQuerySpec.hs
      AccountRegisterQuerySpec.hs
      AnalyticsSpec.hs
      InvestmentsSpec.hs
    Infrastructure/
      SnapshotStoreSpec.hs
    Interface/HTTP/
      AccountsSpec.hs
      TransactionsSpec.hs
      AnalyticsSpec.hs
      InvestmentsSpec.hs
      SystemSpec.hs
    Generator.hs              # Hedgehog generators for domain types
```

Run tests:

```bash
just test-backend
# or directly:
cd backend && cabal test --offline all
```

### Property tests

Hedgehog properties cover:

- Account tree traversal and balance aggregation.
- Transaction classification correctness.
- Cursor round-trip (encode/decode preserves semantics).
- Period report generation with generated inputs.
- Investment performance calculation with generated transactions.

### Fixtures

Shared fixture journals live in `backend/test/fixtures/`:

- `shared.journal` — 13 transactions, 27 postings, multiple commodities,
  dated prices, unit/total costs, parent/child accounts.
- `investments.journal`, `investment-descendant.journal`,
  `investment-foreign-cash.journal` — investment-specific fixtures.
- `malformed.journal` — for error handling tests.

## Benchmarks

Benchmarks use `tasty-bench` in `backend/benchmark/Main.hs`.

```bash
cd backend && cabal bench all
```

Benchmarked operations:

- `snapshot/parse-shared-13` — journal parse time.
- `query/shared-13` and `query/large-50000` — account and boolean query
  matching with pagination.
- `investments/shared-13` and `investments/large-50000` — investment
  performance calculation.
- `transactions/project/shared-13` and `project/scaled-1300` — DTO
  projection.
- `transactions/encode/shared-13` and `encode/scaled-1300` — JSON encoding.

Large-journal benchmarks use a 50,000-transaction fixture at
`examples/performance/india-us-portfolio-large-50000.journal`.

## Naming Conventions

### Type suffixes

- `*Dto` — wire transport objects.
- `*Request` / `*Response` — top-level request/response bodies.
- `*Mappings` — configuration-derived mapping types.

### Module organization

- Domain modules are pure and total. No `IO`, no `AppM`.
- Application modules are thin orchestrators in `AppM`.
- Interface modules handle HTTP translation only.
- `Internal.*` modules contain implementation details re-exported by thin
  public modules.
