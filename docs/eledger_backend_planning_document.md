**eledger Backend and BFF Planning Document**

*Haskell + hledger-lib, screen-oriented API, in-memory snapshots*

**Status: proposed architecture  
**Primary goal: support decades of ledger data without loading the full
journal into the browser

# 1. Executive summary

eledger should use a long-running Haskell backend as a
backend-for-frontend (BFF). The backend owns hledger parsing, accounting
semantics, report calculation, pagination, caching, and screen-shaped
responses. The Elm client owns presentation and interaction state, but
does not understand hledger internals or load the complete journal.

- Source of truth: hledger journal files and their included files.

- Accounting engine: hledger-lib, used directly from Haskell.

- HTTP stack: Servant + WAI + Warp.

- Application stack: ReaderT Env (ExceptT AppError IO), STM for runtime
  state, no effect system initially.

- Primary cache: one immutable parsed Journal snapshot plus application
  indexes.

- Derived cache: revision-scoped report results; SQLite is explicitly
  deferred.

- Read API: GET for simple retrieval; POST /query for structured complex
  reads until HTTP QUERY support matures across tooling.

- API style: screen-oriented BFF endpoints, not a generic hledger
  command endpoint.

- Functional design: make invalid states unrepresentable, parse at
  boundaries, parse rather than repeatedly validate, keep accounting
  calculations pure.

| **Decision**     | **Choice**                        | **Reason**                                                                  |
|------------------|-----------------------------------|-----------------------------------------------------------------------------|
| Backend language | Haskell                           | Direct use of hledger-lib and simple pure domain code.                      |
| Server           | Servant on Warp                   | Typed routes, codecs and errors with standard WAI deployment.               |
| State model      | Immutable snapshot in TVar        | Concurrent reads with atomic replacement during reload.                     |
| Database         | None initially                    | Approximately 36,500 transactions over 50 years is small in process memory. |
| Complex reads    | POST /api/v1/.../query            | Supports structured request bodies with current client/server tooling.      |
| Frontend loading | Screen projections and pagination | Avoids decoding and rendering the whole journal.                            |

# 2. Product and scale assumptions

- History: up to 50 years.

- Average volume: about 2 transactions/day, approximately 36,500
  transactions.

- Postings: likely under a few hundred thousand, depending on
  transaction complexity.

- Primary use: a personal or small-user ledger UI, not a high-throughput
  multi-tenant accounting SaaS.

- The backend may serve several concurrent browser requests and perform
  file reloads, but expected request volume is modest.

- The journal may contain multiple commodities, price directives,
  account declarations, tags, recurring rules, and included files.

## 2.1 Explicit non-goals for the first version

- Reimplementing hledger parsing or accounting semantics.

- A generic endpoint that exposes arbitrary hledger CLI commands.

- A database-backed canonical ledger.

- Distributed caching, Redis, queues, or multiple backend replicas.

- A general effect system or capability typeclass for every operation.

- Incremental journal parsing before measurements show it is necessary.

- Perfect source-preserving edits of arbitrary historical journal text
  in the first read-focused release.

# 3. Architectural principles

## 3.1 Domain-driven design, kept practical

DDD here means separating the accounting domain and application use
cases from transport, files, and concurrency. It does not require
repositories, interfaces, aggregates, or services for every type. The
journal is the source aggregate supplied by hledger; eledger adds
application-specific concepts such as JournalSnapshot, TransactionId,
DateRange, ReportInterval, NetWorthView, and cursor pages.

| **Layer**      | **Responsibility**                                                                               | **May depend on**                                               |
|----------------|--------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| Domain         | Validated values, screen projections, pure report transformations, identifiers and invariants.   | base, containers, vector, hledger domain types behind adapters. |
| Application    | Use cases such as load bootstrap, query transactions, build net-worth screen and reload journal. | Domain and ports represented by explicit functions/records.     |
| Infrastructure | hledger reader, filesystem watcher, hashing, clock and file writes.                              | hledger-lib, fsnotify, cryptonite, IO.                          |
| Delivery       | Servant routes, JSON DTOs, HTTP status/error translation, ETag headers.                          | Application layer, Servant, Aeson.                              |

## 3.2 Make invalid states unrepresentable

Use constructors that establish invariants once, then let internal code
operate on trusted values.

newtype TransactionId = TransactionId Text  
newtype JournalRevision = JournalRevision Text  
newtype PageSize = PageSize Int  
newtype AccountName = AccountName Text  
  
data DateRange = DateRange  
{ fromInclusive :: Day  
, toExclusive :: Day  
}  
  
data Interval = Monthly \| Yearly  
  
data NetWorthQuery = NetWorthQuery  
{ range :: DateRange  
, interval :: Interval  
, basis :: ValuationBasis  
}

- PageSize constructor enforces a range such as 1 to 200.

- DateRange constructor enforces fromInclusive \< toExclusive.

- Cursor decoding produces a TransactionCursor or a parse error;
  handlers never accept raw cursor Text internally.

- Represent money quantities as Decimal/Text-backed values, never
  floating-point JSON numbers.

- Separate persisted transactions from generated forecast occurrences at
  the type level.

## 3.3 Parse, do not validate

At every boundary, convert untrusted representations into domain types.
Internal functions should not repeatedly check strings, optional dates,
or arbitrary enum values. A successful parser returns a value that
already satisfies the invariant.

| **Boundary input**  | **Parser output**               |
|---------------------|---------------------------------|
| JSON date strings   | Day                             |
| from/to fields      | DateRange                       |
| limit integer       | PageSize                        |
| cursor string       | TransactionCursor               |
| account path string | AccountSelector                 |
| valuation JSON      | ValuationBasis                  |
| journal files       | LoadedJournal / JournalSnapshot |
| raw report request  | Canonical ReportKey             |

## 3.4 Push parsing and rendering to boundaries

HTTP JSON -\> Boundary DTO -\> parsed command/query -\> application use
case  
hledger Journal -\> domain projection -\> response DTO -\> JSON  
filesystem -\> hledger parser -\> immutable snapshot

Do not derive API JSON directly from hledger types. The API contract
must remain stable even if hledger data types or field names change.

## 3.5 Functional core, imperative shell

- Pure: filtering, sorting, pagination, account-tree projection,
  report-key canonicalization and DTO conversion.

- Imperative: read files, parse journal, watch filesystem, atomically
  publish snapshot, serve HTTP and write journal changes.

- No TVar, AppM, Servant ServerError, or filesystem logic inside report
  calculation modules.

# 4. Bounded contexts and domain model

| **Context**        | **Core concepts**                                                 | **Responsibilities**                                            |
|--------------------|-------------------------------------------------------------------|-----------------------------------------------------------------|
| Journal            | JournalSnapshot, JournalRevision, JournalStatus, SourceLocation   | Load, parse, validate, reload and expose diagnostics.           |
| Transactions       | TransactionSummary, TransactionDetail, TransactionCursor, Posting | Timeline, detail views, filtering and future write operations.  |
| Accounts           | AccountTree, AccountSummary, RegisterRow                          | Hierarchy, balances, register and account autocomplete.         |
| Reporting          | DateRange, Interval, ValuationBasis, TimeSeries, Breakdown        | Net worth, income/expense, dashboard and statement projections. |
| Recurring/Forecast | RecurringRule, ForecastOccurrence                                 | Rules and generated future occurrences remain distinct.         |
| Import             | ImportPreview, ParsedImportRow, ImportIssue                       | Future CSV preview and acceptance around hledger readers.       |

## 4.1 Key domain types

data JournalSnapshot = JournalSnapshot  
{ snapshotRevision :: JournalRevision  
, snapshotJournal :: Journal  
, snapshotIndexes :: Indexes  
, snapshotLoadedAt :: UTCTime  
}  
  
data Indexes = Indexes  
{ transactionsNewestFirst :: Vector IndexedTransaction  
, transactionById :: HashMap TransactionId Int  
, postingsByAccount :: Map AccountName (Vector PostingRef)  
, accountTree :: AccountTree  
}  
  
data AppError  
= NotFound ResourceRef  
\| InvalidRequest ParseIssue  
\| JournalUnavailable  
\| JournalReloadFailed JournalIssue  
\| RevisionConflict ExpectedRevision ActualRevision  
\| ReportFailure ReportIssue

# 5. Runtime architecture and libraries

Elm application  
\|  
\| JSON/HTTP  
v  
Servant API / Warp  
\|  
v  
Application use cases  
\|  
+---- Journal snapshot and indexes (memory)  
+---- Revision-scoped report cache (memory)  
\|  
v  
hledger-lib  
\|  
v  
Journal files and included files

| **Concern**                         | **Recommended package**                      |
|-------------------------------------|----------------------------------------------|
| HTTP API                            | servant, servant-server                      |
| Server/runtime                      | wai, warp, wai-extra                         |
| JSON                                | aeson                                        |
| Application monad                   | mtl, transformers                            |
| Concurrent runtime state            | stm                                          |
| Worker supervision                  | async                                        |
| Ordered collections                 | containers                                   |
| Opaque-ID lookup                    | unordered-containers                         |
| Ordered compact transaction storage | vector                                       |
| Journal engine                      | hledger-lib                                  |
| File watcher                        | fsnotify                                     |
| Hashes/revisions                    | cryptonite                                   |
| CLI configuration                   | optparse-applicative                         |
| Logging                             | fast-logger or a small structured logger     |
| Tests                               | hspec, QuickCheck, servant-client, temporary |

## 5.1 Application monad

newtype AppM a = AppM  
{ runAppMInternal :: ReaderT Env (ExceptT AppError IO) a  
}  
deriving newtype  
(Functor, Applicative, Monad, MonadIO,  
MonadReader Env, MonadError AppError)  
  
data Env = Env  
{ snapshotVar :: TVar JournalSnapshot  
, reportCache :: TVar ReportCache  
, reloadQueue :: TChan ReloadReason  
, journalLock :: TMVar ()  
, config :: Config  
, logger :: Logger  
}

No dedicated effect system is required. For tests, prefer pure functions
and explicit service records at the few true IO boundaries rather than
introducing broad capability typeclasses.

# 6. In-memory state, caching and indexing

## 6.1 Primary cache: parsed immutable snapshot

The parsed Journal is the primary cache. It is loaded once, indexed, and
placed in a TVar. Every request obtains a snapshot reference and then
works without holding a lock.

snapshot \<- readTVarIO snapshotVar  
-- all subsequent work uses this immutable value

## 6.2 Index choices

| **Need**                          | **Structure**                       | **Reason**                                                         |
|-----------------------------------|-------------------------------------|--------------------------------------------------------------------|
| Newest-first transaction timeline | Vector IndexedTransaction           | Compact ordered storage and efficient slicing.                     |
| Transaction ID lookup             | HashMap TransactionId Int           | Map stable IDs to vector offsets without duplicating transactions. |
| Account hierarchy                 | Tree plus Map AccountName ...       | Deterministic order and natural hierarchy operations.              |
| Posting register by account       | Map AccountName (Vector PostingRef) | Direct register access and ordered rows.                           |
| Date slicing                      | Binary search on date-sorted Vector | Avoid another index until profiling requires it.                   |
| Cached reports                    | Map ReportKey ReportValue           | Deterministic keys and naturally small initial cache.              |

## 6.3 Report cache

data ReportKey = ReportKey  
{ reportKind :: ReportKind  
, dateRange :: DateRange  
, interval :: Interval  
, valuation :: ValuationBasis  
, query :: CanonicalLedgerQuery  
}  
  
data ReportCache = ReportCache  
{ cacheRevision :: JournalRevision  
, cacheEntries :: Map ReportKey ReportValue  
}

- A journal revision change discards the entire derived report cache.

- Compute reports outside STM; insert only if the current revision still
  matches the snapshot used for calculation.

- Initially tolerate duplicate concurrent calculations for the same
  missing key.

- Add per-key single-flight TMVars only if measurements show duplicate
  work is material.

- No TTL or LRU initially. The number of screen-shaped report keys
  should be small.

## 6.4 When SQLite would be introduced

- Startup reparsing becomes meaningfully slow.

- Full-text search or large import history is required.

- Application metadata no longer belongs in journal tags or a small
  sidecar file.

- Multiple users or processes require a persistent shared projection.

The database must remain a disposable projection that can be rebuilt
from journal files.

# 7. Concurrency model

| **Concurrent activity** | **Mechanism**                            | **Invariant**                                                     |
|-------------------------|------------------------------------------|-------------------------------------------------------------------|
| HTTP reads              | Warp threads read TVar snapshot          | Each request sees one complete immutable revision.                |
| Journal file changes    | fsnotify -\> TChan -\> one reload worker | Reloads are serialized and coalesced.                             |
| Snapshot publication    | Single STM write                         | No partial journal/index state is visible.                        |
| Report cache            | TVar with revision checks                | A report from revision A never enters revision B cache.           |
| Future journal writes   | TMVar lock + If-Match revision           | Writes are serialized and stale clients cannot overwrite changes. |

reload worker:  
1. receive/coalesce file events  
2. parse journal outside STM  
3. build indexes outside STM  
4. atomically replace snapshot and reset report cache  
5. retain old snapshot if parsing fails

Accounting and projection functions remain pure. Concurrency exists at
the shell around file reloads, HTTP requests, caches, and future writes.

# 8. API conventions

- Base path: /api/v1.

- GET for simple, cacheable resource retrieval.

- POST to /query endpoints for complex structured reads while HTTP QUERY
  support remains inconsistent in Servant tooling and intermediaries.

- POST/PUT/DELETE for mutations.

- Every successful response includes journalRevision; simple GET
  responses also expose it as ETag.

- Quantities are JSON strings, not floating-point numbers.

- Dates are ISO 8601 dates; ranges use inclusive start and exclusive
  end.

- Cursors are opaque Base64URL-encoded values.

- Errors have stable machine-readable codes and human-readable messages.

data ApiError = ApiError  
{ code :: ErrorCode  
, message :: Text  
, details :: Maybe Value  
, revision :: Maybe JournalRevision  
}

## 8.1 Query model

The client sends structured filters for ordinary screens. An optional
advancedQuery field may carry hledger query syntax for power users. The
backend parses both into one CanonicalLedgerQuery before application
logic runs.

data TransactionQueryRequest = TransactionQueryRequest  
{ dateRange :: Maybe DateRangeDto  
, accounts :: \[Text\]  
, text :: Maybe Text  
, statuses :: \[StatusDto\]  
, tags :: Map Text \[Text\]  
, commodities :: \[Text\]  
, advancedQuery :: Maybe Text  
, pagination :: CursorPageRequest  
}

# 9. Bootstrap contract

Bootstrap provides small, stable information needed before the
application can route and render. It must not return the complete
account tree, all transactions, or large reports.

GET /api/v1/bootstrap  
  
{  
"journalRevision": "...",  
"journal": {  
"transactionCount": 36514,  
"postingCount": 98122,  
"earliestDate": "1976-01-03",  
"latestDate": "2026-07-17",  
"reloadStatus": "idle",  
"lastSuccessfulLoad": "2026-07-17T00:42:00Z"  
},  
"preferences": {  
"baseCommodity": "INR",  
"defaultValuation": "end",  
"firstDayOfWeek": "monday"  
},  
"capabilities": {  
"writeTransactions": false,  
"recurringRules": true,  
"csvImport": false  
}  
}

- Client calls bootstrap on initial load and after a hard revision
  mismatch.

- A journal parse failure may still return bootstrap using the last
  successful snapshot plus reloadStatus=failed and diagnostics metadata.

- The endpoint is ETag-enabled.

# 10. Screen-oriented BFF endpoints and behavior

Each main screen receives one primary projection endpoint. Secondary
detail or pagination endpoints are allowed, but the UI should not
compose a screen by reproducing accounting logic across many low-level
calls.

## 10.1 Net worth screen

The net-worth endpoint supplies all data required by the screen:
headline values, chart series, period comparison, asset/liability
breakdown, and available range metadata. The frontend should not
separately fetch accounts, prices, and transactions to calculate net
worth.

POST /api/v1/net-worth/query  
  
{  
"interval": "monthly" \| "yearly",  
"anchorDate": "2026-07-17", // optional; defaults to journal latest
date  
"valuation": { "type": "end" },  
"commodity": "INR",  
"accountDepth": 2,  
"includeLiabilities": true  
}

### Monthly behavior

- Find the latest applicable date: explicit anchorDate when supplied,
  otherwise the latest journal/report date.

- Return exactly 24 monthly periods ending with the month containing the
  latest applicable date.

- Use complete calendar months for chart buckets. The latest bucket may
  be month-to-date when the latest date is inside the month.

- Include opening value before the first returned month when required to
  calculate each month’s change.

- Return assets, liabilities, and net worth for every month, not only a
  net-worth scalar.

### Yearly behavior

- Return all calendar years represented by the journal, from earliest
  data year through the latest applicable year.

- The latest year may be year-to-date.

- Use one point per year, normally valued at the final available date in
  that year.

- Do not arbitrarily cap the number of years; 50 points is small.

### Net-worth response projection

{  
"journalRevision": "...",  
"range": {  
"from": "2025-08-01",  
"toExclusive": "2026-08-01",  
"interval": "monthly",  
"isPartialFinalPeriod": true  
},  
"headline": {  
"netWorth": { "quantity": "4200000.00", "commodity": "INR" },  
"assets": { "quantity": "4700000.00", "commodity": "INR" },  
"liabilities": { "quantity": "500000.00", "commodity": "INR" },  
"absoluteChange": { "quantity": "350000.00", "commodity": "INR" },  
"percentageChange": "9.09"  
},  
"series": \[  
{  
"period": "2026-07",  
"asOf": "2026-07-17",  
"assets": { ... },  
"liabilities": { ... },  
"netWorth": { ... },  
"change": { ... }  
}  
\],  
"assetBreakdown": \[  
{ "account": "Assets:Investments", "amount": { ... }, "percentage":
"62.3" }  
\],  
"liabilityBreakdown": \[ ... \],  
"availableRange": { "earliestDate": "1976-01-03", "latestDate":
"2026-07-17" }  
}

- Breakdowns are for the final selected period and follow requested
  accountDepth.

- Assets and liabilities use account-type semantics rather than relying
  only on account-name prefixes.

- Valuation behavior is explicit and included in the response metadata.

- The endpoint response is one cacheable ReportValue keyed by interval,
  resolved range, valuation, commodity, depth, and revision.

## 10.2 Dashboard/home screen

GET /api/v1/dashboard?period=2026-07

- Returns current-period income, expenses, net cash change, current
  assets/liabilities, recent transactions, and top expense categories.

- Uses one coherent period and valuation configuration so totals cannot
  disagree across cards.

- Recent transactions are capped, for example 10 items, and use
  TransactionSummary.

- When period is absent, resolve the month containing the latest journal
  date, not necessarily the wall-clock month.

- Include navigation hints: previousPeriod, nextPeriod when available,
  and whether the period is partial.

## 10.3 Transactions screen

POST /api/v1/transactions/query  
GET /api/v1/transactions/{transactionId}

- Initial screen request returns the newest 50-100 matching
  transactions.

- Cursor order is descending (date, stable transaction ID).

- The client retains only a bounded number of pages and may discard
  distant pages.

- List rows use a compact TransactionSummary; opening a transaction
  fetches TransactionDetail.

- Filters include date range, account, text/payee, status, tags,
  commodity, and optional advanced hledger query.

- Response includes active filter summary and next cursor so the UI does
  not recreate filter semantics.

## 10.4 Accounts screen

GET /api/v1/accounts/tree?asOf=2026-07-17&depth=3  
GET /api/v1/accounts?search=bank&limit=20  
GET /api/v1/accounts/{accountName}  
POST /api/v1/accounts/{accountName}/register/query

- Account tree is returned already nested and includes direct and
  inclusive balances as required by the screen.

- Account detail includes declaration metadata, account type, first/last
  activity, posting counts, parent, and children.

- Register rows are posting-oriented and contain transaction link,
  change, running balance, and counter-accounts.

- Register pagination is cursor-based. Running balance must remain
  correct across pages; cursor state therefore includes or allows
  reconstruction of the balance at the page boundary.

- Autocomplete uses a small flat account search endpoint rather than
  downloading the complete tree into every form.

## 10.5 Income and expense screens

POST /api/v1/income-expense/query

- Returns headline income, expense, savings/net amount, period series,
  and category breakdown in one response.

- Supports monthly and yearly intervals with an explicit date range.

- Category hierarchy and depth are resolved on the server.

- A category selection can include a drilldown query descriptor for
  navigation to matching transactions.

## 10.6 Generic reports screen

POST /api/v1/reports/timeseries/query  
POST /api/v1/reports/breakdown/query  
POST /api/v1/reports/statements/query

- These endpoints support advanced/report-builder screens, not the
  primary home or net-worth screens.

- Timeseries metrics include balance, change, income, expense, net
  worth, and cash flow.

- Breakdown groupings include account, tag, payee, commodity, month, and
  year where semantically valid.

- Statement responses use a generic hierarchical table model for balance
  sheet, income statement, and cash-flow style views.

- The backend canonicalizes semantically equivalent request bodies to
  the same ReportKey.

## 10.7 Global search

GET /api/v1/search?q=grocery&types=transaction,account&limit=20

- Optimized for navigation and command-palette style suggestions, not
  complete report filtering.

- Returns typed result variants with labels, snippets, destination
  routes, and stable IDs.

- Initial implementation may scan in-memory indexes; SQLite FTS can
  replace it later without changing the API.

## 10.8 Recurring and forecast screens

GET /api/v1/recurring-rules  
GET /api/v1/recurring-rules/{ruleId}  
POST /api/v1/forecast/query

- Persisted periodic rules and generated forecast occurrences use
  distinct DTOs and identifiers.

- Forecast queries require a bounded date range.

- Forecast screen may return summary series plus a paginated list of
  generated occurrences.

- Generated occurrences are clearly marked as non-persisted and cannot
  be edited as ordinary transactions.

## 10.9 Commodities and prices

GET /api/v1/commodities  
POST /api/v1/prices/query

- Returns commodity metadata and price series needed for investment or
  valuation screens.

- Every report request specifies valuation behavior; no hidden
  conversion to INR.

- Original amounts, costs, and market values remain distinguishable in
  DTOs.

## 10.10 Journal status and diagnostics

GET /api/v1/journal/status  
GET /api/v1/journal/diagnostics  
POST /api/v1/journal/reload  
GET /api/v1/events // optional SSE

- A failed reload retains the last successful snapshot.

- Diagnostics include file, line/range when available, severity, stable
  error code, and message.

- Manual reload is available for debugging and explicit refresh.

- SSE can publish journal.reloading, journal.reloaded, and
  journal.reload-failed; WebSockets are unnecessary initially.

## 10.11 Future write behavior

POST /api/v1/transaction-drafts/parse  
POST /api/v1/transactions  
PUT /api/v1/transactions/{transactionId}  
DELETE /api/v1/transactions/{transactionId}

- Draft endpoint parses a structured input into a balanced transaction
  preview or structured parse issues.

- Mutations require If-Match with the journal revision used by the
  client.

- All writes are serialized through the journal lock, use temporary-file
  plus atomic rename, then reparse before publishing a new snapshot.

- Appending is simpler and should precede arbitrary source-preserving
  edits.

- Explicit UUID tags are preferred for transactions created by eledger;
  existing transactions receive deterministic derived IDs.

# 11. Identity, source locations and revisions

- Transaction IDs must not be source line numbers; inserting lines would
  invalidate them.

- Prefer an explicit UUID tag for transactions created or adopted by
  eledger.

- For legacy transactions, derive an ID from normalized transaction
  content plus an occurrence discriminator for exact duplicates.

- SourceLocation remains metadata for opening/editing the journal, not
  identity.

- JournalRevision is a hash over all effective input content and
  relevant read options.

- All response pages belong to a revision. A cursor from another
  revision is rejected or causes a controlled refresh.

# 12. Servant API shape

type API =  
"api" :\> "v1" :\> "bootstrap"  
:\> Get '\[JSON\] BootstrapResponse  
  
:\<\|\> "api" :\> "v1" :\> "dashboard"  
:\> QueryParam "period" YearMonthParam  
:\> Get '\[JSON\] DashboardResponse  
  
:\<\|\> "api" :\> "v1" :\> "net-worth" :\> "query"  
:\> ReqBody '\[JSON\] NetWorthQueryDto  
:\> Post '\[JSON\] NetWorthResponse  
  
:\<\|\> "api" :\> "v1" :\> "transactions" :\> "query"  
:\> ReqBody '\[JSON\] TransactionQueryDto  
:\> Post '\[JSON\] TransactionPageDto  
  
:\<\|\> "api" :\> "v1" :\> "transactions"  
:\> Capture "transactionId" TransactionIdParam  
:\> Get '\[JSON\] TransactionDetailDto  
  
:\<\|\> AccountsAPI  
:\<\|\> ReportsAPI  
:\<\|\> JournalAPI

Keep the type-level API split by feature modules. Translate boundary
DTOs to parsed domain queries before invoking application functions.

# 13. Suggested module organization

Eledger.Domain.Common  
Eledger.Domain.Journal  
Eledger.Domain.Transaction  
Eledger.Domain.Account  
Eledger.Domain.Report  
Eledger.Domain.NetWorth  
Eledger.Domain.Recurring  
  
Eledger.Application.Bootstrap  
Eledger.Application.Dashboard  
Eledger.Application.NetWorth  
Eledger.Application.Transactions  
Eledger.Application.Accounts  
Eledger.Application.Reports  
Eledger.Application.Journal  
  
Eledger.Infrastructure.Hledger.Load  
Eledger.Infrastructure.Hledger.Query  
Eledger.Infrastructure.Hledger.Report  
Eledger.Infrastructure.Journal.Revision  
Eledger.Infrastructure.Journal.Watch  
Eledger.Infrastructure.Cache  
  
Eledger.Api.Types  
Eledger.Api.Error  
Eledger.Api.Server  
Eledger.Api.Bootstrap  
Eledger.Api.Dashboard  
Eledger.Api.NetWorth  
Eledger.Api.Transactions  
Eledger.Api.Accounts  
Eledger.Api.Reports  
Eledger.Api.Journal  
  
Eledger.Runtime.Env  
Eledger.Runtime.ReloadWorker  
Eledger.Runtime.Server

- Only infrastructure adapter modules should import the broad hledger
  API wherever practical.

- Application modules work with eledger domain projections and small
  hledger adapter functions, not raw Servant request types.

- API modules contain DTO parsing and response rendering but no
  accounting logic.

# 14. Representative request flows

## 14.1 Net-worth request

1.  Aeson decodes NetWorthQueryDto.

2.  Boundary parser constructs Interval, AnchorDate, ValuationBasis,
    Commodity and AccountDepth.

3.  Application resolves the effective range: 24 months ending at latest
    date for monthly, all represented years for yearly.

4.  Application constructs a canonical ReportKey and checks the
    revision-scoped cache.

5.  On miss, pure/hledger adapter functions calculate asset, liability,
    net-worth series and final-period breakdowns.

6.  Application returns NetWorthView; API converts it to stable JSON DTO
    and adds revision/ETag metadata.

## 14.2 Journal reload

7.  fsnotify emits one or more file events.

8.  Watcher enqueues/coalesces ReloadReason values.

9.  Single reload worker acquires the journal lock.

10. hledger-lib parses and finalizes all effective input files outside
    STM.

11. Indexes and revision are built outside STM.

12. One STM transaction publishes the snapshot, resets report cache, and
    updates reload status.

13. On failure, old snapshot remains active and diagnostics/status are
    updated.

# 15. Performance and response-size rules

- Never send the complete journal during bootstrap or screen
  initialization.

- Transaction list default page: 50; maximum page: 200.

- Chart points: monthly net worth returns 24 points by default; yearly
  returns at most the number of journal years.

- Account tree may be complete if account count is small, but
  transaction/posting histories remain paginated.

- Use summary/detail DTO separation.

- Enable gzip at WAI middleware for JSON responses.

- Measure parsing time, index-build time, key report time, response
  size, and frontend decode/render time before adding complexity.

# 16. Testing strategy

| **Test type** | **Examples**                                                                                                            |
|---------------|-------------------------------------------------------------------------------------------------------------------------|
| Unit/pure     | Date-range resolution, 12-month rule, all-years rule, cursor ordering, account hierarchy, ID derivation.                |
| Property      | Pagination concatenation has no gaps/duplicates; cursor round-trip; equivalent report queries canonicalize identically. |
| Golden        | Fixed journal -\> stable JSON projection for bootstrap, dashboard, net worth, transactions and account register.        |
| Integration   | In-memory WAI app calls; successful and failed reload; revision conflict; cache invalidation.                           |
| Compatibility | Representative hledger journals: includes, multiple commodities, prices, tags, periodic rules and balance assertions.   |
| Performance   | 50-year generated journal benchmark for parse, index, monthly/yearly net worth and transaction paging.                  |

## 16.1 Essential net-worth acceptance tests

- Monthly query with latest date 2026-07-17 returns periods 2025-08
  through 2026-07.

- Monthly query remains exactly 24 buckets when some months contain no
  transactions.

- Yearly query returns every represented year, including years with zero
  net change when needed for continuity.

- Final monthly/yearly period is marked partial when latest date
  precedes period end.

- Assets - liabilities equals net worth under the selected sign
  convention.

- Breakdown total reconciles with headline assets/liabilities within
  defined rounding rules.

- A price/valuation change produces a new revision/report result and
  does not reuse a stale cached value.

# 17. Logging and operational behavior

- Structured request log: request ID, route, status, duration, response
  size, journal revision.

- Reload log: reason, affected files, parse duration, index duration,
  transaction/posting counts, success/failure.

- Report log: report kind, normalized range, cache hit/miss, calculation
  duration.

- Health endpoint distinguishes process alive, usable last snapshot, and
  latest reload failure.

- Critical workers are supervised with async; a silent dead reload
  worker is not acceptable.

# 18. Implementation sequence

| **Phase**                | **Deliverable**                                                                                    |
|--------------------------|----------------------------------------------------------------------------------------------------|
| 1\. Skeleton             | Servant/Warp server, AppM, config, errors, health and bootstrap with a static fixture.             |
| 2\. Journal snapshot     | hledger load, revision hash, immutable indexes, TVar publication, diagnostics.                     |
| 3\. Transaction browsing | POST transactions/query, detail endpoint, stable IDs, cursor pagination.                           |
| 4\. Accounts             | Account tree, account detail, account register and autocomplete.                                   |
| 5\. Net worth            | Monthly 12-bucket behavior, yearly all-years behavior, headline and breakdown projection.          |
| 6\. Dashboard            | One screen-shaped home projection and recent transactions.                                         |
| 7\. Report cache/reload  | Revision-scoped cache, fsnotify, reload worker, failed-reload behavior and SSE if useful.          |
| 8\. Other reports        | Income/expense, timeseries, breakdown and statements.                                              |
| 9\. Recurring/forecast   | Rules, generated occurrences and future summary screens.                                           |
| 10\. Writes/import       | Draft parsing, append transactions, optimistic concurrency, then source-safe edits and CSV import. |

# 19. Open decisions to resolve during implementation

- Exact transaction ID tag name and migration behavior for legacy
  transactions.

- Sign convention in API DTOs for liabilities and expenses.

- Default valuation basis for net worth and behavior when market prices
  are missing.

- Whether the latest chart bucket uses latest journal date or current
  wall-clock date when forecast data exists.

- Account-depth defaults for desktop and mobile layouts.

- Whether include-subaccounts is implicit for account-screen registers
  or user-selectable.

- How much source formatting must be preserved once arbitrary editing is
  added.

# 20. Final recommended baseline

Backend:  
Haskell + hledger-lib  
Servant + Warp  
ReaderT Env (ExceptT AppError IO)  
  
State:  
TVar immutable JournalSnapshot  
Vector transactions  
HashMap TransactionId -\> vector offset  
Map AccountName -\> posting references/tree  
revision-scoped Map report cache  
  
Concurrency:  
concurrent Warp readers  
one fsnotify-driven reload worker  
STM snapshot publication  
serialized future journal writes  
  
API:  
GET for simple resources  
POST /query for complex safe reads temporarily  
screen-oriented BFF projections  
bootstrap is small  
net-worth monthly = latest 24 months
net-worth yearly = all represented years  
  
Design:  
parse at boundaries  
make invalid states unrepresentable  
pure domain/report functions  
stable eledger DTOs, not raw hledger JSON  
SQLite only after a measured need
