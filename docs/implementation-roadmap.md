# Hedger Implementation Roadmap

## Purpose

This document is the implementation plan for Hedger: a backend-backed,
read-only replica of eLedger. It consolidates the product inventory and backend
architecture in:

- `docs/backend-replication.md`
- `docs/eledger_backend_planning_document.md`

Those documents remain the detailed sources for user-visible behavior,
calculation rules, and proposed data shapes. This document defines the order in
which the product should be built and the completion gate for each stage.

## Release 1 scope

The first release will have:

- one Hedger deployment per ledger;
- a Haskell backend using `hledger-lib`, Servant, WAI, and Warp;
- an Elm PWA served from the same origin as the API;
- read-only access to Net Worth, Investments, Cashflow, Accounts,
  Transactions, and Settings;
- in-memory immutable journal snapshots and revision-scoped report caches;
- Nix-based development, builds, tests, and deployment;
- responsive phone and desktop layouts;
- cached application assets and previously fetched business data;
- explicit fresh, stale, refreshing, partial, unavailable, and failed states.

Release 1 does not include ledger editing, imports, a canonical database,
multi-ledger selection, multi-tenancy, global search, forecasting, or generic
report builders. These can be added after the replica is complete.

## Decisions to hold constant during Release 1

1. The journal and included files are the source of truth.
2. The backend owns parsing, indexes, filtering, valuation, classification,
   pagination, cost basis, and all financial calculations.
3. Elm owns rendering and transient interaction state: routing, drawers, draft
   filters, selections, expanded nodes, breadcrumbs, responsive composition,
   and PWA lifecycle.
4. API DTOs are Hedger-owned types. Raw `hledger-lib` types and hledger-web
   JSON must not become the public API.
5. All responses used together must belong to one immutable journal revision.
6. Monetary quantities cross the API as decimal strings, never JSON floating
   point numbers.
7. Missing prices and costs are unknown, not zero.
8. The first deployment model is intentionally one backend, one frontend, and
   one ledger. Do not introduce a ledger selector yet.
9. Preferences remain browser-local in Release 1 unless a concrete
   multi-device requirement appears.
10. The API is under `/api/v1`; simple and safely bounded read queries use
    `GET`, while larger screen-shaped analytical requests may use `POST`.
11. Transaction filtering uses one hledger query-language text value parsed and
    evaluated by `hledger-lib`. Do not add a parallel Hedger-owned structured
    transaction-filter JSON model.

## Target repository layout

Create the two application roots before copying or implementing application
code:

```text
hedger/
├── backend/
│   ├── app/
│   ├── src/Hedger/
│   │   ├── Api/
│   │   ├── Application/
│   │   ├── Domain/
│   │   ├── Infrastructure/
│   │   └── Runtime/
│   ├── test/
│   │   ├── fixtures/
│   │   ├── golden/
│   │   ├── integration/
│   │   └── unit/
│   ├── hedger-backend.cabal
│   └── cabal.project
├── frontend-web/
│   ├── public/
│   ├── src/
│   │   ├── Api/
│   │   ├── Page/
│   │   ├── Route.elm
│   │   ├── Shared.elm
│   │   └── Ui/
│   ├── review/
│   ├── tests/
│   ├── elm.json
│   └── package.json
├── deploy/
│   ├── module.nix
│   └── example-ledger.nix
├── docs/
├── flake.nix
├── flake.lock
└── justfile
```

The exact Elm source layout may follow eLedger where doing so makes copying
safer. The important boundary is that reusable presentation code lives apart
from API DTOs and page loading logic.

## What to reuse from eLedger

Before copying, record the source eLedger commit. Copy in small, reviewable
batches and keep a temporary porting checklist that maps every copied module to
its original path and Hedger destination.

### Copy with minimal changes

- `Ui/` primitives, tokens, typography, icons, controls, cards, dialogs,
  tables, skeletons, empty states, and responsive helpers;
- theme definitions and light/dark behavior;
- chart drawing and interaction primitives that operate on already prepared
  series;
- page layout components such as the wide sidebar, narrow bottom navigation,
  split list/detail layout, headers, and content containers;
- PWA manifest, icons, install prompt UI, update prompt UI, and app-shell
  service-worker behavior;
- review/component-gallery setup, screenshot helpers, formatting, linting, and
  Elm test configuration;
- generic date, decimal-display, color, and accessibility helpers whose
  semantics are not coupled to the old API.

### Copy, then adapt deliberately

- routes and navigation labels: retain the visible route set but rename the
  application to Hedger;
- page views: retain layout and interaction state, replace data models and
  loading commands with Hedger API DTOs;
- charts: retain rendering, hover/selection, legends, and responsive behavior;
  remove aggregation, conversion, and accounting calculations;
- transaction queries: retain draft/apply/clear UI and validation display, but
  replace any structured filter builder with one hledger query text field;
- stale-data UI: retain the presentation but drive it from response metadata
  and journal status;
- local preferences: retain theme/display commodity/cash-account UX while
  preserving the three cash-account modes: automatic, manual non-empty, and
  manual empty.

### Do not copy as product architecture

- bulk loading of all transactions, accounts, and prices at startup;
- Elm implementations of conversion, net worth, cashflow, investment cost
  basis, activity classification, or account aggregation;
- reliance on transaction array indexes as stable IDs;
- direct decoding of hledger-web wire types;
- service-worker rules that route `/api/*` through the app-shell cache;
- any eLedger name, package identifier, cache name, storage key, icon, or
  deployment setting that could collide with the original app.

## Common definition of done

Every stage below is complete only when:

- code is formatted and passes unit and integration tests under Nix;
- API behavior has a fixture or contract test, including its error case;
- the corresponding Elm view covers loading, success, empty, stale, and failed
  states, plus partial/unavailable states where relevant;
- the view works at representative phone and desktop widths;
- keyboard use, focus, labels, contrast, and reduced-motion behavior have been
  checked;
- no browser calculation duplicates a backend accounting rule;
- no response silently replaces unknown financial data with zero;
- logs do not include journal contents, transaction descriptions, or complete
  response bodies;
- the change is represented in the review gallery and/or screenshot baseline
  when it introduces reusable UI.

## Implementation sequence

### Phase 0 — Establish the workspace and contracts

Create `backend/` and `frontend-web/`, then add the root Nix flake and task
runner. The development shell should provide GHC/Cabal, Elm, Node tooling,
formatters, test tools, and any browser-test dependencies. CI must invoke the
same Nix-backed commands used locally.

Add a small shared fixture journal that includes:

- assets, liabilities, income, expenses, and equity;
- an opening balance and transfers;
- multiple commodities and dated prices;
- unit and total costs;
- a refund;
- a missing price and a missing investment cost;
- parent and child accounts;
- enough dates to cross month and year boundaries.

Resolve and write down these API decisions before analytics work starts:

- liability and expense sign conventions;
- default valuation basis;
- latest-period behavior when journal and wall-clock dates differ;
- fiscal-year start month, initially `4` for April;
- exact-account register semantics versus subtree-filter semantics.

Use an OpenAPI document or generated Servant schema as the contract boundary.
Store representative JSON responses as golden fixtures that Elm decoder tests
can consume.

**Gate:** one command enters the dev shell; one command formats/checks both
projects; the shared journal fixture and recorded decisions exist.

### Phase 1 — Build the backend skeleton

Create the Haskell executable with:

- Servant routes mounted at `/api/v1`;
- Warp server startup and graceful shutdown;
- `ReaderT Env (ExceptT AppError IO)` application plumbing;
- configuration parsed once at startup;
- request IDs and standard WAI access logs;
- stable JSON error envelopes;
- `GET /api/v1/health/live` for process health;
- `GET /api/v1/health/ready` for usable-snapshot readiness;
- a static `GET /api/v1/bootstrap` response backed by the fixture.

Configuration should include the journal path, listen address/port, default
display commodity, fiscal-year start, log level, and optional public base URL.
The journal path must be a runtime path and must not copy private ledger data
into the Nix store.

**Gate:** the packaged server starts under Nix, validates configuration, serves
health/bootstrap, returns stable errors, and shuts down cleanly.

### Phase 2 — Load and publish journal snapshots

Implement the infrastructure boundary around `hledger-lib`:

1. Load the root journal and all effective includes.
2. Parse and finalize it outside STM.
3. Build an immutable `JournalSnapshot` containing the journal, revision,
   load time, transaction indexes, transaction-ID lookup, posting-by-account
   index, account tree, commodities, and price data.
4. Hash all effective source content and relevant read options into a
   `JournalRevision`.
5. Atomically publish a complete snapshot through a `TVar`.
6. Retain the last successful snapshot when a subsequent reload fails.
7. Expose parse diagnostics without exposing unrelated private file content.

Expand bootstrap to return small startup metadata, commodities, account filter
options, capabilities, revision, source freshness, and reload status. It must
not return the complete journal or complete account/transaction collections.

Add `GET /api/v1/status` and `POST /api/v1/journal/reload`. File watching and
event streaming can wait until Phase 10; manual reload is enough for the first
vertical slices.

**Gate:** changing the fixture produces one new revision; a broken edit keeps
the previous snapshot usable and exposes diagnostics; concurrent readers never
observe partially built indexes.

### Phase 3 — Port the Elm foundation and application shell

Create the Elm app before implementing a financial screen. Copy the approved
eLedger foundations in separate commits:

1. build/test/review configuration;
2. `Ui/` and theme tokens;
3. responsive page layouts and navigation;
4. chart primitives;
5. PWA manifest, install/update UI, and service worker;
6. review gallery and visual fixtures.

Add Hedger-specific modules for:

- route parsing and URL generation;
- API origin and JSON request helpers;
- shared response metadata and error decoding;
- decimal quantities represented without `Float` at the transport boundary;
- remote-data states that preserve stale content during refresh;
- bootstrap loading and application initialization;
- local preference versioning and migration;
- revision mismatch handling.

Create placeholder routes for `/net-worth`, `/investments`, `/cashflow`,
`/accounts`, `/transactions`, and `/settings`; `/` redirects to `/net-worth`.
Placeholders should render inside the real responsive shell so navigation and
layout are settled once.

The service worker may cache versioned static assets and intentionally stored
successful business responses. It must bypass app-shell routing for
`/api/*` and must never turn an API failure into `index.html`.

**Gate:** the shell installs as Hedger, navigation works by URL and browser
history, phone/desktop layouts match the intended composition, theme persists,
and bootstrap failure/staleness is visible.

### Phase 4 — First complete screen: Transactions

Build Transactions first in two parts. Part A establishes the complete DTO and
responsive screen with the smallest possible read path. Part B replaces the
temporary all-at-once fetch with the Release 1 query and pagination contract.
Together they validate complete transaction DTOs, hledger query parsing and
matching, cursor pagination, refresh behavior, and responsive list/detail
composition without requiring the hardest analytical reports.

#### Phase 4A — Fetch and display the complete transaction list

Backend:

- `GET /api/v1/transactions` returning every transaction newest first;
- complete transaction DTOs containing every posting and cost needed by the
  list preview and selected detail;
- no query, limit, cursor, Hedger-specific transaction identifier,
  transaction-detail endpoint, or summary/detail DTO split;
- response metadata identifying the journal revision and freshness;
- measure response construction, JSON size, and fixture/representative-journal
  behavior so Part B has a recorded baseline.

Frontend:

- fetch the complete list when the Transactions route loads and render all of
  it;
- month-grouped transaction rows with posting previews;
- wide list/detail and narrow replace-list-with-detail layouts;
- full selected transaction detail with every posting, commodity, and cost;
- retain the selected complete DTO locally while its revision remains current;
  clear selection safely when the revision changes;
- loading, success, empty, stale, refreshing, and failed states.

This all-at-once endpoint is intentional implementation scaffolding. Part B
replaces it before Release 1; bulk transaction loading is not the final API or
performance target.

**Gate:** the fixture's complete transaction list renders correctly on phone
and desktop; list previews and selected detail use the same complete DTO; the
measured baseline is recorded; no transaction identity or detail fetch is
introduced.

#### Phase 4B — Add hledger text querying and cursor limits

Backend:

- extend the endpoint to
  `GET /api/v1/transactions?query=...&limit=...&cursor=...`;
- accept only one hledger query-language text parameter; an absent or empty
  `query` matches all transactions, and there is no JSON request DTO or
  structured transaction-filter model;
- use `limit` for the requested page size, with a default of `50`, a minimum of
  `1`, and a server-enforced maximum of `200`;
- return an opaque `nextCursor` when another page exists; bind cursors to the
  canonical query, journal revision, ordering, and relative-date reference;
- parse the query with the repository's pinned `hledger-lib` version and apply
  the resulting `Query` with hledger's transaction-matching semantics;
- support hledger account, description, payee, note, code, date, secondary-date,
  status, commodity, amount, tag, negation, and boolean query expressions,
  including `expr:`, `any:`, and `all:` semantics;
- return detailed, stable validation errors for invalid syntax and unsupported
  report options, including a safe parser reason and position when available,
  without echoing the complete query;
- resolve relative date expressions against one documented reference date and
  preserve that date across cursor pages;
- reject a cursor used with a different query or journal revision;
- cap query length, URL-encode it correctly, and redact the `query` parameter
  from access logs and operational telemetry;
- return complete transaction DTOs in every page.

Frontend:

- replace the all-at-once request with the limited endpoint;
- one hledger query text field with draft, Apply, Cancel, Clear, examples, and
  concise query-language help;
- show detailed parser errors next to the input while preserving the user's
  draft for correction;
- no structured date, amount, commodity, account, or status controls and no
  structured filter chips;
- load-more/infinite paging with a bounded page window;
- clear paging and selection when the applied query or revision changes;
- no-matches recovery that keeps the query editable and offers Clear;
- selection and scroll behavior that remain sensible after refresh.

**Gate:** representative hledger queries, invalid syntax, unsupported options,
query-length limits, and cursor conflicts have backend tests; cursor pages
concatenate with no gaps or duplicates; fixture results match equivalent
hledger queries; detailed errors are usable without leaking the complete query;
the responsive screen works with bounded pages; neither the API nor frontend
introduces a Hedger transaction identifier or structured filter model.

### Phase 5 — Accounts and account register

Phase 5 is split into two deliberately small vertical slices. Both slices
should reuse `hledger-lib` account structures and calculations wherever they
cover the required behavior. Add Hedger-specific code only for the immutable
snapshot boundary, API DTO mapping, response metadata, routing, and the
smallest amount of application state needed by the UI.

#### Phase 5A — Account tree and register placeholder

Backend:

- use hledger's account-tree facilities, such as `accountFromPostings`,
  `accountsFromPostings`, `accountTree`, and `sumAccounts`, rather than
  reimplementing account hierarchy and balance aggregation;
- add the hledger-derived account tree to the immutable journal snapshot;
- map hledger account nodes into Hedger-owned tree DTOs without exposing raw
  `hledger-lib` report types;
- preserve direct/exclusive and subtree/inclusive balances separately;
- add `GET /api/v1/accounts/tree`;
- include journal revision, freshness, and reload warnings in the response;
- do not add account autocomplete, a separate account-detail endpoint, or
  register query behavior;
- make account selection route to a temporary register placeholder response
  or page so the tree can be exercised before register implementation begins.

Frontend:

- port the relevant eLedger `Page/Account` implementation and supporting UI
  with the smallest necessary changes for Hedger API DTOs and routes;
- retain the hierarchical tree, expansion state, type labels/icons, parent
  totals, loading, empty, stale, and failed states;
- make selecting an account navigate to `/accounts/:name` with safe account
  path encoding;
- render a clearly identified dummy register page when an account is opened;
- avoid adding account search, register filters, or new client-side balance
  calculations.

**Phase 5A gate:** the fixture account tree is served from hledger-derived
  data, parent totals do not double-count children, account routes round-trip
  safely, and selecting an account opens the dummy register page on phone and
  desktop layouts.

#### Phase 5B — Exact-account register

Backend:

- reuse hledger's posting/account data and ordering helpers wherever possible;
- add the smallest exact-account posting index needed to avoid rescanning the
  complete journal for every register request;
- implement `GET /api/v1/accounts/{accountPath}/register?limit=...&cursor=...`;
- support only `limit` and opaque `cursor` parameters—no hledger query text,
  date filters, subtree switch, or JSON request body;
- return postings owned by the exact account only;
- calculate running balances using hledger amounts and the existing account
  balance semantics, while ensuring balances remain correct across pages;
- bind cursors to the account, journal revision, and stable register ordering;
- return stable errors for invalid limits, invalid/conflicting cursors, missing
  accounts, and unavailable snapshots;
- include conversion completeness and warnings only where existing backend
  valuation behavior requires them.

Frontend:

- port the relevant eLedger `Page/AccountRegister` implementation and
  supporting UI with the smallest necessary changes for Hedger DTOs;
- replace the dummy page with the real exact-account register;
- retain month grouping, wide columns, compact narrow rows, back navigation,
  loading, empty, stale, partial, and failed states;
- use cursor pagination without introducing register query controls or
  client-side accounting calculations.

**Phase 5B gate:** exact-account register semantics are covered by fixture and
  contract tests, running balances remain correct across cursor pages, encoded
  account paths round-trip through routes, and the copied register view works
  at representative phone and desktop widths.

### Phase 6 — Third screen and default route: Net Worth

Implement the first analytics endpoint as a screen-shaped projection, not as
low-level account/price/transaction responses that Elm must combine.

Backend:

- `POST /api/v1/net-worth/query` returning headline values, KPI comparisons,
  trend points, assets/liabilities/net-worth series, selected-period category
  breakdowns, Net Investment versus Net Worth, and completeness warnings;
- monthly mode with 24 buckets ending at the effective latest month;
- yearly mode covering all represented years;
- dated conversion without future prices;
- liabilities displayed as positive magnitude and subtracted from assets;
- category depth 1–4 and `Other` grouping;
- `GET /api/v1/activity/query` or equivalent for asset and liability calendar
  navigation, returning complete contributing transaction DTOs where the UI
  needs to inspect a transaction.

Frontend:

- current Net Worth, Assets, and Liabilities cards with comparisons/trends;
- timeline selection and monthly/yearly switching;
- synchronized breakdown period navigation, depth, selection, and `Other`;
- Net Investment performance view;
- month and April-to-March year activity views;
- day selection and local detail display from a complete transaction DTO;
- explicit incomplete-history and missing-conversion states.

**Gate:** headline and breakdowns reconcile within defined decimal rounding;
monthly/yearly golden tests pass; activity results contain the same transactions
as eLedger; `/` now provides a complete default experience.

### Phase 7 — Fourth screen: Cashflow

Reuse the backend hledger query parser and matching boundary so Transactions and
Cashflow cannot drift in filter meaning, while retaining independent query text
in Elm.

Backend:

- `POST /api/v1/cashflow/query` returning total and filtered counts,
  income/expense/net series, and selected income/expense breakdowns;
- activity queries for income and expense calendars;
- transfer/opening-balance exclusion and refund reduction;
- half-sum-of-absolute-postings magnitude behavior;
- all-or-nothing chart availability when a participating transaction cannot be
  fully converted;
- category depth 1–4 and `Other` grouping.

Frontend:

- query draft/apply/clear/error UX reused from Transactions;
- monthly/yearly timeline and selected-period synchronization;
- separate Income and Expenses breakdowns;
- separate Income and Expenses calendars;
- empty result and conversion-unavailable states;
- calendar day to local detail display from a complete transaction DTO;
- no general transaction list on this screen.

**Gate:** one applied query affects count, timeline, breakdowns, and calendars
together; transfer/refund/opening fixtures match eLedger; failed conversion is
never presented as a partial or zero chart.

### Phase 8 — Fifth screen: Investments

Backend:

- `POST /api/v1/investments/query` for the whole portfolio or one account
  subtree;
- market value, remaining purchase cost, gain/loss, optional return percentage,
  historical points, immediate children, breadcrumbs, and completeness;
- investment policy excluding `assets:cash`, `assets:property`, and
  `assets:receivables`, expressed as versioned configuration/policy;
- explicit unit/total cost handling, transfer-preserved basis, priced disposal,
  dated period-end valuation, and structured missing-cost/price warnings;
- return percentage only for positive, complete cost basis.

Frontend:

- portfolio KPI cards and monthly/yearly chart;
- period selection;
- one-segment-at-a-time account drilldown;
- breadcrumbs, child summaries, and `hasChildren` affordances;
- partial-result messaging without inventing performance values.

**Gate:** cost-basis fixtures cover purchases, internal transfers, disposals,
missing costs, and multi-commodity prices; drilldown and selected periods remain
valid after refresh when the referenced data still exists.

### Phase 9 — Sixth screen: Settings

Settings closes the basic replica because it depends on bootstrap, status,
reload, account options, commodities, and all shared stale/error behaviors.

Implement:

- immediate light/dark theme changes;
- searchable display-commodity selection;
- automatic, manual non-empty, and intentionally manual-empty cash-account
  selection;
- reconciliation when account/commodity options change;
- backend/hledger version and connection status;
- journal revision, source timestamp, last successful load, last reload error,
  and freshness;
- manual refresh with progress and outcome;
- optional browser-local reset/debug actions that do not expose journal data.

Do not add server-side preference endpoints yet unless authentication and
multi-device synchronization are introduced together.

**Gate:** all three cash-account modes survive reload and storage migration;
obsolete choices reconcile predictably; failed refresh retains and labels the
last successful data.

### Phase 10 — Reloading, caching, offline behavior, and operations

Once all screens use revisioned responses:

1. Add an `fsnotify` watcher and one supervised reload worker.
2. Coalesce events, parse/build outside STM, and atomically publish success.
3. Reset the derived report cache only when a new revision is published.
4. Key reports by revision and canonical query; add single-flight or eviction
   only if measurements justify them.
5. Add ETags/`If-None-Match` to simple reads and a consistent revision conflict
   response for queries/cursors.
6. Add optional SSE only if polling/manual refresh is materially worse.
7. Persist only deliberate API response caches in the PWA, including response
   metadata and revision; keep them separate from the static app-shell cache.
8. Add structured measurements for parse, index, report, request, response-size,
   cache-hit, and reload duration.
9. Verify recovery after backend downtime, offline startup, failed reload, and
   service-worker update.

**Gate:** the UI keeps usable stale data during refresh/outage, never combines
revisions, and clearly distinguishes offline, stale, refreshing, partial,
unavailable, and fatal states.

### Phase 11 — Nix deployment for one ledger

Produce three Nix outputs:

- the `hedger-backend` executable;
- static `frontend-web` assets;
- a NixOS module (or the repository's chosen equivalent) that runs one Hedger
  instance and serves both under one origin.

The deployment module should accept an instance configuration similar to:

```nix
services.hedger.instances.personal = {
  enable = true;
  journalPath = "/var/lib/hedger/personal/main.journal";
  hostName = "ledger.example.net";
  defaultCommodity = "INR";
  fiscalYearStartMonth = 4;
};
```

The module should:

- run the backend as a dedicated unprivileged user with read-only access to the
  journal tree;
- provide the journal path at runtime rather than embedding it in a derivation;
- reverse proxy `/api/` to the backend and serve the Elm assets for other
  routes, including SPA fallback;
- apply compression and sensible immutable caching to hashed static assets;
- disable app-shell fallback for `/api/`;
- restart safely, expose readiness, and retain useful structured logs;
- support TLS at the reverse proxy or document the expected upstream proxy;
- make the per-ledger data directory, port, and hostname unique.

Authentication is a deployment concern for Release 1. If an instance is not on
a trusted private network, protect the entire same-origin application at the
proxy. Do not add a client-selectable ledger parameter as an authentication
substitute.

**Gate:** a clean Nix build produces both applications; a disposable deployment
starts against a fixture mounted at runtime; deep frontend routes, `/api/v1`,
health checks, reload, PWA update, and restart all work.

### Phase 12 — Parity, performance, and release hardening

Run the complete acceptance pass:

- compare every screen with eLedger using the same representative journals;
- verify phone, tablet, and desktop behavior;
- verify keyboard and screen-reader basics;
- test includes, tags, statuses, balance assertions, multiple commodities,
  dated prices, unit/total costs, duplicates, and empty journals;
- generate a roughly 50-year benchmark ledger and measure parse, index, report,
  JSON size, Elm decode, and rendering time;
- keep transaction pages at 50 by default and 200 maximum;
- confirm bootstrap does not grow with complete history;
- confirm no screen downloads the complete journal;
- complete the review gallery and stable screenshot suite;
- document installation, configuration, backup expectations, upgrades, and
  troubleshooting.

**Gate:** all replica acceptance criteria in `backend-replication.md` pass, the
Nix deployment is reproducible, and measured performance does not require a
database or extra distributed infrastructure.

## API-to-screen dependency map

| Screen/shared area | Primary backend capability |
| --- | --- |
| App shell | bootstrap, readiness, response metadata |
| Transactions | hledger text query and complete paginated transaction DTOs |
| Accounts | account tree, account detail, exact-account register |
| Net Worth | net-worth projection, asset/liability activity |
| Cashflow | shared transaction query parser, cashflow projection, income/expense activity |
| Investments | investment projection and account-subtree drilldown |
| Settings | bootstrap options, status, diagnostics, manual reload |
| PWA/offline | revisions, ETags, freshness/completeness metadata |

## Test pyramid and review workflow

Backend tests should include pure unit/property tests for hledger query
integration, conversion, classification, cursors, pagination, account
aggregation, and cost basis; golden journal-to-JSON tests for each screen
projection; and WAI integration tests for routes, errors, reload, cache
invalidation, and revision conflicts.

Frontend tests should include JSON decoder tests against backend goldens,
update-function tests for query text/selections/navigation, review-gallery
states for reusable components, browser tests for each route's main flow, and
screenshots at fixed narrow and wide widths.

For each vertical slice, review in this order:

1. fixture and expected financial result;
2. API contract and error/partial behavior;
3. backend pure calculation and integration tests;
4. Elm decoding and state transitions;
5. responsive visual review and accessibility;
6. Nix build and deployment smoke test.

## Deferred roadmap

After Release 1 parity is stable, reconsider these independently and only with
new requirements:

- server-synchronized preferences and authentication;
- multiple ledgers in one deployment;
- dashboard/home projections beyond the existing Net Worth default;
- global search and generic reports;
- recurring/forecast screens;
- transaction creation, append-only writes, edits, and imports;
- SQLite as a rebuildable projection if measured startup/search needs justify
  it;
- multiple backend replicas or distributed caches.

Moving from one deployment per ledger to a shared multi-ledger service is an
architecture milestone, not a configuration toggle. It requires authenticated
ledger authorization, tenant-scoped IDs/caches/logging, and a migration plan;
it should not be partially implemented during the replica build.

## Recommended milestone summary

| Milestone | User-visible result |
| --- | --- |
| 0–2 | Reproducible workspace and a revisioned backend that safely loads a ledger |
| 3 | Installable Hedger shell with real bootstrap/status behavior |
| 4 | Transactions list, filters, and detail |
| 5 | Account tree and exact-account register |
| 6 | Complete Net Worth default screen |
| 7 | Complete Cashflow screen |
| 8 | Complete Investments screen |
| 9 | Complete Settings/status experience |
| 10–11 | Reliable refresh/offline behavior and one-ledger Nix deployment |
| 12 | Verified eLedger parity and Release 1 readiness |
