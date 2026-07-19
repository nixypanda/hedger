# Backend Replication: User Features And API Contract

## Purpose

This document describes what a user can see and do in the current eLedger
application, evaluates whether `docs/user-stories.md` is complete, and proposes
a backend API for reproducing the product without moving ledger calculations
into every client.

The implementation, rather than the backlog, is the source of truth for this
inventory. The relevant routes are `/net-worth`, `/investments`, `/cashflow`,
`/accounts`, `/accounts/:name`, `/transactions`, and `/settings`. The root route
also opens Net Worth.

The application is read-only. Creating, editing, importing, or deleting ledger
transactions is outside the replication scope.

## Executive Summary

`docs/user-stories.md` covers the major product capabilities and most important
financial rules. It is a good behavioral overview, but it does not cover every
user-visible detail and is not sufficient by itself as a backend contract.

The main omissions are:

- navigation, responsive composition, and the precise information visible in
  transaction detail and account registers;
- transaction count, filter validation, search/autocomplete, and the exact
  meaning of amount and account filters;
- category grouping into `Other`, the supported depth range, and the April to
  March year-calendar window currently used by the implementation;
- Settings search behavior and the distinction between automatic, explicitly
  empty, and non-empty manual cash-account selection;
- empty, partial, stale, refreshing, and failed states for individual page
  sections;
- stable IDs, pagination, freshness metadata, error shapes, and consistency
  rules that an API needs.

For a new backend-backed implementation, the recommended boundary is:

1. The backend owns ledger parsing, price conversion, classification, and all
   analytical calculations.
2. The client owns navigation, open panels, draft filters, selected chart
   points, expanded tree nodes, and responsive layout.
3. The backend returns resource data and page-ready analytical results, always
   tagged with a ledger revision, calculation policy version, and freshness.
4. Preferences can remain local for a single-device product, but should use a
   preferences API if users sign in or use multiple devices.

## User-Visible Feature Inventory

### Application Shell And Navigation

- Navigate among Net Worth, Investments, Cashflow, Accounts, Transactions, and
  Settings.
- Use a sidebar on wide screens and bottom navigation on narrower screens.
- See the same feature set recomposed for phone, tablet, and desktop widths.
- Open `/` or `/net-worth` as the default screen.
- See cached financial data while newer data is loading or when refresh fails.
- See install guidance when the browser supports PWA installation.
- See and apply an available service-worker update.
- Use the installed application shell offline with previously cached business
  data.

These shell and layout behaviors are documented more completely in
`docs/design-system.md` and `docs/qa-runbook.md` than in the user stories.

### Net Worth

The user can:

- see current Net Worth, Assets, and Liabilities summary values;
- see change from the previous period and a compact trend for each KPI;
- switch the analysis between monthly and yearly periods;
- select a period on the assets/liabilities/net-worth timeline;
- inspect Assets and Liabilities category breakdowns for the selected period;
- move backward and forward through breakdown periods;
- group account categories at depth 1 through 4;
- select a category and expand or collapse the aggregated `Other` category;
- compare Net Investment with Net Worth and see gain or loss between them;
- inspect Asset Allocation and Liability Activity calendars;
- use a month calendar or a 12-month year view;
- inspect acquired, disposed, reallocated, borrowed, and repaid activity;
- select a day, see its contributing transactions, and open transaction detail;
- see incomplete-history and missing-conversion notices without fabricated zero
  values.

Current calculation behavior that must be preserved is described under
"Calculation Contract" below.

### Investments

The user can:

- see portfolio Market Value, remaining Purchase Cost, and Gain or Loss;
- switch between monthly and yearly performance periods;
- select a chart period;
- scope the summary and chart to an investment account subtree;
- drill down one account segment at a time;
- use breadcrumbs to return to any ancestor or the whole portfolio;
- see whether a subtree has more children;
- distinguish complete results from partial results;
- see a return percentage only when cost basis is positive and complete.

The implementation considers asset accounts to be investment accounts except
for the `assets:cash`, `assets:property`, and `assets:receivables` branches.
That is product policy and should be configurable or explicitly versioned in a
replacement backend.

### Cashflow

The user can:

- see the total transaction count or the count matching applied filters;
- edit filters without changing the dashboard until Apply is selected;
- reset filters and remove individual applied-filter chips;
- switch between monthly and yearly periods;
- inspect Income, Expenses, and Net Flow on a timeline;
- select a timeline period and synchronize the breakdowns and calendars;
- inspect separate Income and Expenses category breakdowns;
- navigate periods, choose category depth 1 through 4, select categories, and
  expand or collapse `Other`;
- inspect separate Income and Expenses month calendars or 12-month year views;
- select a day, view contributing transactions, and open transaction detail;
- see a clear empty result when filters match nothing;
- see a conversion error instead of a misleading partially calculated chart.

The Cashflow screen deliberately does not contain a general transaction list.
The Transactions screen owns that browsing experience.

### Accounts And Account Register

The user can:

- browse a hierarchical account tree;
- expand and collapse parent accounts;
- see account type through labels, color, and iconography;
- see parent balances without double-counting child balances;
- see converted totals in the global display symbol;
- see partial-conversion warnings when some balances cannot be converted;
- open an exact account at `/accounts/:name`;
- return from the register to the account tree;
- see exact-account postings grouped by month;
- see Date, Description, Amount, and running Balance on wide layouts;
- see a compact equivalent on narrow layouts;
- see an empty state when the exact account has no convertible register entries.

An account-subtree filter and an exact-account register intentionally have
different semantics. The filter matches postings in descendants; the register
only includes postings owned by the exact account.

### Transactions And Transaction Detail

The user can:

- browse transactions newest first and grouped by month;
- see date, description, a concise flow classification, total amounts, and up
  to five posting previews per transaction;
- see how many additional postings were omitted from the preview;
- filter by inclusive start and end dates;
- filter by a non-negative minimum and/or maximum posting magnitude;
- optionally require the matching posting amount to use one commodity;
- include one or more account subtrees;
- exclude one or more account subtrees;
- search account choices while building include/exclude filters;
- edit a filter draft, validate it, Apply it, or close it without applying;
- see applied filters as individually removable chips;
- reset both the draft and active filter;
- clear filters from the no-matches state;
- select a transaction and return to the list;
- see detail beside the list at sufficiently wide content widths and in place
  of the list otherwise;
- inspect the transaction date, full description, all posting accounts, and all
  amount lines, including every commodity label.

The exact current filter semantics are:

- dates are inclusive;
- include accounts use OR and match the account or any descendant;
- a transaction fails if any posting matches any excluded subtree;
- amount and commodity constraints must match the same posting amount;
- amount comparisons use the absolute raw posting quantity, without conversion;
- the transaction passes if at least one posting amount satisfies all active
  amount/commodity constraints.

### Settings, Status, And Refresh

The user can:

- switch immediately between Light and Dark themes;
- select exactly one display symbol from available commodities;
- search available display symbols;
- use automatically inferred cash-like asset accounts;
- search asset accounts and maintain a manual set of cash-like prefixes;
- return to automatic inference;
- see whether the assigned hledger service is connected;
- see the hledger version and refresh only the version check;
- refresh all ledger data;
- see status and timestamps for account names, accounts/prices, transactions,
  commodities, and the version check;
- continue using stale cached data while refresh is running or after it fails.

Cash-account configuration has three meaningful states:

- `automatic`: infer prefixes from asset account segments such as `bank`,
  `cash`, `checking`, `current`, `deposit`, `savings`, and `wallet`;
- `manual non-empty`: only the selected prefixes and their descendants are
  cash-like;
- `manual empty`: deliberately classify no asset account as cash-like.

Obsolete manual prefixes are removed when the available account list changes.
If a formerly non-empty manual selection becomes entirely obsolete, behavior
falls back to automatic inference. An intentionally empty manual selection
remains empty.

## Does `docs/user-stories.md` Cover Everything?

No. It covers the product's main goals and financial correctness well, but the
following implemented behavior should be added to it or retained in a separate
acceptance specification.

| Area | Covered well | Missing or only implied |
| --- | --- | --- |
| Shell | Routes and PWA basics | Navigation composition, default route, install dismissal, responsive thresholds |
| Net Worth | Main charts, breakdowns, calendars, exclusions | KPI comparisons/trends, depth 1-4, `Other`, year window, detailed partial states |
| Investments | Cost-basis and valuation rules, drilldown | Investment branch exclusion policy and precise tree summary fields |
| Cashflow | Timeline, breakdowns, filters, calendars | Transaction count, depth 1-4, `Other`, year window, all-or-nothing chart conversion |
| Accounts | Tree, aggregation, conversion, exact register | Back/empty states, visible register columns, narrow composition |
| Transactions | Sorting, previews, filters, draft/apply, detail layout | Inclusive/raw-posting filter semantics, validation, search, visible detail fields |
| Settings | Symbol, theme, cash classification, status/refresh | Symbol/account search and the three cash-selection states |
| Data state | Cached stale data and refresh | A section-level response contract for fresh, stale, partial, unavailable, and failed data |

The stories should remain user-centered. API pagination, ETags, revision IDs,
error objects, authentication, and calculation versions belong in this
document or an OpenAPI definition rather than in user stories.

### Observed Story/Implementation Discrepancy

The Investments story says a return percentage is shown only when cost basis is
positive and complete. The current calculation returns a percentage for any
non-zero basis, uses the absolute basis as its denominator, and does not remove
the percentage when the result is partial. A faithful clone must choose which
contract is authoritative. The recommended API follows the safer documented
intent: return a percentage only for a positive, complete basis. Add a parity
test before changing the current UI so this difference is resolved explicitly.

## Current API And Why It Is Not The Recommended Final API

The current frontend calls the same-origin paths below. A reverse proxy removes
`/api` before forwarding them to hledger-web.

| Current request | Purpose |
| --- | --- |
| `GET /api/version` | hledger version/status |
| `GET /api/accountnames` | account names used by filters and Settings |
| `GET /api/accounts` | account hierarchy and balance periods |
| `GET /api/prices` | dated commodity prices |
| `GET /api/transactions` | every transaction and posting |
| `GET /api/commodities` | available display symbols |

The frontend combines `/accounts` and `/prices` into one cached account
snapshot. It then downloads all transactions and performs filters, account
registers, cashflow, net worth, investment performance, and calendars in Elm.

These endpoints are adequate for the current single-user PWA, but they are not
the best boundary for a general replication because:

- every client must reproduce complex financial calculation rules exactly;
- initial payloads grow with the entire ledger;
- there is no server pagination or targeted retrieval;
- there is no ledger revision tying multiple responses to one consistent view;
- freshness and partial-calculation information are client-inferred;
- transaction indices are usable locally but are not a robust public ID
  contract unless the backend guarantees their stability.

Keep these endpoints only as an upstream adapter or a compatibility API. Expose
a versioned application API to new clients.

## Recommended Backend Boundary

Use a modular backend with three logical layers:

1. **Ledger adapter** reads hledger or another ledger source and normalizes
   accounts, postings, quantities, costs, commodities, and dated prices.
2. **Domain services** implement filtering, classification, valuation, cost
   basis, cashflow, net worth, activity, and completeness policies.
3. **Application API** returns paginated resources and page-ready analytics.

The backend should use decimal quantities, not binary floating point, for
stored and transported money. A decimal value should be serialized as a string,
for example `{ "commodity": "INR", "quantity": "1234.50" }`.

The frontend should continue to own ephemeral interaction state:

- open or closed drawers and dropdowns;
- unapplied filter drafts and validation presentation;
- selected chart period, category, calendar day, and transaction;
- expanded account tree nodes and investment breadcrumbs;
- responsive layout and PWA shell lifecycle.

## Recommended API List

All endpoints below use an `/api/v1` prefix. Requests are same-origin unless a
separate authenticated API is deliberately introduced.

### Bootstrap, Status, Preferences, And Refresh

| Method and path | Purpose |
| --- | --- |
| `GET /api/v1/bootstrap` | One lightweight startup response with service metadata, commodities, account filter options, preferences, freshness, and current ledger revision |
| `GET /api/v1/status` | Service health, backend/hledger versions, latest ledger revision, and per-source freshness |
| `GET /api/v1/preferences` | Fetch synchronized user preferences when accounts/multi-device support exists |
| `PUT /api/v1/preferences` | Replace display symbol, theme, and cash-account selection atomically |
| `POST /api/v1/refreshes` | Start an upstream ledger refresh/re-index operation |
| `GET /api/v1/refreshes/{refreshId}` | Poll refresh progress and per-source failures |

If the backend reads an already-current local ledger on every request, a
refresh job is unnecessary. Keep the endpoint only when it corresponds to a
real upstream synchronization or index rebuild.

### Accounts

| Method and path | Purpose |
| --- | --- |
| `GET /api/v1/accounts/tree` | Account hierarchy, direct and subtree balances, converted display value, type, child count, and conversion completeness |
| `GET /api/v1/accounts/{accountPath}` | Metadata and current balances for one exact account |
| `GET /api/v1/accounts/{accountPath}/register` | Paginated exact-account register with posting amount and running balance |
| `GET /api/v1/account-options` | Lightweight searchable account names for filters and cash-account Settings |

`accountPath` must be one URL-encoded path parameter. Colons are data inside
the account name, not URL path separators.

### Transactions

| Method and path | Purpose |
| --- | --- |
| `GET /api/v1/transactions` | Cursor-paginated transaction summaries with server-side filters |
| `GET /api/v1/transactions/{transactionId}` | Full transaction, posting amounts, costs, statuses, and account names |
| `POST /api/v1/transaction-queries` | Optional alternative for complex or very large filter sets that should not be encoded in a URL |

The POST query endpoint is read-only and must not create transactions. Start
with the GET endpoint; add the POST form only if URL length becomes a real
constraint.

### Analytics

| Method and path | Purpose |
| --- | --- |
| `GET /api/v1/analytics/net-worth` | Current summary, period KPIs, historical timeline, category breakdowns, and performance series |
| `GET /api/v1/analytics/investments` | Portfolio/subtree summary, market-value/cost timeline, completeness, child accounts, and breadcrumbs |
| `GET /api/v1/analytics/cashflow` | Filtered count, income/expense/net timeline, and selected-period category breakdowns |
| `GET /api/v1/analytics/activity` | Month or year activity data for income, expense, asset allocation, or liabilities, including contributing transaction IDs |

This split keeps page startup efficient while allowing calendar navigation and
transaction detail to be fetched independently. A backend may internally use
one read model or database query per page; the public contract need not expose
its storage design.

## Shared Query Parameters

Use the same names and semantics across endpoints.

| Parameter | Values / meaning |
| --- | --- |
| `displayCommodity` | Required for converted resources and analytics; must be an available commodity |
| `granularity` | `month` or `year` |
| `period` | `YYYY-MM` for monthly analysis or `YYYY` for yearly analysis |
| `startDate`, `endDate` | Inclusive ISO dates |
| `minAmount`, `maxAmount` | Non-negative raw posting magnitude bounds |
| `commodity` | Requires the matching raw posting amount to use this commodity |
| `includeAccount` | Repeatable account-subtree inclusion; OR semantics |
| `excludeAccount` | Repeatable account-subtree exclusion; matching any excludes the transaction |
| `depth` | Integer 1 through 4 for category grouping |
| `cursor`, `limit` | Stable cursor pagination; server-capped page size |
| `ledgerRevision` | Optional consistency guard; reject or redirect when the requested revision is unavailable |

The same filter object must be reusable by Transactions and Cashflow. Cashflow
must apply it before calculating the count, timeline, breakdowns, and calendars.

## Shared Response Envelope

Every successful response should include enough metadata to explain the data
the user sees:

```json
{
  "data": {},
  "meta": {
    "ledgerRevision": "sha256:...",
    "calculationPolicy": "eledger-analytics-v1",
    "generatedAt": "2026-07-17T10:20:30Z",
    "sourceAsOf": "2026-07-17T10:20:29Z",
    "freshness": "fresh",
    "completeness": "complete",
    "warnings": []
  }
}
```

Recommended enumerations are:

- `freshness`: `fresh`, `stale`, or `refreshing`;
- `completeness`: `complete`, `partial`, or `unavailable`;
- warnings with stable `code`, human-readable `message`, affected period or
  account, source commodity, and omitted item count when known.

Do not turn an unknown converted amount into zero. Return the known result only
when the product calculation permits a partial result, mark it `partial`, and
identify omissions. Return the affected analytical section as `unavailable`
when the calculation is all-or-nothing, as Cashflow currently is.

## Core Resource Shapes

### Money, Amount, And Cost

```json
{
  "commodity": "USD",
  "quantity": "12.345",
  "cost": {
    "type": "unit",
    "commodity": "INR",
    "quantity": "83.20"
  }
}
```

`cost.type` is `unit` for hledger `@` and `total` for `@@`. Cost is nullable.
Preserve original decimal precision and every commodity in a posting.

### Transaction Summary

```json
{
  "id": "txn_01J...",
  "sourceIndex": 417,
  "date": "2026-07-15",
  "description": "Example",
  "status": "cleared",
  "classification": "expense",
  "flowLabel": "Assets to Expenses",
  "totalAmounts": [
    { "commodity": "INR", "quantity": "1250.00" }
  ],
  "postingPreviews": [],
  "postingCount": 7,
  "omittedPostingCount": 2
}
```

Use a stable public `id`. `sourceIndex` may be returned for traceability, but
clients should not depend on it unless its stability is guaranteed across
revisions.

### Transaction Detail

Transaction detail adds the complete posting list. Each posting contains:

- exact account path;
- posting status;
- every amount and its optional unit or total cost;
- original commodity and decimal quantity.

### Account Tree Node

```json
{
  "path": "assets:bank:checking",
  "name": "checking",
  "rootType": "assets",
  "hasChildren": false,
  "postingCount": 28,
  "directBalances": [],
  "subtreeBalances": [],
  "displayValue": { "commodity": "INR", "quantity": "25000.00" },
  "completeness": "complete",
  "children": []
}
```

Return both direct and subtree balances so the client never has to guess which
value was aggregated. The displayed parent value must not add an already
inclusive balance to its children.

### Register Entry

```json
{
  "transactionId": "txn_01J...",
  "date": "2026-07-15",
  "description": "Example",
  "amount": { "commodity": "INR", "quantity": "-1250.00" },
  "runningBalance": { "commodity": "INR", "quantity": "23750.00" }
}
```

The register uses exact posting ownership. Define whether the cursor traverses
ascending or descending entries, and ensure running balances are correct for
that order. The current UI displays month groups after calculating balances in
ascending date order.

## Analytics Response Requirements

### Net Worth

`GET /api/v1/analytics/net-worth` should accept:

- `displayCommodity`;
- `granularity`;
- optional `assetPeriod` and `liabilityPeriod`;
- `assetDepth` and `liabilityDepth`.

It should return:

- current assets, liabilities, and net worth;
- per-KPI previous-period absolute and percentage changes;
- recent KPI trend values;
- all historical timeline points;
- asset and liability categories for the selected period;
- Net Investment versus Net Worth performance points;
- gain/loss and incomplete-period count.

Calendar data may be embedded for the initially selected month, but subsequent
calendar navigation should use `/analytics/activity`.

### Investments

`GET /api/v1/analytics/investments` should accept:

- `displayCommodity`;
- `granularity`;
- optional `accountPrefix`;
- optional selected `period`.

It should return:

- scoped market value, remaining cost basis, gain/loss, optional return
  percentage, and completeness;
- period points with the same fields;
- current prefix, breadcrumbs, and immediate child prefixes;
- each child's current summary and whether it has children;
- structured warnings for missing posting costs, prices, or conversions.

### Cashflow

`GET /api/v1/analytics/cashflow` should accept all shared transaction filters,
plus:

- `displayCommodity`;
- `granularity`;
- optional `incomePeriod` and `expensePeriod`;
- `incomeDepth` and `expenseDepth`.

It should return:

- total and filtered transaction counts;
- Income, Expense, and Net Flow timeline points;
- Income and Expense categories for the selected period;
- completeness and conversion warnings.

The response need not include the general filtered transaction list.

### Activity Calendars

`GET /api/v1/analytics/activity` should accept:

- `kind`: `income`, `expense`, `asset`, or `liability`;
- `range`: `month` or `year`;
- `period`: `YYYY-MM` for a month or for the first month of a 12-month window;
- `displayCommodity`;
- `depth`;
- the shared filters for Cashflow calendars;
- resolved cash account prefixes, or a reference to server-side preferences,
  for asset activity.

It should return each day or month with:

- total and category proportions;
- increase, decrease, and reallocated totals where applicable;
- contributing transaction IDs;
- missing-conversion details.

The current year view is a 12-month fiscal window beginning in April. Make
`fiscalYearStartMonth` deployment or user configuration instead of hard-coding
it, while defaulting to `4` for behavioral parity.

## Calculation Contract

A replacement is not behaviorally equivalent unless it preserves these rules.

### Prices And Conversion

- Current account summaries use the current account snapshot and its price
  graph.
- Historical investment market value uses the latest applicable price on or
  before the end of each period.
- Dated multi-step conversion must not use a future price.
- Missing conversions are unknown, not zero.
- Every analytical response identifies its display commodity.

### Cashflow

- Transfers and opening balances do not count as income or expense.
- Refunds reduce their corresponding flow.
- A transaction's commodity magnitudes are based on half the sum of absolute
  posting amounts, avoiding double-counting balanced postings.
- The current Cashflow chart becomes unavailable if a participating filtered
  transaction cannot be fully converted.

### Net Worth

- Current summary values come from current account snapshot balances.
- Historical balances are cumulative transaction-derived asset and liability
  movements.
- Liabilities are displayed as a positive magnitude and subtracted from assets.
- Category breakdowns include activity through the selected period.
- Performance compares transaction-derived Net Investment with dated market
  value.

### Investments

- Acquisition basis comes from explicit unit (`@`) or total (`@@`) posting
  cost, not current market price.
- Internal investment transfers preserve cost basis.
- Explicitly priced disposals reduce remaining basis.
- Missing cost on a basis-affecting posting makes the result partial.
- Market value uses holdings and prices available at the period end.
- Gain/loss equals market value minus remaining cost basis.
- Return percentage is present only for positive, complete basis.
- Do not label the result as time-weighted, money-weighted, or annualized
  return.

### Activity Classification

- Asset activity distinguishes acquisition, disposal, and non-cash
  reallocation.
- Cash-only transfers, income, expenses, and opening balances do not count as
  asset allocation.
- A transaction with an unconvertible participating asset is reported and
  omitted rather than partially counted.
- Liability activity distinguishes borrowing and repayment.
- Category labels are account paths truncated to the requested depth.
- Chart categories below 1% and categories beyond the visible top set are
  grouped into `Other`; the current UI shows at most ten slices including
  `Other`.

## Preferences Contract

If preferences are server-side, use an atomic shape such as:

```json
{
  "displayCommodity": "INR",
  "theme": "dark",
  "cashAccounts": {
    "mode": "manual",
    "prefixes": ["assets:bank", "assets:cash"]
  }
}
```

An empty `prefixes` array in manual mode must remain different from automatic
mode. When commodities or accounts change, return reconciliation warnings and
the effective value. Do not silently turn a user's explicitly empty manual
selection into automatic mode.

For an anonymous, single-device deployment, these preferences may remain in
local storage. In that case `/bootstrap` should omit user preferences and still
return commodities and account options needed to validate the local choice.

## Errors, Validation, And HTTP Behavior

Use a stable error envelope:

```json
{
  "error": {
    "code": "INVALID_FILTER",
    "message": "The transaction filter is invalid.",
    "fields": {
      "startDate": "Must be on or before endDate."
    },
    "requestId": "req_01J..."
  }
}
```

Recommended status behavior:

- `400` for malformed query syntax;
- `404` for an unknown account, transaction, revision, or refresh job;
- `409` when a requested ledger revision is no longer available or a refresh
  conflicts with one already running;
- `422` for a structurally valid but semantically invalid filter, commodity,
  period, depth, or preference;
- `503` when no usable source data is available;
- `200` with stale metadata when stale data is usable;
- `200` with partial metadata when a partial calculation is intentionally
  supported.

Validate that start is not after end, minimum is not above maximum, bounds are
non-negative, depth is 1 through 4, and requested commodities and accounts
exist in the selected ledger revision.

## Consistency, Caching, And Offline Support

- Assign a new immutable `ledgerRevision` after each successful source refresh.
- Calculate every response against one revision.
- Include the revision in cache keys and ETags.
- Honor `If-None-Match` with `304 Not Modified` for read endpoints.
- Do not mix account balances from one revision with transactions or prices
  from another.
- Retain the most recent successful response while refresh is in progress.
- Make warnings and completeness part of the cached response, not ephemeral
  headers.
- Keep service-worker shell caching separate from business-data caching.
- Never service API requests from the PWA app-shell cache.

For large ledgers, cache analytical responses by revision, display commodity,
granularity, selected period, depths, cash-account policy, and normalized
transaction filter.

## Authentication And Deployment

The current model assigns one same-origin backend and one ledger to each
frontend deployment. Replication can preserve that simple model without a
backend selector.

If multiple users or ledgers share one backend:

- authenticate every request;
- derive ledger access from authorization, not a freely trusted query string;
- include a server-validated ledger identifier in cache and audit keys;
- ensure transaction and account IDs cannot be used across tenants;
- treat financial data and descriptions as sensitive and avoid logging response
  bodies or raw filter values unnecessarily.

## Suggested Delivery Order

1. Normalize the hledger wire data and implement revisioned snapshots.
2. Add `/bootstrap`, `/status`, account tree, transaction list/detail, and exact
   account register.
3. Port Cashflow and Net Worth calculations with fixture-based parity tests.
4. Port investment cost-basis and dated valuation with incomplete-data tests.
5. Add activity calendars and cash-account preferences.
6. Add refresh jobs, ETags, offline response storage, and operational metrics.
7. Publish an OpenAPI 3.1 document and generate request/response contract tests.

## Acceptance Criteria For A Faithful Replica

- Every implemented route and detail state in the feature inventory is
  reachable on phone and desktop layouts.
- The replacement returns the same results as the current Elm calculations for
  representative single- and multi-commodity fixtures.
- Exact-account registers and subtree transaction filters remain distinct.
- Missing prices and costs never become zero silently.
- Cashflow filters affect its count, timeline, breakdowns, and calendars
  together, while Transactions keeps independent filter state.
- Net Worth and Cashflow calendar days link to the correct transaction IDs.
- Investment drilldown, breadcrumbs, period selection, and completeness survive
  data refresh when still valid.
- Cached stale data remains visible during refresh and after failure.
- Responses from different ledger revisions are never combined.
- The API is read-only with respect to ledger transactions.
