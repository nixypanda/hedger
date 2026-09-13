# Features

## Summary

Hedger is a read-only personal finance viewer that displays data from an
hledger journal. It provides six screens: Net Worth, Cashflow, Investments,
Accounts, Transactions, and Settings.

```text
hledger journal -> parse into snapshot -> query/calculate -> display per screen
```

The backend owns all calculations (valuation, cost basis, classification,
aggregation). The frontend owns rendering and interaction state.

Current implementation covers all six screens with full backend support. The
product goal is a faithful read-only replica of eLedger, not a ledger editor
or analytics platform.

## Implemented Features

### Net Worth

- [x] Net Worth, Assets, and Liabilities KPI cards with absolute and
      percentage changes.
- [x] Sparkline trends on KPI cards.
- [x] Period-by-period timeline chart (Net Investment vs Net Worth).
- [x] Monthly and yearly granularity.
- [x] Assets breakdown with category depth 1-4 and `Other` grouping.
- [x] Liabilities breakdown with the same depth controls.
- [x] Asset allocation activity calendar (month and year views).
- [x] Liability activity calendar.
- [x] Day selection with transaction detail drill-down from complete DTOs.
- [x] Incomplete period warnings.
- [x] Skeleton loading, stale/refreshing, empty, and failed states.

### Cashflow

- [x] Free-text hledger query with draft/apply/clear behavior.
- [x] Query help text and examples.
- [x] Parser error display next to the input field.
- [x] Period-by-period income and expense timeline.
- [x] Monthly and yearly granularity.
- [x] Separate Income category breakdown with depth controls.
- [x] Separate Expense category breakdown with depth controls.
- [x] Income activity calendar (month and year views).
- [x] Expense activity calendar.
- [x] Calendar day to transaction detail drill-down.
- [x] Filtered count display.
- [x] Conversion-unavailable and empty states.

### Investments

- [x] Portfolio KPI cards: market value, purchase cost, gain/loss, total
      profit.
- [x] Performance chart (market value and purchase cost over time).
- [x] Monthly and yearly granularity.
- [x] Period selection with synchronized detail view.
- [x] Account-prefix drilldown with breadcrumb navigation.
- [x] Child holdings list with `hasChildren` affordance.
- [x] Performance breakdown: attributed income, realised gain, unrealised gain.
- [x] Completeness badges (complete/partial/unavailable).
- [x] Structured warnings for missing prices, costs, and performance mappings.
- [x] Skeleton loading, empty, and failed states.

### Accounts

- [x] Hierarchical account tree with expand/collapse.
- [x] Account type labels and icons where inferred.
- [x] Direct (exclusive) balances per account.
- [x] Subtree (inclusive) balances per account.
- [x] Display-commodity valuation with dated market prices.
- [x] Valuation completeness status (complete/partial/unavailable).
- [x] Optional commodity parameter for valuation projection.
- [x] Navigation to account register.
- [x] Wide list/detail and narrow layouts.
- [x] Loading, stale/refreshing, empty, and failed states.

### Account Register

- [x] Exact-account register with postings for the account and descendants.
- [x] Date range filtering (from/to).
- [x] Cursor-based pagination with Load More.
- [x] Month-grouped register entries.
- [x] Running balance per entry.
- [x] Transaction detail expansion with full posting details.
- [x] Display-commodity valuation per entry.
- [x] Wide list/detail and narrow layouts.
- [x] Loading, stale/refreshing, empty, and failed states.

### Transactions

- [x] Free-text hledger query with draft/apply/clear behavior.
- [x] Query help text and examples.
- [x] Parser error display next to the input field.
- [x] Cursor-based pagination with bounded page window (5 pages).
- [x] Month-grouped transaction rows with posting previews.
- [x] Full transaction detail with every posting, commodity, and cost.
- [x] Transaction classification labels (income, expense, transfer,
      opening_balance, borrowing, repayment, asset_acquisition,
      asset_disposal, asset_reallocation, unclassified).
- [x] Wide list/detail and narrow replace-list-with-detail layouts.
- [x] No-matches recovery with Clear action.
- [x] Loading, success, empty, stale, refreshing, and failed states.

### Settings

- [x] Light/dark theme toggle with immediate visual change.
- [x] Theme persistence in browser localStorage.

### System

- [x] Health endpoints (`/health/live`, `/health/ready`).
- [x] Bootstrap endpoint returning API version, app name, fiscal year start
      month, and response metadata.
- [x] Response metadata on every response: journal revision, freshness,
      warnings.
- [x] Journal reload via SIGHUP with atomic snapshot replacement.
- [x] Last successful snapshot retained on reload failure.

### PWA

- [x] Service worker with app-shell caching of static assets.
- [x] SPA fallback for navigation routes.
- [x] `/api/*` bypass (never cached by service worker).
- [x] Standalone display mode with Hedger identity.
- [x] Responsive phone and desktop layouts.
- [x] Bottom navigation bar on phone, sidebar on desktop.

## Data Freshness States

Every screen handles these states:

| State | Meaning |
|-------|---------|
| Fresh | Data is from the current journal revision. |
| Refreshing | Data is from a previous revision; a refresh is in progress. |
| Stale | Data is from a previous revision; no refresh in progress. |
| Partial | Some categories could not be converted to the display commodity. |
| Unavailable | No data could be converted; charts are hidden. |
| Failed | Request failed; error message shown. |
| Empty | Successful fetch returned no data. |

The frontend preserves stale content during refresh instead of replacing it
with a loading spinner.

## Query System

Transactions and Cashflow share the same hledger query parser:

- Free-text input with hledger query-language syntax.
- Account, description, payee, note, code, date, status, commodity, amount,
  tag, negation, and boolean expressions.
- `expr:`, `any:`, and `all:` semantics.
- Query text capped at 2048 characters.
- Unsupported options (`inacct:`, `inacctonly:`, `interval:`) are rejected
  with stable validation errors.

## Investment Performance Model

Investment calculations support:

- Market value from dated period-end prices.
- Purchase cost with transfer-preserved basis.
- Unit and total cost handling.
- Gain/loss decomposition: attributed income, realised gain, unrealised gain.
- Return percentage only for positive, complete cost basis.
- Account-prefix drilldown with breadcrumb navigation.
- `investments.assetClasses` is the portfolio allowlist; configured account
  prefixes and their descendants participate without requiring gain mappings.
- Asset accounts omitted from `investments.assetClasses` are ignored.
- Structured warnings for missing prices, missing costs, missing performance
  mappings, and foreign costs.

## Deferred Features (Release 1 Scope)

These are explicitly out of scope for Release 1:

- [ ] Ledger editing, transaction creation, or imports.
- [ ] Multi-ledger selection or multi-tenancy.
- [ ] Global search.
- [ ] Recurring/forecast screens.
- [ ] Generic report builders.
- [ ] Dashboard/home projections beyond Net Worth as default.
- [ ] Authentication (deployment concern at reverse proxy).
- [ ] Server-side preferences or multi-device sync.
- [ ] File watcher (fsnotify) for automatic reload.
- [ ] ETags or `If-None-Match` for simple reads.
- [ ] Server-Sent Events for push updates.
- [ ] API response caching in the PWA.
- [ ] Display-commodity selection in Settings.
- [ ] Cash-account configuration modes in Settings.
- [ ] Journal diagnostics and manual refresh in Settings.
- [ ] Backend status display in Settings.
- [ ] SQLite or any database.
- [ ] Effect system or capability typeclasses.
