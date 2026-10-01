# Features

Hedger is a read-only personal finance viewer for a single
[hledger](https://hledger.org/) journal. It answers questions about net worth,
cashflow, investments, accounts, and transactions without ever modifying the
ledger.

The stories below describe the intended experience. They are deliberately at
the level of what the user wants, not how it is implemented.

## Net worth

- As a ledger owner, I want to see my current net worth, assets, and
  liabilities at a glance, so that I can judge my financial position.
- As a ledger owner, I want to see how net worth, assets, and liabilities
  changed over time, so that I can spot trends.
- As a ledger owner, I want to compare the latest period with the previous one,
  so that I can tell whether I am improving.
- As a ledger owner, I want net worth broken down by account and category, so
  that I can see what drives it.
- As a ledger owner, I want to pick a day and see the transactions behind it, so
  that I can understand a movement.
- As a ledger owner, I want to know when values could not be converted to my
  display commodity, so that I do not trust an incomplete number.

## Cashflow

- As a ledger owner, I want to see income and expenses over time, so that I
  understand where my money goes.
- As a ledger owner, I want income and expenses broken down by category, so that
  I can see spending patterns.
- As a ledger owner, I want to see daily activity on calendars, so that I can
  spot unusual days.
- As a ledger owner, I want to filter the view with an hledger query, so that I
  can focus on a subset.
- As a ledger owner, I want to drill from a period or day into its transactions,
  so that I can verify the totals.
- As a ledger owner, I want reversals and refunds shown separately from posted
  amounts, so that net figures are honest.

## Investments

- As an investor, I want to see portfolio value, cost, gain/loss, total profit,
  and return, so that I can judge performance.
- As an investor, I want value and cost over time, so that I can see how the
  portfolio evolved.
- As an investor, I want my allocation compared with targets, so that I can
  rebalance.
- As an investor, I want to drill into an account and its children, so that I
  can find what is driving a result.
- As an investor, I want performance split into attributed income, realised
  gain, and unrealised gain, so that I understand where it came from.
- As an investor, I want warnings for missing prices, costs, or performance
  mappings, so that I know why a number is partial.

## Accounts

- As a ledger owner, I want a hierarchy of accounts with balances, so that I can
  browse my chart of accounts.
- As a ledger owner, I want direct and subtree balances, so that I can tell an
  account apart from its descendants.
- As a ledger owner, I want balances valued in a chosen commodity using dated
  prices, so that figures are comparable.
- As a ledger owner, I want to jump from an account to its register, so that I
  can review its postings.

## Account register

- As a ledger owner, I want the postings for an account and its descendants over
  a date range, so that I can audit it.
- As a ledger owner, I want the register grouped by month with a running
  balance, so that I can follow it over time.
- As a ledger owner, I want to load older entries, so that I can page through
  history.

## Transactions

- As a ledger owner, I want to browse all transactions, so that I can look up
  any event.
- As a ledger owner, I want to filter with hledger query syntax, so that I can
  find exactly what I need.
- As a ledger owner, I want to see every posting, commodity, cost, and tag, so
  that nothing is hidden.
- As a ledger owner, I want each transaction labelled with its effects, so that
  I can understand what happened — income, expense, transfer, borrowing, an
  asset acquisition, and so on.
- As a ledger owner, I want to page through results, so that large journals stay
  responsive.

## Settings

- As a ledger owner, I want to choose a light or dark theme, so that the
  dashboard is comfortable to read.

## System and operations

- As an operator, I want the dashboard to refresh automatically when the journal
  changes on disk, so that I always see current data.
- As an operator, I want the app to keep serving the last good journal if a
  reload fails, so that a broken edit does not take the site down.
- As an operator, I want health endpoints, so that I can monitor the service.
- As an operator, I want to install the app to my home screen and load its
  shell offline, so that it feels like a native app.

## Data freshness

Every screen distinguishes the same states:

| State | Meaning |
|-------|---------|
| Fresh | Data is from the current journal revision. |
| Refreshing | Data is from a previous revision; a refresh is in progress. |
| Stale | Data is from a previous revision; no refresh is in progress. |
| Partial | Some amounts could not be converted to the display commodity. |
| Unavailable | No data could be converted; charts are hidden. |
| Empty | A successful fetch returned no data. |
| Failed | The request failed; an error is shown. |

The frontend keeps stale content visible during a refresh instead of replacing
it with a loading spinner.

## Roadmap

Hedger needs no feature beyond these at present.

| Feature | Status | Notes |
|---------|--------|-------|
| Authentication | Planned | Access control is currently delegated to the reverse proxy and deployment. |
| Automatic journal reload via file watcher | Done | `fsnotify` watches the root journal and its includes, debounced; SIGHUP also reloads. |
| Live updates over Server-Sent Events | Done | `/api/v1/events` streams revision changes and connected clients refresh automatically. |
| Response caching for large journals | Planned | Tracked in issue [#17](https://github.com/nixypanda/hedger/issues/17); needed to keep very large journals interactive. |

## Out of scope

Hedger is a viewer, not a ledger editor. It intentionally does not edit or import
transactions, serve multiple ledgers or tenants from one deployment, provide
budgets, forecasting, or recurring-transaction screens, offer a generic report
builder or global search, or sync preferences to a server.
