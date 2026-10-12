# Hedger

Hedger is a responsive, read-only personal finance dashboard for
[hledger](https://hledger.org/) journals. Point it at a journal and it displays
net worth, cashflow, investment performance, accounts, and transactions as a
fast, installable web app. It never writes to the ledger.

Built with Haskell ([Servant](https://hackage.haskell.org/package/servant) +
Warp + hledger-lib), an [Elm](https://elm-lang.org/) frontend, and Nix.

## Feature showcase

### Net worth

Net worth, asset, and liability KPIs with sparkline trends; monthly and yearly
performance charts; account composition; activity calendars; and transaction
drill-down.

![Desktop Net Worth dashboard](docs/images/readme/net-worth-desktop.png)

### Investments

Portfolio value, XIRR, total profit, target allocation, account drill-down,
value and return history, and realised/unrealised gain analysis.

![Tablet Investments dashboard](docs/images/readme/investments-tablet.png)

### Cashflow

hledger query filtering, income/expense timelines, category composition, and
monthly or fiscal-year activity.

![Desktop Cashflow dashboard](docs/images/readme/cashflow-desktop.png)

![Fiscal-year income and expense composition with monthly activity](docs/images/readme/cashflow-year-analysis-desktop.png)

### Accounts and transactions

Expandable account trees, direct and subtree balances, account registers,
month-grouped transactions, complete postings, query filtering, and cursor
pagination.

![Mobile Transactions view](docs/images/readme/transactions-mobile.png)

Hedger is an installable PWA with desktop, tablet, and mobile layouts,
light/dark themes, automatic journal reloads, and live frontend refresh.

## Documentation

- [Features](docs/features.md) — what Hedger does, as user stories, plus the roadmap.
- [Architecture](docs/architecture.md) — how the system fits together.
- [Backend](docs/backend.md) — Haskell service, configuration, and HTTP API.
- [Frontend](docs/frontend.md) — Elm app structure, pages, and design system.
- [Performance](docs/performance.md) — benchmarking methodology, results, and lessons.
- [Deployment](docs/deployment.md) — the NixOS module and production topology.
- [Local development](docs/local-development.md) — dev shell, running, and checks.

The [configuration example](hedger.example.yaml) shows every supported option.

Agent and contributor instructions live in [AGENTS.md](AGENTS.md).

## License

Hedger is free software, licensed under the
[GNU Affero General Public License v3.0 or later](LICENSE)
(AGPL-3.0-or-later).
