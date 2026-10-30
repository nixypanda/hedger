# Hedger

Hedger is a responsive, read-only personal finance dashboard for
[hledger](https://hledger.org/) journals. Point it at a journal and it displays
net worth, cashflow, investment performance, accounts, and transactions as a
fast, installable web app. It never writes to the ledger.

Built with Haskell ([Servant](https://hackage.haskell.org/package/servant) +
Warp + hledger-lib), an [Elm](https://elm-lang.org/) frontend, and Nix.

## Feature showcase

The screenshots use the bundled synthetic journal and switch between light and
dark with the page theme.

### Net worth

Net worth, asset, and liability KPIs with sparkline trends; monthly and yearly
performance charts; account composition; activity calendars; and transaction
drill-down.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/net-worth-desktop.png">
  <img alt="Net Worth dashboard with KPI cards, performance chart, and balance over time." src="docs/images/readme/light/net-worth-desktop.png" width="1600">
</picture>

### Investments

Portfolio value, XIRR, total profit, target allocation, account drill-down,
value and return history, and realised/unrealised gain analysis.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/investments-desktop.png">
  <img alt="Investments dashboard with portfolio value, allocation targets, and portfolio history." src="docs/images/readme/light/investments-desktop.png" width="1600">
</picture>

### Cashflow

hledger query filtering, income/expense timelines, category composition, and
monthly or fiscal-year activity.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/cashflow-desktop.png">
  <img alt="Cashflow dashboard with an hledger query filter and income and expense chart." src="docs/images/readme/light/cashflow-desktop.png" width="1600">
</picture>

### Monthly and yearly analysis

The analysis section pairs composition donuts with activity calendars at two
scales: a single month and the configured fiscal year. Account depth controls
let you drill into the hierarchy.

<table>
  <thead>
    <tr>
      <th align="left">Period</th>
      <th align="center">Net worth</th>
      <th align="center">Cashflow</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th align="left">Monthly<br><sub>December 2025</sub></th>
      <td align="center">
        <a href="docs/images/readme/light/net-worth-monthly-analysis-desktop.png" title="Open full resolution">
          <picture>
            <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/net-worth-monthly-analysis-desktop.png">
            <img alt="Net worth composition and daily asset and liability activity for December 2025." src="docs/images/readme/light/net-worth-monthly-analysis-desktop.png" width="800">
          </picture>
        </a>
      </td>
      <td align="center">
        <a href="docs/images/readme/light/cashflow-monthly-analysis-desktop.png" title="Open full resolution">
          <picture>
            <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/cashflow-monthly-analysis-desktop.png">
            <img alt="Cashflow composition and daily income and expense activity for December 2025." src="docs/images/readme/light/cashflow-monthly-analysis-desktop.png" width="800">
          </picture>
        </a>
      </td>
    </tr>
    <tr>
      <th align="left">Fiscal year<br><sub>FY 2025–26</sub></th>
      <td align="center">
        <a href="docs/images/readme/light/net-worth-yearly-analysis-desktop.png" title="Open full resolution">
          <picture>
            <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/net-worth-yearly-analysis-desktop.png">
            <img alt="Net worth composition and fiscal-year asset and liability activity for FY 2025–26." src="docs/images/readme/light/net-worth-yearly-analysis-desktop.png" width="800">
          </picture>
        </a>
      </td>
      <td align="center">
        <a href="docs/images/readme/light/cashflow-yearly-analysis-desktop.png" title="Open full resolution">
          <picture>
            <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/cashflow-yearly-analysis-desktop.png">
            <img alt="Cashflow composition and fiscal-year income and expense activity for FY 2025–26." src="docs/images/readme/light/cashflow-yearly-analysis-desktop.png" width="800">
          </picture>
        </a>
      </td>
    </tr>
  </tbody>
</table>

### Accounts and transactions

Expandable account trees, direct and subtree balances, account registers,
month-grouped transactions, complete postings, query filtering, and cursor
pagination.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/accounts-desktop.png">
  <img alt="Accounts page with an expandable account tree and subtree balances." src="docs/images/readme/light/accounts-desktop.png" width="1600">
</picture>

Account registers stay usable on tablet:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/account-register-tablet.png">
  <img alt="Account register on tablet with month-grouped transactions and running balances." src="docs/images/readme/light/account-register-tablet.png" width="820">
</picture>

### Responsive layouts

The same app switches to bottom navigation and single-column cards on phones.

<p align="center">
  <a href="docs/images/readme/light/phone-views.png" title="Open full resolution">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="docs/images/readme/dark/phone-views.png">
      <img alt="Phone layouts for Net Worth, Investments, Cashflow, and Transactions." src="docs/images/readme/light/phone-views.png" width="1608">
    </picture>
  </a>
</p>

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
