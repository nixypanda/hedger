# Frontend

## Overview

The frontend is an Elm 0.19.1 Progressive Web App in `frontend-web/`. It uses
`mdgriffith/elm-ui` for layout and `gampleman/elm-visualization` for charts.

```text
frontend-web/
  src/
    Main.elm                  # app composition root
    Route.elm                 # URL routing
    Page.elm                  # page composition
    RemoteData.elm            # stale/refreshing/partial data states
    Preferences.elm           # browser-local theme persistence
    Pwa.elm                   # PWA lifecycle
    Port.elm                  # Elm ports
    Page/
      NetWorth.elm            # default dashboard
      Cashflow.elm            # income/expense analytics
      Investments.elm         # investment performance
      Account.elm             # account tree
      AccountRegister.elm     # exact-account register
      Transactions.elm        # transaction list with hledger query
      Settings.elm            # theme toggle
      Shared/
        Analysis.elm          # shared analytics components
        QueryPanel.elm        # shared query input panel
    Api/
      Client.elm              # request construction, error handling
      Bootstrap.elm           # bootstrap endpoint
      Analytics.elm           # net-worth/cashflow/activity endpoints
      Investments.elm         # investment endpoint
      Transaction.elm         # transaction DTOs and API
      Account.elm             # account tree API
      Register.elm            # account register API
      Metadata.elm            # response metadata decoding
      Revision.elm            # journal revision tracking
      Error.elm               # API error types
    Data/
      Decimal.elm             # decimal quantity representation
      Date.elm                # date helpers
      Calendar.elm            # calendar rendering
      ActivityCalendar.elm    # activity calendar data
      AccountType.elm         # account type inference
      Quantity.elm            # quantity formatting
      Status.elm              # data status types
      Time.elm                # time helpers
    Ui/
      Theme.elm               # light/dark theme tokens
      Typography.elm          # font sizes and styles
      Icon.elm                # icon set
      Skeleton.elm            # skeleton loading states
      Responsive.elm          # device-aware layout helpers
      Css.elm                 # CSS-in-Elm helpers
      DesignTokens.elm        # color and spacing tokens
     .elm                     # shared UI primitives
    View/
      Shell.elm               # app shell, navigation, bottom bar
      Analytics.elm           # chart rendering (timeline, breakdowns)
      Transaction.elm         # transaction rendering
      Formatting.elm          # number and date formatting
      AccountPresentation.elm # account display helpers
      States.elm              # loading/empty/error state views
    Lib/
      Text.elm                # text helpers
  tests/
  review/
  public/
    manifest.webmanifest      # PWA manifest
    sw.js                     # service worker
  elm.json
  package.json
  vite.config.js
```

It is an Elm 0.19.1 `Browser.application` that talks to the Haskell backend
through HTTP JSON under `/api/v1`.

## Architecture

The frontend follows a strict ownership model:

```text
Main -> Route / Page
Page -> Data / Api / Ui / View
View -> Ui / Data
Api -> Client / Data decoders
Data -> other Data
Ui -> Theme / DesignTokens
```

Disallowed dependency directions:

- `Data` importing `Page`, `Main`, or `Api`.
- `Ui` importing page-specific modules.
- `Api` owning page state, navigation decisions, or view formatting.
- `View` triggering HTTP or owning page orchestration.

### Module Ownership

`Main.elm` is the app-wide composition root. It owns:

- URL changes and browser navigation.
- Bootstrap loading and application initialization.
- Page lifting and `Cmd.map`.
- Top-level theme state and PWA integration.

`Route.elm` is the single source of truth for route parsing and serialization.

`Page.*` modules own page-local TEA (Model, Msg, init, update, view),
page-local loading state, and page-specific composition of shared UI and API
calls.

`Api.*` modules own request construction, response decoding, and transport
errors. They do not own business interpretation beyond transport concerns.

`Data.*` modules own domain-facing frontend types and pure helpers. They are
safe to test in isolation without HTTP or page wiring.

`Ui.*` and `View.*` own shared presentation primitives. Pages compose them.

## Routes

Routes are defined in `Route.elm`:

| Route            | URL path                | Page module         |
|------------------|-------------------------|---------------------|
| `NetWorth`       | `/` or `/net-worth`     | `Page.NetWorth`     |
| `Investments`    | `/investments`          | `Page.Investments`  |
| `Cashflow`       | `/cashflow`             | `Page.Cashflow`     |
| `Accounts`       | `/accounts`             | `Page.Account`      |
| `Account name`   | `/accounts/{name}`      | `Page.AccountRegister` |
| `AccountValue name` | `/accounts/{name}/value` | `Page.Account`   |
| `Transactions`   | `/transactions`         | `Page.Transactions` |
| `Settings`       | `/settings`             | `Page.Settings`     |

`/` redirects to `/net-worth`. Account names are percent-encoded in URLs.

## Page Structure

Every non-trivial `Page.*` module follows standard TEA:

```elm
module Page.SomePage exposing (Model, Msg, init, update, view)

-- MODEL

type alias Model = ...

init : ... -> ( Model, Cmd Msg )
init args =
    ...

-- UPDATE

type Msg = ...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ...

-- VIEW

view : Model -> Element Msg
view model =
    ...
```

Pages compose `Ui.*` primitives and `View.*` helpers. They own their loading,
success, empty, stale, and error states.

## Pages

### Net Worth (`Page.NetWorth`)

Default dashboard. Fetches `POST /api/v1/net-worth/query`.

- Net Worth, Assets, and Liabilities KPI cards with absolute/percentage
  changes and sparkline trends.
- Performance chart (Net Investment vs Net Worth over time).
- Monthly/yearly granularity toggle.
- Assets and liabilities breakdown with category depth 1-4 and `Other`
  grouping.
- Asset and liability activity calendars.
- Day selection with transaction detail drill-down from complete DTOs.
- Incomplete period warnings.
- Skeleton loading states.

### Cashflow (`Page.Cashflow`)

Income/expense analytics. Fetches `POST /api/v1/cashflow/query`.

- Query panel with draft/apply/clear (reuses `Page.Shared.QueryPanel`).
- Monthly/yearly timeline with selected-period synchronization.
- Separate Income and Expense category breakdowns.
- Separate Income and Expense activity calendars.
- Calendar day to transaction detail drill-down.
- Filtered count display.
- Conversion-unavailable and empty states.

### Investments (`Page.Investments`)

Investment performance tracker. Fetches `POST /api/v1/investments/query`.

- Portfolio KPI cards: market value, purchase cost, gain/loss, total profit.
- Performance chart (market value and purchase cost over time).
- Monthly/yearly granularity toggle.
- Period selection with synchronized detail.
- Account drilldown with breadcrumbs and child holdings list.
- Performance breakdown: attributed income, realised gain, unrealised gain.
- Completeness badges (complete/partial/unavailable).
- Warnings display.

### Account Tree (`Page.Account`)

Hierarchical account browser. Fetches `GET /api/v1/accounts/tree`.

- Expandable/collapsible tree with account type labels/icons.
- Direct and subtree balances in raw commodities.
- Display-commodity valuation with completeness status.
- Wide list/detail and narrow layouts.
- Navigation to account register and account value detail.

### Account Register (`Page.AccountRegister`)

Account-level transaction ledger. Fetches `GET /api/v1/accounts/{name}/register`.

- Date range filtering (from/to).
- Cursor-based pagination with Load More.
- Month-grouped register entries with running balances.
- Transaction detail expansion.
- Wide list/detail and narrow layouts.

### Transactions (`Page.Transactions`)

Global transaction list. Fetches `GET /api/v1/transactions`.

- Hledger query text field with draft/apply/clear.
- Query help text and examples.
- Parser error display next to input.
- Cursor-based pagination with bounded page window (5 pages).
- Month-grouped transaction rows with posting previews.
- Full transaction detail with every posting, commodity, and cost.
- Transaction classification labels (income, expense, transfer, etc.).
- Wide list/detail and narrow replace-list-with-detail layouts.
- Loading, success, empty, stale, refreshing, and failed states.

### Settings (`Page.Settings`)

Minimal settings. No API calls.

- Light/dark theme toggle with immediate visual change.
- Theme persistence via `Preferences` and browser localStorage.

## State Modeling

### RemoteData

The frontend uses a `RemoteData` type parameterized by its error and value
types. Loading and failure states retain the previous value when one exists:

```elm
type RemoteData error value
    = NotAsked
    | Loading (Maybe value)
    | Success value
    | Failure error (Maybe value)
```

Pages can show retained content while loading or after a failed refresh instead
of replacing it with a spinner. Empty-result and freshness semantics belong to
the page response and its metadata rather than separate `RemoteData`
constructors.

### Response Metadata

Every API response includes `ResponseMetadata` with:

- `revision` — the journal revision that produced this response.
- `freshness` — `Fresh`, `Refreshing`, `Stale`, `Partial`, or `Unavailable`.
- `warnings` — structured conversion or calculation warnings.

## API Boundary

API modules live in `Api/`:

- `Client.elm` — centralizes request construction and API error handling.
- Per-feature modules decode their own response types.
- `Error.elm` — `ApiError` ADT (`BadUrl`, `Timeout`, `NetworkError`,
  `BadStatus`, `BadBody`).

Rules:

- `Api.*` owns transport details and decoding.
- `Page.*` owns what the data means for the screen.
- Transport failures are normalized through `Api.Error`.
- Session-expired behavior is surfaced through page `OutMsg` and handled
  in `Main`.

## PWA

The frontend is a Progressive Web App:

- `public/manifest.webmanifest` — standalone display mode, `/net-worth`
  start URL, Hedger identity.
- `public/sw.js` — service worker with app-shell caching of static assets,
  SPA fallback for navigation, `/api/*` bypass (never caches API responses).
- Offline support is limited to the app shell. Business data is not cached
  across sessions in Release 1.

## Design System

Shared primitives live in `Ui.elm`. Pages compose them.

### Visual Language

- `elm-ui` layout primitives, not CSS utility classes.
- System sans-serif typography.
- Light theme: page `#f6f7ff`, card `#ffffff`, primary text `#1e183d`, muted
  text `#6b6487`, and primary accent `#5b2ec4`.
- Dark theme: page `#0f172a`, card `#1e293b`, primary text `#f1f5f9`, muted
  text `#94a3b8`, and primary accent `#60a5fa`.
- Cards use theme-owned surfaces, borders, radii, and shadows.

### Typography Tokens (`Ui.Typography`)

```text
page title       24px
section title    18px
subsection       15px
body/detail      14px
caption/metadata 13px
chart label      15px
navigation       16px
brand            20px
detail hero      22px
KPI value        28px
```

### Controls

- `Ui.button`, `Ui.submitButton`, `Ui.dangerButton` for actions.
- `Ui.input`, `Ui.select` for form fields.
- `Ui.card`, `Ui.emptyState`, `Ui.loading`, `Ui.skeletonLoading` for common
  states.

### Theme

`Ui.Theme` provides light/dark theme tokens. The settings page toggles
between them. Theme preference is persisted in browser localStorage via
`Preferences.elm` and a port to `main.js`.

### Responsive Design

`Ui.Responsive` provides device-aware helpers:

- Phone vs desktop detection based on viewport width.
- Wide layout: sidebar navigation, split list/detail.
- Narrow layout: bottom navigation bar, replace-list-with-detail.
- Skeleton loading states match the target layout.

### Charts

`View.Analytics` provides chart primitives using `elm-visualization`:

- Timeline charts (net worth, cashflow, investment value over time).
- Bar charts for period breakdowns.
- Fixed axes with labeled ticks.

## Testing

Elm tests live in `frontend-web/tests/`:

```text
tests/
  Phase0Test.elm
  ActivityCalendarTest.elm
  RegisterTest.elm
  AnalysisTest.elm
  FoundationTest.elm
  InvestmentsTest.elm
  TransactionsTest.elm
```

Run:

```bash
just check-frontend
# or directly:
cd frontend-web && npm test
```

### Build Commands

```bash
just check-frontend          # all frontend checks
just check-frontend-format   # elm-format validation
just check-frontend-line-length  # 110 char limit
just format-frontend         # elm-format in-place
just dev-frontend            # Vite dev server
```

Or directly:

```bash
cd frontend-web && npm run dev      # dev server
cd frontend-web && npm run build    # production build
cd frontend-web && npm run format   # format
cd frontend-web && npm run review   # elm-review
```

No browser e2e test suite is configured.
