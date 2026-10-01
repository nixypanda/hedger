# Frontend

## Overview

The frontend is an Elm 0.19.2 Progressive Web App in `frontend-web/`. It uses
`mdgriffith/elm-ui` for layout and `gampleman/elm-visualization` for charts, and
talks to the backend through HTTP JSON under `/api/v1` plus a Server-Sent Events
stream at `/api/v1/events`.

```text
frontend-web/
  index.html
  src/
    Main.elm                  # app composition root
    Route.elm                 # URL routing
    Page.elm                  # page composition
    RemoteData.elm            # loading/refreshing/partial data states
    Preferences.elm           # browser-local theme persistence
    Pwa.elm                   # PWA lifecycle and install state
    Ports.elm                 # Elm ports
    main.js                   # Elm init, service worker, EventSource, install prompt
    Page/
      NetWorth.elm            # default dashboard
      Cashflow.elm            # income/expense analytics
      Investments.elm         # investment performance
      Account.elm             # account tree and account value detail
      AccountRegister.elm     # account register
      Transactions.elm        # transaction list with hledger query
      Settings.elm            # theme toggle
      Shared/
        Analysis.elm          # shared analytics host
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
      Json.elm                # shared JSON helpers
      Error.elm               # API error types
    Data/
      Account.elm             # account path and tree types
      AccountType.elm         # account type inference
      ActivityCalendar.elm    # activity calendar data
      Analytics.elm           # analytics read-models
      Calendar.elm            # calendar rendering
      Completeness.elm        # valuation completeness
      Date.elm                # date helpers
      Decimal.elm             # decimal quantity representation
      Freshness.elm           # data freshness
      Granularity.elm         # monthly/yearly
      Metadata.elm            # response metadata
      Paginated.elm           # paginated state machine
      Quantity.elm            # quantity formatting
      Revision.elm            # journal revision matching
      Status.elm              # data status types
      Time.elm                # time helpers
      Transaction.elm         # transaction read-models
    State/
      Viewport.elm            # viewport size
    Ui.elm                    # shared UI primitives
    Ui/
      Theme.elm               # light/dark theme tokens
      Typography.elm          # font sizes and styles
      Icon.elm                # icon set
      Skeleton.elm            # skeleton loading states
      Responsive.elm          # device-aware layout helpers
      Css.elm                 # CSS helpers
      DesignTokens.elm        # color and spacing tokens
      Calendar/Month.elm      # month grid
      Chart.elm               # chart toolkit
      Chart/Cartesian.elm     # cartesian charts
      Chart/Donut.elm         # donut charts
      Chart/Ring.elm          # activity rings
      Chart/Sparkline.elm     # KPI sparklines
    View/
      Shell.elm               # app shell, navigation, banners
      Analytics.elm           # chart and calendar rendering
      Transaction.elm         # transaction rendering
      Formatting.elm          # number and date formatting
      AccountPresentation.elm # account display helpers
      States.elm              # loading/empty/error state views
    Lib/
      Text.elm                # text helpers
      Time.elm                # time helpers
  tests/
  review/                     # elm-review configuration
  public/
    manifest.webmanifest      # PWA manifest
    sw.js                     # service worker
    icon.svg
  scripts/                    # check-ui-boundary.mjs
  elm.json
  package.json
  vite.config.js
```

`Main.elm` is an Elm `Browser.application`. It initializes from flags supplied
by `main.js`, which also owns everything the Elm side cannot: localStorage
preferences, service-worker registration, the install prompt, and the
`EventSource` connection.

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

A Node script (`scripts/check-ui-boundary.mjs`, run by `just
check-frontend-ui-boundary`) enforces part of this at check time.

### Module ownership

- `Main.elm` — URL changes and browser navigation, bootstrap loading, page
  lifting and `Cmd.map`, top-level theme state, PWA integration, and the live
  journal-change subscription.
- `Route.elm` — the single source of truth for route parsing and serialization.
- `Page.*` — page-local TEA (Model, Msg, init, update, view), page-local loading
  state, and page-specific composition of shared UI and API calls.
- `Api.*` — request construction, response decoding, and transport errors. No
  business interpretation beyond transport concerns.
- `Data.*` — domain-facing frontend types and pure helpers, testable without
  HTTP or page wiring.
- `Ui.*` / `View.*` — shared presentation primitives composed by pages.

## Routes

Routes are defined in `Route.elm`:

| Route                | URL path                  | Page module            |
|----------------------|---------------------------|------------------------|
| `NetWorth`           | `/` or `/net-worth`       | `Page.NetWorth`        |
| `Investments`        | `/investments`            | `Page.Investments`     |
| `Cashflow`           | `/cashflow`               | `Page.Cashflow`        |
| `Accounts`           | `/accounts`               | `Page.Account`         |
| `Account name`       | `/accounts/{name}`        | `Page.AccountRegister` |
| `AccountValue name`  | `/accounts/{name}/value`  | `Page.Account`         |
| `Transactions`       | `/transactions`           | `Page.Transactions`    |
| `Settings`           | `/settings`               | `Page.Settings`        |

`/` redirects to `/net-worth`. Account names are percent-encoded in URLs.

## Page structure

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

Pages compose `Ui.*` primitives and `View.*` helpers, and own their loading,
success, empty, stale, and error states.

## Pages

### Net worth (`Page.NetWorth`)

Default dashboard. Fetches `POST /api/v1/net-worth/query`.

- Net Worth, Assets, and Liabilities KPI cards with absolute/percentage changes
  and sparkline trends.
- Performance chart (Net Investment vs Net Worth over time).
- Monthly/yearly granularity toggle.
- Assets and liabilities breakdowns with category depth 1–4 and `Other`
  grouping.
- Asset and liability activity calendars with typed movements.
- Day selection with transaction drill-down from complete DTOs.
- Incomplete-period warnings and skeleton loading states.

### Cashflow (`Page.Cashflow`)

Income/expense analytics. Fetches `POST /api/v1/cashflow/query`.

- Query panel with draft/apply/clear (reuses `Page.Shared.QueryPanel`).
- Monthly/yearly timeline with selected-period synchronization.
- Separate income and expense category breakdowns.
- Separate income and expense activity calendars showing posted, reversed, and
  net amounts, including zero-net periods with activity.
- Calendar-day to transaction drill-down and filtered count.
- Conversion-unavailable and empty states.

### Investments (`Page.Investments`)

Investment performance tracker. Fetches `POST /api/v1/investments/query`.

- Portfolio KPI cards: market value, purchase cost, gain/loss, total profit.
- Performance chart (market value and purchase cost over time).
- Monthly/yearly granularity toggle and synchronized period selection.
- Account drill-down with breadcrumbs and child holdings.
- Performance breakdown: attributed income, realised gain, unrealised gain.
- Completeness badges (complete/partial/unavailable) and warnings.

### Account tree (`Page.Account`)

Hierarchical account browser. Fetches `GET /api/v1/accounts/tree`; the
`/accounts/{name}/value` route shows the same page focused on a specific
account.

- Expandable/collapsible tree with account type labels and icons.
- Direct and subtree balances in raw commodities.
- Display-commodity valuation with completeness status.
- Wide list/detail and narrow layouts, navigation to the account register.

### Account register (`Page.AccountRegister`)

Account-level ledger. Fetches `GET /api/v1/accounts/{name}/register`.

- Date-range filtering (from/to).
- Cursor-based pagination with Load More.
- Month-grouped entries with running balances.
- Transaction detail expansion, wide and narrow layouts.

### Transactions (`Page.Transactions`)

Global transaction list. Fetches `GET /api/v1/transactions`.

- Hledger query text field with draft/apply/clear, help text, and parser errors.
- Cursor-based pagination with a bounded page window (5 pages).
- Month-grouped rows with posting previews.
- Full transaction detail with every posting, commodity, and cost.
- Effect labels (with all effects in the detail view).
- Loading, success, empty, stale, refreshing, and failed states; a changed
  revision resets the window.

### Settings (`Page.Settings`)

Minimal settings, no API calls.

- Light/dark theme toggle with immediate visual change.
- Theme persistence via `Preferences` and browser localStorage.

## State modeling

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
of replacing it with a spinner. Paginated screens use `Data.Paginated`, a
separate opaque state machine (`NotAsked`, `Loading`, `Loaded`, `LoadingMore`,
`Failed`, and so on) that carries window, cursor, and selection state.

### Response metadata

Every API response includes `ResponseMetadata`:

- `revision` — the journal revision that produced the response.
- `freshness` — `Fresh`, `Refreshing`, `Stale`, `Partial`, or `Unavailable`.
- `warnings` — structured conversion or calculation warnings.

`Data.Freshness`, `Data.Completeness`, `Data.Metadata`, and `Data.Revision`
model these. `Data.Revision.ensureMatching` detects when a refresh produced a
different revision, which lets paginated screens reset rather than mix rows
from two journal states.

## API boundary

API modules live in `Api/`:

- `Client.elm` centralizes request construction and API error handling.
- Per-feature modules decode their own response types.
- `Error.elm` defines `ApiError` (`BadUrl`, `Timeout`, `NetworkError`,
  `BadStatus`, `BadBody`).
- `Metadata.elm` and `Json.elm` hold shared decoding helpers.

Rules:

- `Api.*` owns transport details and decoding.
- `Page.*` owns what the data means for the screen.
- Transport failures are normalized through `Api.Error`.
- Out-of-band behavior (such as a journal revision change) is surfaced through
  messages and handled in `Main`.

## Live updates

The backend streams a `revision` event whenever the journal is reloaded on disk.
`main.js` opens the stream and forwards events to Elm:

```js
const journalEvents = new EventSource("/api/v1/events");
journalEvents.addEventListener("revision", (event) =>
  app.ports.journalChanged.send(event.data)
);
```

`Main` handles `JournalChanged` by refreshing the current page. `EventSource`
reconnects on its own, so no retry logic lives in the app. This is what makes an
open dashboard reflect edits without a manual reload.

## PWA

The frontend is a Progressive Web App:

- `public/manifest.webmanifest` — standalone display mode, `/net-worth` start
  URL, Hedger identity.
- `public/sw.js` — service worker with app-shell caching of static assets, SPA
  fallback for navigation, and `/api/*` bypass (API responses are never cached).
- `main.js` registers the service worker in production and unregisters/caches-
  clears it in development.
- `Pwa.elm` models service-worker state (installing, update-ready, controlling)
  and install-hint state; `View.Shell` renders the corresponding banners.

Offline support is limited to the app shell. Business data is not cached across
sessions.

## Design system

Shared primitives live in `Ui.elm` and the `Ui.*` modules; pages compose them.

### Visual language

- `elm-ui` layout primitives, not CSS utility classes.
- System sans-serif typography.
- Light and dark themes with theme-owned surfaces, borders, radii, and shadows.

### Type and tokens

- `Ui.Typography` — the type scale (page title, section, body, caption, KPI
  value, and so on).
- `Ui.DesignTokens` — color and spacing tokens.
- `Ui.Theme` — light/dark theme records; the settings page toggles between them.

### Components

- `Ui.elm` exposes buttons, inputs, cards, chips, badges, overlays, list rows,
  segmented controls, and common loading/empty states.
- `Ui.Skeleton`, `Ui.Responsive`, and `Ui.Icon` provide loading placeholders,
  device-aware layout helpers, and the icon set.

### Charts

`Ui.Chart` and its submodules build on `elm-visualization`:

- `Ui.Chart.Cartesian` — keyed series, domains, and fixed labeled axes.
- `Ui.Chart.Donut` and `Ui.Chart.Ring` — category and activity rings.
- `Ui.Chart.Sparkline` — compact KPI trends.

`View.Analytics` composes these into timelines, breakdowns, and calendars.

### Responsive design

`Ui.Responsive` and `State.Viewport` provide phone/desktop detection: a sidebar
and split list/detail on wide layouts, a bottom navigation bar and
replace-list-with-detail on narrow layouts, with skeletons matching the target
layout.

## Known debt

- `Transaction.Presentation.classification` is a stringly-typed enum re-matched
  in several view/page sites; it should be decoded once into a custom type so
  the matches are total.
- Domain identifiers (commodity, account path, period, date) often travel as raw
  `String`/`Float` across the API boundary, so nothing prevents passing one where
  another is expected.
- `Ui.elm` is a large grab-bag of component families and exported helpers. It
  would benefit from being split by component family and having dead exports
  removed.

## Testing

Elm tests live in `frontend-web/tests/`:

```text
tests/
  ActivityCalendarTest.elm
  AnalysisTest.elm
  AnalyticsActivityTest.elm
  FormattingTest.elm
  FoundationTest.elm
  InvestmentsTest.elm
  PaginatedTest.elm
  RegisterTest.elm
  ThemeTest.elm
  TransactionsTest.elm
```

### Build and check commands

```bash
just check-frontend              # all frontend checks
just check-frontend-ui-boundary  # UI boundary rules
just check-frontend-format       # elm-format validation
just check-frontend-line-length  # 100 char limit
just format-frontend             # elm-format in place
just dev-frontend                # Vite dev server
```

Or directly:

```bash
cd frontend-web && npm run dev      # dev server
cd frontend-web && npm run build    # production build
cd frontend-web && npm test         # elm-test
cd frontend-web && npm run review   # elm-review
cd frontend-web && npm run format   # elm-format
```

No browser end-to-end test suite is configured.
