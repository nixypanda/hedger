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
    Chrome.elm                # app chrome assembly: navigation and PWA notices
    Route.elm                 # URL routing and document titles
    Page.elm                  # page composition
    Preferences.elm           # browser-local theme persistence
    Pwa.elm                   # PWA lifecycle and install state
    Ports.elm                 # Elm ports
    main.js                   # Elm init, service worker, EventSource, install prompt
    main.css                  # document shell, safe-area, app shell, shared CSS
    Component/
      Analysis.elm            # shared analytics report host
    Page/
      NetWorth.elm            # default dashboard
      Cashflow.elm            # income/expense analytics
      Investments.elm         # investment performance
      Account.elm             # account tree and account value detail
      AccountRegister.elm     # account register
      Transactions.elm        # transaction list with hledger query
      Settings.elm            # theme toggle
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
      Formatting.elm          # domain, quantity, and period display formatting
      Freshness.elm           # data freshness
      Granularity.elm         # monthly/yearly
      Metadata.elm            # response metadata
      Paginated.elm           # paginated state machine
      Quantity.elm            # quantity formatting
      Revision.elm            # journal revision matching
      Status.elm              # data status types
      Time.elm                # time helpers
      Transaction.elm         # transaction read-models and effects
    Lib/
      ListExtra.elm           # chunk/pad helpers
      Number.elm              # generic numeric formatting
      RemoteData.elm          # loading/refreshing/partial data states
      Time.elm                # generic time helpers
    Ui/
      Button.elm              # buttons
      Chip.elm                # chips and status/financial badges
      Color.elm               # color blending and alpha helpers
      Common.elm              # shared attributes and CSS color conversion
      Css.elm                 # theme CSS variables
      DesignTokens.elm        # layout, sizing, and spacing tokens
      Env.elm                 # theme, device, and viewport context
      Feedback.elm            # loading, empty, error, and warning states
      Icon.elm                # icon set
      Input.elm               # text, date, and segmented controls
      Notice.elm              # fixed-position transient notices
      Page.elm                # page scaffold, header, and period navigation
      Skeleton.elm            # skeleton loading shapes
      Surface.elm             # cards, sections, and surfaces
      Theme.elm               # light/dark theme tokens and mode parsing
      Typography.elm          # generic type scale and roles
      Viewport.elm            # width-driven layout thresholds
      Workspace.elm           # independently scrolling master/detail panes
      Calendar/Month.elm      # month grid
      Chart.elm               # chart toolkit
      Chart/Cartesian.elm     # cartesian charts
      Chart/Donut.elm         # donut charts
      Chart/Ring.elm          # activity rings
      Chart/Sparkline.elm     # KPI sparklines
    View/
      AccountPresentation.elm # account display helpers
      Chrome.elm              # app shell, navigation, fixed-position notices
      QueryPanel.elm          # stateless hledger query input panel
      States.elm              # freshness status and completeness badges
      Transaction.elm         # transaction rendering
      Analytics/
        Activity.elm          # month/year calendars and transaction lists
        Activity/Ring.elm     # activity ring construction and views
        Category.elm          # category breakdown cards and depth control
        Kpi.elm               # Net Worth and Investments KPI cards
        Legend.elm            # chart and activity legends
        Palette.elm           # analytics palette and activity colors
        Timeline.elm          # timeline charts and selected-period detail
        Types.elm             # shared analytics presentation types
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
`EventSource` connection. `Main` renders the page inside `Ui.Page.layout` and
`Chrome.view`; `Chrome` supplies navigation and PWA notices and delegates the
shell to `View.Chrome`.

## Architecture

The frontend follows a strict ownership model:

```text
Main -> Chrome / Route / Page
Chrome -> View.Chrome / Pwa / Ui / Route
Page -> Component / Data / Api / Ui / View / Lib
Component -> Data / Api / Ui / View / Lib
View -> Ui / Data / Lib
Api -> Api.Client / Api.Json / Data decoders
Data -> other Data / Lib
Lib -> no Data, Api, Page, or View
Ui -> Theme / DesignTokens / Common / Viewport / Data
```

Disallowed dependency directions:

- `Data` importing `Page`, `Main`, or `Api`.
- `Lib` importing `Data`, `Api`, `Page`, or `View`.
- `Ui` importing page-specific modules.
- `Api` owning page state, navigation decisions, or view formatting.
- `View` triggering HTTP or owning page orchestration.

A Node script (`scripts/check-ui-boundary.mjs`, run by `just
check-frontend-ui-boundary`) enforces the appearance boundary: pages and views
must express spacing, sizing, colors, and classes through `Ui` modules.

### Module ownership

- `Main.elm` — URL changes and browser navigation, bootstrap loading, page
  lifting and `Cmd.map`, the app environment and theme state, PWA integration,
  the live journal-change subscription, and theme CSS injection.
- `Chrome.elm` — app chrome assembly: navigation destinations, active-route
  state, and PWA notice/install actions passed to `View.Chrome`.
- `Route.elm` — the single source of truth for route parsing, serialization,
  and document titles.
- `Page.elm` — route-aware page model/message composition and page lifting.
- `Page.*` — page-local TEA (Model, Msg, init, update, view), page-local loading
  state, and page-specific composition of shared UI and API calls. Views receive
  `Ui.Env.Env`.
- `Component.*` — reusable stateful UI hosts. `Component.Analysis` owns the
  shared net-worth/cashflow analytics model, update loop, query panel, controls,
  and report view.
- `Api.*` — request construction, response decoding, and transport errors. No
  business interpretation beyond transport concerns.
- `Data.*` — domain-facing frontend types and pure helpers, testable without
  HTTP or page wiring. `Data.Formatting` owns domain display formatting.
- `Lib.*` — generic helpers only: `Lib.Number` formatting, `Lib.ListExtra`
  chunking, `Lib.RemoteData`, and `Lib.Time`. `Lib` never imports `Data`.
- `Ui.*` — reusable presentation primitives, layout, theme, and shared
  attributes. This is the only layer allowed to use raw elm-ui dimensions.
- `View.*` — stateless presentation composed from `Ui` and `Data`; `View.Analytics`
  is split into focused chart, category, activity, legend, palette, KPI, and
  shared-type modules.

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

view : Env -> Model -> Element Msg
view env model =
    ...
```

Pages compose `Ui.*`, `Component.*`, and `View.*` helpers, and own their
loading, success, empty, stale, and error states. Standard pages use
`Ui.Page.scaffold` for the header/body frame and `Ui.Workspace` for split
master/detail layouts; `Page.Investments` keeps its own page wrapper.

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

- Query panel with draft/apply/clear (reuses `View.QueryPanel`).
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
- Typed effect labels (with all effects in the detail view).
- Loading, success, empty, stale, refreshing, and failed states; a changed
  revision resets the window.

### Settings (`Page.Settings`)

Minimal settings, no API calls.

- Light/dark theme toggle with immediate visual change.
- Theme persistence via `Preferences` and browser localStorage.

## State modeling

### RemoteData

The frontend uses `Lib.RemoteData` parameterized by its error and value types.
Loading and failure states retain the previous value when one exists:

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
  and install-hint state; `Chrome` passes the corresponding notices to
  `View.Chrome`, which renders them as top-edge fixed-position notices. Page
  and query errors use bottom-edge fixed-position notices above the bottom
  navigation. Service-worker update prompts offer Update and Later; Later hides
  the prompt until the next update-ready event.

Offline support is limited to the app shell. Business data is not cached across
sessions.

## Design system

All shared presentation primitives now live under `Ui.*`; pages and views
compose them.

### Visual language

- `elm-ui` layout primitives, not CSS utility classes.
- System sans-serif typography.
- Light and dark themes with theme-owned colors, borders, and shadows; radii are
  `Ui.DesignTokens` constants.

### Environment and layout

- `Ui.Env` carries `{ theme, device, viewport }` through `Main` and pages.
- `Ui.Viewport` owns the width-driven content layout and responsive thresholds.
- `Ui.Page.layout` provides the page background and theme root.
- `Ui.Page.scaffold` renders the standard header band, header, and page container.
- `Ui.Workspace` lays out master/detail panes with independent scrolling; pages
  choose which panes are visible for the current layout.

### Type and tokens

The spacing and radius scales follow CalCo's scales. Typography uses its own
coarse scale. Layout spacing follows the scale;
component-internal chip and skeleton paddings may use explicit values. Layout
and component sizes (sidebar width, navigation height, chart sizes, column
widths, and similar) are separate constants, not scale steps.

- Spacing (`Ui.DesignTokens`): `0, 2, 4, 8, 12, 16, 20, 24, 32`.
- Radius (`Ui.DesignTokens`): `2` hairline, `8` small, `12` control, `20` card,
  `999` pill.
- Type (`Ui.Typography`): `12, 14, 16, 20, 24, 32, 44`.
  - `32` is the page title.
  - `24` is the section heading.
  - `20` is the list/month subsection label.
  - `16` is the card and in-card header step (`cardTitle`); it is also
    available for emphasized body and detail text via `bodyLarge`.
  - `14` is body text.
  - `12` is the small-text step for captions, metadata, chips, and compact
    labels.
  - `44` is reserved for future display text and is currently unused.
- `Ui.Typography` owns the generic scale (`Size`, `Weight`, `fontSize`,
  `fontWeight`, `fontFamily`) and app-agnostic roles (`display`, `headline`,
  `heading`, `subheading`, `body`, `bodyLarge`, `label`, `labelSmall`,
  `caption`, `numeric`). Document roles (`pageTitle`, `sectionTitle`,
  `subsectionTitle`, `cardTitle`) and muted text helpers also live there.
- Do not add app-specific roles such as KPI, row, brand, tab, or detail styles
  to `Ui.Typography`; compose the generic roles in the owning `Ui.*`,
  `Component.*`, or `View.*` module.
- `Ui.DesignTokens` — layout and component constants plus the spacing and
  radius scales.
- `Ui.Theme` — light/dark theme records and mode parsing; the settings page
  toggles between them.
- `Ui.Common` / `Ui.Color` — shared attributes, CSS color conversion, blending,
  and alpha helpers.

### Components

- `Ui.Button`, `Ui.Input`, `Ui.Surface`, `Ui.Chip`, `Ui.Feedback`, `Ui.Notice`,
  and `Ui.Skeleton` provide buttons, fields, cards, chips, state feedback,
  fixed-position notices, and loading placeholders.
- `Ui.Icon` provides the icon set.
- `Ui.Page` provides the page scaffold, headers, and period navigation.
- `Ui.Workspace` provides split-pane layout and scroll ownership.

### Charts

`Ui.Chart` and its submodules build on `elm-visualization`:

- `Ui.Chart.Cartesian` — keyed series, domains, and fixed labeled axes.
- `Ui.Chart.Donut` and `Ui.Chart.Ring` — category and activity rings.
- `Ui.Chart.Sparkline` — compact KPI trends.

`View.Analytics.*` composes these into KPI cards, timelines, breakdowns,
legends, and calendars. `View.Analytics.Palette` owns analytics colors and does
not depend on the activity views.

### Responsive design

`Ui.Env` and `Ui.Viewport` drive phone/desktop behavior: a sidebar and split
list/detail on wide layouts, a bottom navigation bar and
replace-list-with-detail on narrow layouts. `Ui.Workspace` mounts only the panes
a page selects, so narrow layouts drop the detail pane rather than hiding it.
Skeletons match the target layout.

## Known debt

- Domain identifiers (commodity, account path, period, date) often travel as raw
  `String`/`Float` across the API boundary, so nothing prevents passing one where
  another is expected.
- `Page.Investments` still uses its own master/detail wrapper and page scroll
  instead of `Ui.Page.scaffold` and `Ui.Workspace`; moving it is a separate
  parity task.
- Exposed-value pruning is manual because `NoUnused.Exports` is not enabled.

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
just check-frontend-review       # elm-review
just format-frontend             # elm-format in place
just dev-frontend                # Vite dev server
```

Or directly:

```bash
cd frontend-web && npm run dev      # dev server
cd frontend-web && npm run build    # production build
cd frontend-web && npm run check    # boundary, format, compile, review, tests
cd frontend-web && npm test         # elm-test
cd frontend-web && npm run review   # elm-review
cd frontend-web && npm run format   # elm-format
```

No browser end-to-end test suite is configured.
