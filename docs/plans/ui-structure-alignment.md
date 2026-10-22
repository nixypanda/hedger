# UI Structure Alignment Plan

Status: proposed for implementation
Branch: `refactor/ui-structure-alignment`
Scope: `frontend-web/` only
Reference: Calco `frontend-web` Elm frontend

This plan aligns Hedger's Elm UI structure with Calco's module families without
importing Calco-only products. Every stage is intended to leave the tree
compiling and the test suite passing. The full repository check is deliberately
left to the final stage.

## 1. Goals

The initial audit found these structural problems:

1. `Ui.elm` is a 973-line facade mixing page layout, surfaces, buttons, inputs,
   chips/badges, feedback, and common helpers. Calco has no `Ui.elm`; it exposes
   component families under `Ui/`.
2. `uiClass`/`minWidthZero` live in `Ui.Typography`; `Ui.Css.htmlClass`
   duplicates `uiClass`; color helpers are duplicated in `View.Analytics` and
   `Page.Investments`; there is no `Ui.Color`.
3. `Ui.Responsive` and `View.Shell` depend on `State.Viewport`; the UI layer
   imports app state. Calco threads an ambient `Ui.Env` view context.
4. Page scaffolds and split-pane layouts are duplicated across pages; page CSS
   classes leak from `index.html`.
5. `Main` builds shell, navigation, notices, bootstrap UI, and a placeholder
   page; `isRealPage _ = True` makes the placeholder branch dead and
   `Page.PlaceholderPage` is never constructed.
6. Shared modules are misplaced: `Page/Shared/QueryPanel` is stateless view,
   `Page/Shared/Analysis` is reusable nested TEA, `View/Formatting` is pure
   formatting, and `RemoteData` is a generic type.
7. `View.Analytics` is 1,996 lines and mixes KPI cards, charts, breakdowns,
   calendars, rings, legends, transaction lists, and formatting. Page/component
   state types live in it.
8. Presentation helpers are duplicated: KPI cards, legends, color blending,
   period shortening, chunking, money formatting, and skeleton bars.
9. `Preferences.ThemeMode` duplicates `Ui.Theme.Mode`; theme serialization is
   duplicated; `Page.Context` carries both `theme` and `themeMode`;
   `Ui.Css.themeCssVariables` is dead; `index.html` hardcodes theme CSS values;
   there is no `src/main.css`.
10. `Data.Transaction.Presentation.classification` and `effects` are raw strings
    re-matched in `View.Transaction` and `Page.Transactions`.
11. Dead exports/code remain, `elm-review` is not part of `just check-frontend`,
    and `Page.Investments` lacks canonical `-- MODEL` / `-- UPDATE` / `-- VIEW`
    sections.

The target is structural alignment, not a visual redesign and not a product
change. Calco-only products (access/auth, i18n, server store/caching, barcode
scanning, profile menus) stay out.

Baseline at planning time: `elm-format --validate`, `elm make src/Main.elm`,
`elm-test` (69 tests), and `npm run build` pass. `elm-review` reports three
errors: an unused `View.Analytics.categoriesForDirection`, a consecutive-list
simplification in `View.Transaction`, and a consecutive-list simplification in
`tests/ThemeTest.elm`. Stage 1 fixes these.

## 2. Target structure

```text
frontend-web/src/
  Main.elm                       # composition root only
  Chrome.elm                     # app chrome assembly: nav + PWA notices
  Page.elm
  Route.elm                      # route parsing + Route.title
  Preferences.elm                # browser-local preferences codec
  Pwa.elm
  Ports.elm
  Api/                           # unchanged ownership
  Component/
    Analysis.elm                 # moved from Page/Shared/Analysis
  Data/                          # domain-facing frontend types
    Transaction.elm              # now owns TransactionEffect
    ...
  Lib/
    Formatting.elm               # moved from View/Formatting
    ListExtra.elm                # chunksOf / padToMultiple
    RemoteData.elm               # moved from src/RemoteData.elm
    Text.elm
    Time.elm
  Ui/
    Button.elm
    Chip.elm
    Color.elm
    Common.elm
    Css.elm
    DesignTokens.elm
    Env.elm
    Feedback.elm
    Icon.elm
    Input.elm
    Page.elm
    Skeleton.elm
    Surface.elm
    Theme.elm
    Typography.elm
    Viewport.elm
    Workspace.elm
    Calendar/Month.elm
    Chart.elm
    Chart/Cartesian.elm
    Chart/Donut.elm
    Chart/Ring.elm
    Chart/Sparkline.elm
  View/
    Analytics/
      Activity.elm
      Activity/Ring.elm
      Category.elm
      Kpi.elm
      Legend.elm
      Palette.elm
      Timeline.elm
      Types.elm
    AccountPresentation.elm
    Chrome.elm                  # renamed from View/Shell.elm
    QueryPanel.elm              # moved from Page/Shared/QueryPanel.elm
    States.elm
    Transaction.elm
  main.css                      # document shell, motion, shared CSS classes
  main.js
```

Delete:

- `src/Ui.elm`
- `src/State/Viewport.elm` (and the `src/State/` directory)
- `src/Ui/Responsive.elm`
- `src/View/Shell.elm`
- `src/View/Formatting.elm`
- `src/View/Analytics.elm`
- `src/RemoteData.elm`
- `src/Page/Shared/` after both modules move
- `src/Api/Bootstrap.elm` and `Api.Client.getBootstrap` once the dead bootstrap
  UI is removed (see decision 2)

### Dependency rules

Adopt Calco's inward direction, adapted to Hedger:

```text
Main -> Chrome / Route / Page
Chrome -> View.Chrome / Pwa / Ui / Data.Route
Page -> Component / Data / Api / Ui / View / Lib
Component -> Data / Api types / Ui / View / Lib
View -> Ui / Data / Lib
Api -> Api.Client / Data decoders / Lib.Json
Data -> other Data / Lib
Ui -> Ui.Theme / Ui.DesignTokens / Ui.Common / Ui.Viewport / Data
```

Disallowed:

- `Ui` importing `Page`, `Main`, `Api`, or `State`.
- `View` triggering HTTP or owning page orchestration.
- `Component` owning routes or HTTP execution; pages execute effects.
- `Data` importing `Page`, `Main`, or `Api`.
- Page-specific CSS classes in `index.html`.

### Deliberate differences from Calco

- Hedger's `Ui.Env` carries `{ theme, device, viewport }`, not
  `{ theme, device, language }`. Hedger has no i18n. It carries the measured
  viewport because Hedger's existing breakpoints (1544 / 700 / 960) and chart
  widths are width-driven; reducing them to `Element.Device` classes would
  change layout behavior. `device` remains available for capability decisions;
  layout decisions use `Ui.Viewport`.
- No `Access.elm`, `Store.elm`, `I18n.elm`, or `Chrome` menu state. Hedger's
  chrome is stateless apart from PWA notices.
- `Ui.Workspace` supports weighted/fixed pane widths because Hedger's
  Investments navigator is fixed-width and its master/detail panes are 2:3.
  Calco's workspace only needs equal panes.

## 3. `Ui.elm` function-to-module map

Every export of `Ui.elm` maps as follows. Internal helpers move with their
public caller.

| Current export | New module | Notes |
| --- | --- | --- |
| `FinancialTone(..)` | `Ui.Chip` | Shared with `financialBadge`. |
| `StatusTone(..)` | `Ui.Chip` | Shared with status badges. |
| `PeriodLabelKind(..)` | `Ui.Page` | Used only by period navigation. |
| `layout` | `Ui.Page` | App root theme layout; only `Main` calls it. |
| `pageContainer` | `Ui.Page` | Max-width content wrapper. |
| `pageHeaderBand` | `Ui.Page` | Header band wrapper. |
| `pageHeader` | `Ui.Page` | Header content. |
| `periodNavigation` | `Ui.Page` | Generic previous/next period control. |
| `card` | `Ui.Surface` | Surface family. |
| `cardWith` | `Ui.Surface` | |
| `section` | `Ui.Surface` | |
| `formSection` | `Ui.Surface` | |
| `hairline` | `Ui.Surface` | |
| `rowSurfaceAttrs` | `Ui.Surface` | Row selection surface. |
| `accentDot` | `Ui.Surface` | Small color accent. |
| `primaryButton` | `Ui.Button` | Keep existing names/signatures. |
| `secondaryButton` | `Ui.Button` | |
| `dangerButton` | `Ui.Button` | |
| `ghostButton` | `Ui.Button` | |
| `iconButton` | `Ui.Button` | |
| `backButton` | `Ui.Button` | |
| `fieldLabel` | `Ui.Input` | |
| `dateInput` | `Ui.Input` | |
| `searchInputInline` | `Ui.Input` | |
| `segmentedControl` | `Ui.Input` | |
| `segmentedOption` | `Ui.Input` | |
| `wideSegmentedControl` | `Ui.Input` | |
| `chip` | `Ui.Chip` | |
| `badge` | `Ui.Chip` | Delete if still unused after bootstrap removal. |
| `financialBadge` | `Ui.Chip` | |
| `statusBadge` | `Ui.Chip` | |
| `wrappingStatusBadge` | `Ui.Chip` | Currently unused; delete in stage 10. |
| `legendButton` | `Ui.Chip` | Legend chip selection control. |
| `legendToggleButton` | `Ui.Chip` | Legend show more/less control. |
| `loading` | `Ui.Feedback` | |
| `emptyState` | `Ui.Feedback` | |
| `errorBanner` | `Ui.Feedback` | |
| `truncatedText` | `Ui.Common` | Generic element helper. |
| `touchTargetHeight` | `Ui.Common` | |
| `hoverBackground` | `Ui.Common` | |
| `hoverBorder` | `Ui.Common` | |
| private `buttonBase` | `Ui.Button` | Private. |
| private `iconButtonBase` | `Ui.Button` | Private. |
| private `inputAttrs` | `Ui.Input` | Private. |
| private `inputBaseAttrs` | `Ui.Input` | Private. |
| private `segmentedSurface` | `Ui.Input` | Private. |
| private `chipAttrs` | `Ui.Chip` | Private. |
| private `coloredPill` | `Ui.Chip` | Private. |
| private `statusToneColors` | `Ui.Chip` | Private. |
| private `legendSurface` | `Ui.Chip` | Private. |
| private `legendToggleSurface` | `Ui.Chip` | Private. |
| private `optionalWithDivider` | `Ui.Surface` | Private. |
| private `optionalFooter` | `Ui.Surface` | Private. |
| private `periodLabelWidth` | `Ui.Page` | Private. |

Supporting moves:

| Current location | New location |
| --- | --- |
| `Ui.Typography.uiClass` | `Ui.Common.uiClass` |
| `Ui.Typography.minWidthZero` | `Ui.Common.minWidthZero` |
| `Ui.Css.htmlClass` | Delete; use `Ui.Common.uiClass` |
| `Ui.Css.toCssColor` | `Ui.Common.toCssColor` |
| `Ui.Css.themeCssVariables` | Stays in `Ui.Css`, becomes live |
| `Ui.Responsive.ContentLayout(..)` | `Ui.Viewport.ContentLayout(..)` |
| `Ui.Responsive.contentLayout` | `Ui.Viewport.contentLayout` |
| `Ui.Responsive.contentPadding` | `Ui.Viewport.contentPadding` |
| `Ui.Responsive.isCompact` | `Ui.Viewport.isCompact` |
| `Ui.Responsive.usableContentWidth` | `Ui.Viewport.usableContentWidth` |
| `Ui.Responsive.usesBottomNavigation` | `Ui.Viewport.usesBottomNavigation` |
| `Ui.Responsive.usesMasterDetail` | `Ui.Viewport.usesMasterDetail` |
| `State.Viewport.Viewport` | `Ui.Viewport.Viewport` |
| `View.Analytics` color blend/alpha helpers | `Ui.Color.mix` / `Ui.Color.withAlpha` |
| `Page.Investments.mixColor` | `Ui.Color.mix` |
| `Page.Investments.chunksOf` / `padToMultiple` | `Lib.ListExtra.chunksOf` / `padToMultiple` |
| `View.Analytics.chunks` / `padToMultiple` | `Lib.ListExtra` |
| `View.Analytics.decimalFloat` / `Page.Investments.investmentDecimalFloat` | `Data.Decimal.toFloat` |
| `View.Analytics.money` / `Page.Investments.investmentMoney` | `Lib.Formatting.formatDecimalDisplay` |
| `View.Analytics.axisMoney` | `Lib.Formatting.formatDecimalAxis` |
| `View.Analytics.shortPeriod` / `Page.Investments.shortInvestmentPeriod` | `Lib.Formatting.shortPeriod` / `shortPeriodWithYear` |
| `View.Analytics.periodName` | `Lib.Formatting.periodDisplayName` |

## 4. Staged implementation

Unless a stage says otherwise, run the following from `frontend-web/`:

```bash
./node_modules/.bin/elm-format src tests --validate
npm run check                 # ui-boundary + format + elm make + elm-test
npm run review                # elm-review, expected clean from stage 1 on
npm test
npm run build
```

`npm run check` already prepends `node_modules/.bin` to `PATH`, so the local
`elm`, `elm-format`, and `elm-test` binaries are used. If a tool is somehow
missing, enter one persistent `nix develop` shell rather than wrapping every
command in `nix develop -c`. The full `just check` is run only in stage 10.

### Stage 1 — Foundation helpers and green `elm-review`

Goal: remove the low-level duplication that every later stage would otherwise
carry forward, and fix the three existing `elm-review` errors so review can be
a continuous gate.

Create:

- `src/Ui/Common.elm` with `uiClass`, `minWidthZero`, `toCssColor`.
- `src/Ui/Color.elm` with `mix` and `withAlpha` (Calco's API).

Change:

- `src/Ui/Typography.elm`: remove `uiClass` and `minWidthZero`; keep typography
  tokens and text helpers only.
- `src/Ui/Css.elm`: delete `htmlClass` and `toCssColor`; temporarily keep
  `themeCssVariables` (it becomes live in stage 6).
- `src/Ui.elm`, `src/Ui/Chart.elm`, `src/Ui/Chart/Cartesian.elm`,
  `src/Ui/Chart/Donut.elm`, `src/Ui/Chart/Ring.elm`,
  `src/Ui/Chart/Sparkline.elm`, `src/Ui/Icon.elm`: import `Ui.Common` for
  `toCssColor`/`uiClass`/`minWidthZero`.
- Every file using `Typography.uiClass` or `Typography.minWidthZero` switches to
  `Ui.Common`; this is mostly `Main`, `View/Shell`, `View/Transaction`,
  `View/Analytics`, `Page/Account`, `Page/AccountRegister`, `Page/Cashflow`,
  `Page/Investments`, `Page/NetWorth`, `Page/Settings`, `Page/Transactions`, and
  `Page/Shared/Analysis`.
- `Page/Investments.mixColor` is replaced by `Ui.Color.mix`.
- `View/Analytics.blendColors`/`withAlpha` are replaced by
  `Ui.Color.mix`/`Ui.Color.withAlpha`.
- Pre-clean the current review failures: delete unused
  `View/Analytics.categoriesForDirection`, simplify the list concatenation in
  `View/Transaction.transactionDetail`, and simplify the list concatenation in
  `tests/ThemeTest.elm`.

Deferred: all component-family moves; `Preferences.ThemeMode`; page signature
changes.

Verify: standard gate above. `npm run review` must report zero errors after this
stage.

Commit: `refactor(frontend): add shared UI common and color helpers`

### Stage 2 — Split `Ui.elm` into component families behind a facade

Goal: move implementations into the target `Ui/*` modules while keeping
`Ui.elm` as a temporary re-export facade, so no call site changes and the tree
compiles after each file move.

Create:

- `src/Ui/Button.elm` — `primaryButton`, `secondaryButton`, `dangerButton`,
  `ghostButton`, `iconButton`, `backButton`; private button bases.
- `src/Ui/Surface.elm` — `card`, `cardWith`, `section`, `formSection`,
  `hairline`, `rowSurfaceAttrs`, `accentDot`.
- `src/Ui/Chip.elm` — `chip`, `badge`, `StatusTone(..)`, `statusBadge`,
  `wrappingStatusBadge`, `FinancialTone(..)`, `financialBadge`, `legendButton`,
  `legendToggleButton`.
- `src/Ui/Input.elm` — `fieldLabel`, `dateInput`, `searchInputInline`,
  `segmentedControl`, `wideSegmentedControl`, `segmentedOption`.
- `src/Ui/Page.elm` — `layout`, `pageContainer`, `pageHeaderBand`, `pageHeader`,
  `PeriodLabelKind(..)`, `periodNavigation`. This module initially imports
  `Ui.Responsive.contentPadding`; stage 4 changes that to `Ui.Viewport`.
- `src/Ui/Feedback.elm` — `loading`, `emptyState`, `errorBanner`.
- Extend `src/Ui/Common.elm` with `touchTargetHeight`, `hoverBackground`,
  `hoverBorder`, and `truncatedText` moved out of `Ui.elm`.

Change:

- `src/Ui.elm` becomes a thin facade importing the modules above and re-exporting
  the same names, including the `StatusTone(..)` and `FinancialTone(..)`
  constructors and `PeriodLabelKind(..)`. `Ui.layout` forwards to
  `Page.layout`.
- No page, view, or test import changes in this stage.

Deferred: deleting `Ui.elm`; updating call sites.

Verify: standard gate. Additionally run:

```bash
rg -n '^module Ui' src/Ui.elm src/Ui/*.elm
```

and confirm `Ui.elm` contains only imports, re-exported definitions, and no
component implementation.

Commit: `refactor(frontend): split Ui facade into component families`

### Stage 3 — Migrate imports and delete `Ui.elm`

Goal: replace the facade with explicit component-family imports.

Call-site mapping:

- `Main.elm`: `Ui.Page` (`layout`, `pageContainer`, `pageHeaderBand`,
  `pageHeader`), `Ui.Chip` (`badge`, `statusBadge`), `Ui.Surface` (`card`),
  `Ui.Feedback` (`loading`, `errorBanner`).
- `View/Shell.elm`: `Ui.Button` (`primaryButton`, `ghostButton`),
  `Ui.Common` (`touchTargetHeight`).
- `View/States.elm`: `Ui.Chip` (`statusBadge`), `Ui.Feedback` (`errorBanner`).
- `View/Transaction.elm`: `Ui.Surface` (`card`, `section`, `hairline`),
  `Ui.Chip` (`FinancialTone`).
- `View/Analytics.elm`: `Ui.Surface`, `Ui.Chip`, `Ui.Input`, `Ui.Page`,
  `Ui.Feedback`, `Ui.Common`, `Ui.Color`.
- `Page/Shared/Analysis.elm`: `Ui.Page` (`periodNavigation`, `PeriodLabelKind`),
  `Ui.Input` (`segmentedControl`/`segmentedOption`), `Ui.Surface`, `Ui.Chip`,
  `Ui.Feedback`, `Ui.Common`.
- `Page/Shared/QueryPanel.elm`: `Ui.Input`, `Ui.Button`, `Ui.Surface`,
  `Ui.Feedback`, `Ui.Chip`.
- `Page/NetWorth.elm`, `Page/Cashflow.elm`, `Page/Investments.elm`: `Ui.Page`,
  `Ui.Input`, `Ui.Button`, `Ui.Surface`, `Ui.Chip`, `Ui.Feedback`,
  `Ui.Common`, `Ui.Skeleton`.
- `Page/Account.elm`, `Page/AccountRegister.elm`, `Page/Transactions.elm`:
  `Ui.Page`, `Ui.Surface`, `Ui.Chip`, `Ui.Feedback`, `Ui.Button`, `Ui.Input`,
  `Ui.Common`, `Ui.Skeleton`.
- `Page/Settings.elm`: `Ui.Page`, `Ui.Input`, `Ui.Surface`.

Also:

- Update `scripts/check-ui-boundary.mjs` to drop the special case for
  `Ui.elm`; after this stage every UI module is under `Ui/`.
- Delete `src/Ui.elm`.
- Use `rg -n '^import Ui$' src tests` to confirm no facade imports remain;
  qualified imports such as `Ui.Theme`, `Ui.Icon`, `Ui.Typography`,
  `Ui.Skeleton`, `Ui.Chart`, and `Ui.Calendar` stay because they are real
  modules, not facade names.

Deferred: `Ui.Env`, page scaffolds, Chrome extraction.

Verify: standard gate. `npm run check` proves the boundary script and compile
are clean.

Commit: `refactor(frontend): migrate to explicit Ui component imports`

### Stage 4 — Ambient `Ui.Env`, `Ui.Viewport`, and theme-mode unification

Goal: remove app state from the UI layer and make `Ui.Theme.Mode` the single
theme mode type.

Create:

- `src/Ui/Viewport.elm`:

  ```elm
  type alias Viewport =
      { width : Int, height : Int }

  type ContentLayout
      = Compact
      | Medium
      | Wide

  contentPadding : Int
  usableContentWidth : Viewport -> Int
  contentLayout : Viewport -> ContentLayout
  isCompact : ContentLayout -> Bool
  usesBottomNavigation : Viewport -> Bool
  usesMasterDetail : Viewport -> Bool
  ```

  Preserve today's exact behavior: bottom navigation below 1544px, 232px
  navigation width, 16px content padding, and the 700/960 content-layout
  thresholds.

- `src/Ui/Env.elm`:

  ```elm
  type alias Env =
      { theme : Theme
      , device : Device
      , viewport : Viewport
      }

  withTheme : Env -> Theme -> Env
  withDevice : Env -> Device -> Env
  withViewport : Env -> Viewport -> Env
  resize : Env -> Int -> Int -> Env
  ```

  `resize` updates the measured viewport and `Element.classifyDevice`; layout
  code must use `Ui.Viewport`, not `device.class`, where the existing thresholds
  matter.

Sub-stages (each compiles):

1. Add `Ui.Viewport`; make `State/Viewport.elm` a temporary alias to
   `Ui.Viewport.Viewport`, and point `Ui.Responsive` at `Ui.Viewport`. This keeps
   existing page signatures compiling.
2. Add `Ui.Env`; change `Main.Model` from `viewport : Viewport` to
   `env : Env`; `Resized` uses `Ui.Env.resize`; theme changes update
   `env.theme`. Change `Page.Context` to `{ route : Route, env : Env, themeMode :
   ThemeMode }` temporarily, so `Page/Settings.elm` keeps compiling until the
   theme-mode step. `Page.elm` temporarily passes `context.env.theme` and
   `context.env.viewport` to pages that still use the old signatures.
3. Unify theme mode:
   - `Preferences` stores `mode : Ui.Theme.Mode`; wire field remains
     `"themeMode"` for localStorage compatibility.
   - Use `Ui.Theme.modeFromString`/`modeToString`; delete
     `Preferences.ThemeMode`, its duplicate serializers, and
     `Preferences.theme`.
   - `Page.Settings` uses `Ui.Theme.Mode`; `Page.init` reads
     `context.env.theme.mode`; `Page.Context.themeMode` disappears.
   - `Page.OutMsg.SetThemeMode` and `Page.Settings.OutMsg` use
     `Ui.Theme.Mode`; `Main.applyPageOutMsg` updates `preferences` and
     `env.theme`.
   - Update `Main`, `Page.elm`, `Page/Settings.elm`, and
     `tests/FoundationTest.elm`.
4. Convert page views to `Env`:
   - `Page.NetWorth.view`, `Page.Cashflow.view`, `Page.Investments.view`,
     `Page.Account.view`/`valueView`, `Page.AccountRegister.view`,
     `Page.Transactions.view`, `Page.Settings.view` become
     `Env -> Model -> Element Msg`.
   - `Page.update` only passes `Env` where needed (Account's update props).
   - `View.Shell.view` becomes `Env -> { notice, navigation, page } -> Element
     msg`.
   - `Page/Shared/Analysis.view` becomes
     `Env -> Config -> Report -> Model -> Element Msg`; `controlsView` may keep
     `Theme` because it only needs theme.
   - Inside pages, bind `theme = env.theme` and `viewport = env.viewport` at the
     top of `view` to keep the diff mechanical.
5. Delete `src/Ui/Responsive.elm` and `src/State/Viewport.elm` (and the empty
   `State/` directory). Update `tests/FoundationTest.elm` to import
   `Ui.Viewport` and to use `Preferences`' new mode field.

Deferred: `Ui.Page.scaffold`/`Ui.Workspace`; Chrome extraction.

Verify: standard gate, plus:

```bash
rg -n 'State\.Viewport|Ui\.Responsive|ThemeMode' src tests
```

The only expected hits are `"themeMode"` JSON wire strings in
`Preferences`/tests and `main.js` localStorage.

Manual checks: resize at 390, 768, 1024, 1543, 1544, and 1920 widths; confirm
bottom navigation, master/detail, and chart widths do not change from the
baseline. Toggle light/dark and reload to confirm persistence.

Commits (recommended):

- `refactor(frontend): add Ui.Env and Ui.Viewport`
- `refactor(frontend): use Ui.Theme.Mode in preferences`
- `refactor(frontend): pass Ui.Env through page views`
- `refactor(frontend): remove State.Viewport and Ui.Responsive`

### Stage 5 — Page scaffolds and split-pane workspace

Goal: remove duplicated page scaffolding and page-specific CSS classes.

Extend `Ui.Page`:

```elm
type alias Header msg =
    { leading : Maybe (Element msg)
    , title : Element msg
    , subtitle : Maybe String
    , actions : List (Element msg)
    }

scaffold :
    Theme
    -> { header : Header msg, body : List (Element msg) }
    -> Element msg
```

`scaffold` renders a full-width column with `pageHeaderBand` + `pageHeader` and
then `pageContainer body`. It does not add page-local scrolling: single-pane
pages scroll through the app content wrapper, while split panes scroll through
`Ui.Workspace`.

Create `Ui.Workspace`:

```elm
type PaneWidth
    = Fill
    | Portion Int
    | Fixed Int

type alias Pane msg =
    { key : String
    , width : PaneWidth
    , content : Element msg
    }

view : Env -> List (Pane msg) -> Element msg
```

Behavior:

- Use `Ui.Viewport.usesMasterDetail` for the two-pane limit, not
  `Element.Device`, so the existing 1024-width behavior is preserved.
- Show one pane at a time below the threshold, two above it.
- Use `Element.Keyed` for panes so scroll offsets and selection state survive
  re-renders. Use stable keys such as `accounts`, `account-detail`,
  `transactions`, `transaction-detail`, `register`, `register-detail`,
  `investments-navigator`, and `investments-history`.
- Render panes as independent `height fill` scroll containers with
  `min-height: 0`.
- Return the single pane content directly when only one pane is visible, so
  ordinary page scrolling still comes from the app content wrapper.

Migrate pages:

- `Page.NetWorth`, `Page.Cashflow`, `Page.Investments`: replace the outer
  `column` + `pageHeaderBand` + `pageContainer` with `Ui.Page.scaffold`. Move
  transaction detail headers to `scaffold` with a `leading` back button.
- `Page.Settings`: use `scaffold`.
- `Page.Account`: list and value/detail views use `scaffold`; `wideLayout`
  becomes `Ui.Workspace.view env` with master `Portion 2` and detail
  `Portion 3`.
- `Page.AccountRegister`: list and detail views use `scaffold`; the wide
  selected layout uses `Workspace` with `Portion 2` / `Portion 3`.
- `Page.Transactions`: list and detail views use `scaffold`; the wide selected
  layout uses `Workspace` with `Portion 2` / `Portion 3`.
- `Page.Investments`: use `Workspace` only in the wide branch with navigator
  `Fixed Tokens.investmentNavigatorWidth` and history `Fill`; keep the compact
  branch as a stacked column because the current compact layout shows the
  navigator above the history.

Remove these class names from page modules after the switch, and remove the
corresponding selectors from `index.html` where they still exist:

- `page-scroll`
- `analytics-page`
- `transactions-page`
- `transactions-master-pane`
- `transactions-detail-pane`
- `transactions-wide-layout`
- `accounts-wide-layout`
- `account-register-page`
- `account-detail-page`
- `register-detail-page`
- `transaction-detail-page`

`Ui.Workspace` may attach `ui-workspace`/`ui-workspace-pane` classes if needed;
put those rules in `main.css` in stage 6, or express the layout with elm-ui
attributes and no CSS.

Deferred: Chrome extraction; CSS file move; skeleton cleanup.

Verify: standard gate, plus a manual pass at 390, 768, 1024, 1544, and 1920
widths:

- exactly one scrollbar for single-pane pages;
- independent scroll for master/detail panes;
- load-more and selected-row visibility in Transactions/AccountRegister;
- back navigation and detail selection on narrow screens;
- Investments navigator/history split and compact stack.

Commits (recommended):

- `refactor(frontend): add Ui.Page scaffold`
- `refactor(frontend): adopt page scaffolds in analytics and settings`
- `refactor(frontend): adopt page scaffolds in account and transaction pages`
- `refactor(frontend): add Ui.Workspace split panes`
- `refactor(frontend): remove page-specific CSS classes`

### Stage 6 — Extract Chrome, remove dead Main branches, move document CSS

Goal: make `Main` a composition root; remove the dead placeholder/bootstrap
path; move browser-owned CSS out of `index.html`.

Create:

- `src/Chrome.elm` — app chrome assembly. Suggested signature:

  ```elm
  view :
      { env : Env
      , route : Route
      , pwa : Pwa.Model
      , page : Element msg
      , onNavigate : Route -> msg
      , onPwa : Pwa.Msg -> msg
      }
      -> Element msg
  ```

  Move from `Main`: `shellNavigation`, `destination`, `routeIsActive`,
  `shellNotice`, `installationNotice`, `shellAction`, and `dismissAction`.

- `src/View/Chrome.elm` — rename `src/View/Shell.elm` with `git mv`, update the
  module name, and keep the existing `Destination`, `Notice`, `NoticeAction`,
  and `ActionStyle` types. It takes `Env` instead of separate `theme`/`viewport`
  fields.

- `src/main.css` — move the document shell and shared CSS from `index.html`:
  `html/body/#app`, safe-area padding, `.app-shell`, `.shell-body`,
  `.app-content`, `.ui-card-shadow`, `.ui-text-ellipsis`, `.ui-text-wrap`,
  `.ui-motion`, `.ui-surface`, `.ui-chart-motion`, hover rules, skeleton
  shimmer, reduced-motion rules, and any workspace rules. Keep only
  browser-owned/document-level CSS here; delete page-specific selectors.

Change:

- `Route` exposes `title : Route -> String`, moved from `Main.pageTitle`.
- `Main.Model` drops `bootstrap`; `Main.Msg` drops `BootstrapLoaded` and
  `RefreshBootstrap`; `init` drops `Api.getBootstrap`; `JournalChanged` stops
  refreshing bootstrap.
- Delete from `Main`: `bootstrapStatus`, `bootstrapSummary`, `placeholder`,
  `pageTitle`, `isRealPage`, and the placeholder branch in `pageView`.
- Delete `Page.PlaceholderPage` from `Page.Model` and its `init`/`view`
  fallbacks where the route union now makes them unreachable.
- Delete `Api.Bootstrap` and `Api.Client.getBootstrap` if the orchestrator
  approves decision 2. The backend endpoint stays; this is frontend dead-code
  removal only.
- Delete `Ui.Skeleton.loading` (only `Main` bootstrap used it).
- `Main.view` prepends a theme style node to the document body:

  ```elm
  Html.node "style" []
      [ Html.text (":root { " ++ Ui.Css.themeCssVariables theme ++ " }") ]
  ```

  followed by `Element.layoutWith ... [] (Ui.Page.layout theme (Chrome.view ...))`.
- `src/main.js` imports `./main.css` at the top.
- `index.html` keeps the early theme boot script (to set `data-theme` before
  first paint), metadata, and the app node; remove the large `<style>` block and
  hardcoded theme variable block. A minimal pre-hydration page background may
  remain in `main.css`; component tokens are owned by `Ui.Css.themeCssVariables`
  from Elm.

Sub-stages (each compiles):

1. Add `Chrome.elm` as a wrapper over the existing `View.Shell.view`; switch
   `Main` to it; verify.
2. Move the shell module to `View/Chrome.elm` and update the wrapper.
3. Delete the dead bootstrap/placeholder path and API wiring.
4. Create/import `main.css`, move the style block, and inject theme variables
   from `Main`.

Verify: standard gate, plus manual checks:

- all six navigation destinations and active states;
- PWA update-ready and install notices (mobile Safari/Chrome if available);
- live journal revision refresh still refreshes the current page;
- theme toggle updates CSS variables and localStorage;
- first paint in dark mode has no white flash;
- `rg -n 'Bootstrap|PlaceholderPage|isRealPage|View\.Shell|Ui\.Skeleton\.loading' src`
  is empty.

Commits (recommended):

- `refactor(frontend): extract app chrome from Main`
- `refactor(frontend): remove dead placeholder and bootstrap UI`
- `refactor(frontend): move document styles to main.css and inject theme variables`

### Stage 7 — Move shared modules to their owning layers

Goal: put stateless view, nested TEA, pure formatting, and generic data types in
the right modules.

Moves (use `git mv` to preserve history):

| From | To | Reason |
| --- | --- | --- |
| `Page/Shared/Analysis.elm` | `Component/Analysis.elm` | Reusable nested TEA with `Model`/`Msg`/`Effect`. |
| `Page/Shared/QueryPanel.elm` | `View/QueryPanel.elm` | Stateless view. |
| `View/Formatting.elm` | `Lib/Formatting.elm` | Pure string/number formatting. |
| `src/RemoteData.elm` | `Lib/RemoteData.elm` | Generic state type, matching Calco's `Lib.Loadable` placement. |

Update imports:

- `Page.NetWorth`, `Page.Cashflow`: `Component.Analysis as Analysis`.
- `Page.Cashflow`, `Page.Transactions`: `View.QueryPanel as QueryPanel`.
- `Page.Investments`, `Page.Account`, `Page.AccountRegister`,
  `View.Transaction`, `View.Analytics`, and `tests/FormattingTest.elm`:
  `Lib.Formatting as Formatting`.
- `View.States`, `Page.NetWorth`, `Page.Cashflow`, `Page.Investments`,
  `Page.Transactions`, and `tests/FoundationTest.elm`: `Lib.RemoteData`.
- `tests/AnalysisTest.elm`: `Component.Analysis as Analysis`.
- Rename the `Component.Analysis` module declaration and its documentation.
  Its `view` already takes `Env` after stage 4; keep `controlsView` on `Theme`
  unless it starts needing device/viewport data.
- Delete the now-empty `Page/Shared/` directory.

Deferred: splitting `View.Analytics`; moving page-shaped view helpers.

Verify: standard gate, plus:

```bash
rg -n 'Page\.Shared|View\.Formatting|import RemoteData|View\.Shell' src tests
```

Expected: no output.

Commit: `refactor(frontend): move shared analysis and pure helpers`

### Stage 8 — Split analytics, dedupe presentation helpers, prune skeleton

Goal: replace the 1,996-line `View.Analytics` with focused modules and remove
duplicated KPI/legend/formatting/chunking code.

Supporting helpers (commit separately; compile after each):

- Add `Lib/ListExtra.elm` with total `chunksOf` and `padToMultiple` (guard
  non-positive sizes).
- Add `Data.Decimal.toFloat : Decimal -> Float` and use it in place of local
  `decimalFloat`/`investmentDecimalFloat` helpers.
- Extend `Lib.Formatting` with:
  - `formatDecimalDisplay : String -> Decimal -> String`
  - `formatDecimalAxis : String -> Float -> String`
  - `shortPeriod : String -> String` (`"2026-03"` -> `"03"`)
  - `shortPeriodWithYear : String -> String` (`"2026-03"` -> `"03/26"`)
  - `periodDisplayName : String -> String`
- Replace the duplicate implementations in `Page.Investments` and
  `View.Analytics`.

Create:

- `src/View/Analytics/Types.elm` — `Palette(..)`, `ActivityTone(..)`,
  `CalendarDisplay(..)`, `LegendState(..)`. These are presentation/component
  state types, so `Component.Analysis` may import them (Calco allows
  `Component -> View`).
- `src/View/Analytics/Palette.elm` — `paletteColor`, `otherColor`,
  `activityColor`, `activityCategoryColor`, `flowMovementColors`, and
  direction-spec/color helpers.
- `src/View/Analytics/Kpi.elm` — `kpiCard` (NetWorth's sparkline/comparison
  card) and `metricCard` (Investments' icon/title/value card), sharing private
  icon-tile and value typography helpers. Delete
  `Page.Investments.investmentMetricCard`.
- `src/View/Analytics/Timeline.elm` — `positiveNegativeTimelineChart`,
  `netWorthPerformanceChart`; private selected-period detail helpers.
- `src/View/Analytics/Category.elm` — `categoryCard` and `depthControl`;
  private donut/row helpers.
- `src/View/Analytics/Legend.elm` — `chartLegend` (removes the duplicate in
  `Page.NetWorth` and `Component.Analysis`) and the activity legend helpers
  moved out of `View.Analytics`.
- `src/View/Analytics/Activity.elm` — `activityMonth`, `activityYear`,
  `activityTransactions`; private day-cell/calendar helpers.
- `src/View/Analytics/Activity/Ring.elm` — ring construction, segment/color
  helpers, movement summary, and accessible labels used by `Activity.elm`.
- Add `Ui.Feedback.warningList : Theme -> List String -> Element msg` and use it
  from `Component.Analysis`, `Activity`, and the Investments warnings where the
  message shape permits.

Update call sites:

- `Page.NetWorth`: `View.Analytics.Kpi`, `.Timeline`, `.Legend`, and `.Types`
  for its palette/tone config.
- `Page.Cashflow`: `.Types` for palette/tone config.
- `Page.Investments`: `View.Analytics.Kpi.metricCard`, `Ui.Color.mix`,
  `Lib.Formatting`, `Lib.ListExtra`.
- `Component.Analysis`: `View.Analytics.Activity`, `.Category`, `.Timeline`,
  `.Legend`, `.Types`.
- `tests/AnalysisTest.elm`: `Component.Analysis` and `View.Analytics.Types`.
- `tests/ThemeTest.elm`: `View.Analytics.Palette.flowMovementColors` and
  `View.Analytics.Types.ActivityTone`.

Delete `src/View/Analytics.elm` after all imports move.

Skeleton cleanup in `Ui.Skeleton`:

- Delete unused `metricCard`, `datePair`, and `bar` exports; keep `bar` private
  if the remaining shapes use it.
- Rename `kpiCard` to `summaryCard` and update NetWorth/Investments.
- Keep `summaryCard`, `chartCard`, `listRow`, `inputBar`, and `chipRow`.
- Rationale: `scripts/check-ui-boundary.mjs` intentionally requires raw
  dimensions to live under `Ui/`. Page modules choose which skeleton shapes
  appear; the dimensioned shapes stay in `Ui.Skeleton`. A full move to `View/`
  would require new design tokens and is deferred (decision 3).

`Page.Investments`:

- Add canonical `-- MODEL`, `-- UPDATE`, `-- VIEW` section comments.
- Group submodels/init under MODEL, Msg/update/effects under UPDATE, view
  helpers under VIEW.
- Keep view helpers in the page for now; extracting `View.Investments` is
  deferred unless the page still feels too large after deduplication.

Deferred: extracting `View.Investments`; splitting `Activity.elm` further;
`Cost.costType` ADT.

Verify: standard gate. Manually check KPI cards, chart legends, category donuts,
activity calendars/rings, and selected-period details in both themes at compact
and wide widths.

Commits (recommended):

- `refactor(frontend): centralize formatting and list helpers`
- `refactor(frontend): split analytics types, palette, and charts`
- `refactor(frontend): split analytics category, activity, and legends`
- `refactor(frontend): dedupe KPI and legend rendering`
- `refactor(frontend): prune skeleton primitives`
- `refactor(frontend): organize investments page sections`

### Stage 9 — Type transaction presentation effects

Goal: remove stringly-typed classification/effect rematching.

Change `Data.Transaction`:

```elm
type TransactionEffect
    = TransactionIncome
    | TransactionExpense
    | TransactionTransfer
    | OpeningBalance
    | Borrowing
    | Repayment
    | AssetAcquisition
    | AssetDisposal
    | AssetReallocation
    | AssetTransfer
    | Refinancing
    | DebtForgiveness
    | BorrowingReversal
    | IncomeReversal
    | ExpenseReversal
    | Unclassified
    | Unknown String

type alias Presentation =
    { classification : TransactionEffect
    , effects : List TransactionEffect
    }
```

Expose `effectDecoder : Decoder TransactionEffect`. It must accept every label
currently emitted by the backend (`income`, `expense`, `transfer`,
`opening_balance`, `borrowing`, `repayment`, `asset_acquisition`,
`asset_disposal`, `asset_reallocation`, `asset_transfer`, `refinancing`,
`debt_forgiveness`, `borrowing_reversal`, `income_reversal`,
`expense_reversal`, `unclassified`) and return `Unknown raw` for anything else
so future backend labels do not break the client.

Update:

- `Api.Transaction.presentationDecoder` decodes both fields with
  `Data.Transaction.effectDecoder`.
- `View.Transaction.classificationTone : TransactionEffect -> Ui.Chip.FinancialTone`
  and `effectLabel : TransactionEffect -> String` become total. Preserve the
  current labels; map `Unknown _` to the current fallback label (`"Transfer"`)
  unless the orchestrator prefers showing the raw label.
- `Page.Transactions.trailingSummary` no longer filters the string
  `"unclassified"`; filter `Unclassified`.
- `tests/TransactionsTest.elm` and `tests/AnalysisTest.elm` construct and expect
  `TransactionEffect` values instead of strings.
- Add one focused decoder test for a mixed transaction and one for an unknown
  label fallback.

Deferred: `Cost.costType` ADT; any backend/wire renaming.

Verify: standard gate. The existing 69 tests may change shape but must all pass.

Commit: `refactor(frontend): type transaction presentation effects`

### Stage 10 — Dead-code sweep, check gates, and docs

Goal: finish with no dead exports and make `elm-review` part of the standard
frontend check.

Dead-code sweep:

- `rg` for removed modules and imports; confirm zero hits for `Ui.elm`,
  `State.Viewport`, `Ui.Responsive`, `View.Shell`, `View.Formatting`,
  `View.Analytics`, `Page.Shared`, and `src/RemoteData.elm`.
- Remove unused exports identified by `elm-review`, including `Ui.Chip.badge`
  if the bootstrap removal leaves it unused, `Ui.Skeleton` leftovers, and dead
  `Ui.Typography` helpers (`delimitedSectionTitle`, `navLabel`,
  `listItemTitle`, `truncatedListItemTitle`, `wrappedListItemTitle` if still
  unused).
- Keep `scripts/check-ui-boundary.mjs` strict; verify it no longer special-cases
  `Ui.elm`.

Check gates:

- `package.json`: add `npm run review` to the `check` script (for example after
  the format validation and before/after `elm make`).
- `justfile`: add `check-frontend-review: cd frontend-web && npm run review`
  and include it in `check-frontend`.
- Run the full repository `just check` once at the end, as required by
  `AGENTS.md`.

Docs:

- Update `docs/frontend.md`: tree, dependency graph, module ownership, page
  structure, design system, testing commands, and the "Known debt" list (remove
  the items this plan fixes; keep domain-identifier and browser-e2e debt).
- Update the frontend section of `docs/architecture.md` to show
  `Main -> Chrome / Route / Page`, `Page -> Component`, `Ui.Env`,
  `Ui.Page`, and `Ui.Workspace`.
- Mark this plan's status/outcome at the top of this document.

Verify:

```bash
cd frontend-web
./node_modules/.bin/elm-format src tests --validate
npm run check
npm run review
npm test
npm run build
cd ..
just check
```

Commit:

- `chore(frontend): enforce elm-review and prune dead UI code`
- `docs(frontend): document aligned UI module structure`

## 5. Verification summary

| Stage | Required automated checks |
| --- | --- |
| 1 | format validate, `npm run check`, `npm run review`, `npm test`, `npm run build` |
| 2 | same; plus `rg` facade-content check |
| 3 | same; plus `rg` for `Ui.` facade imports |
| 4 | same; plus `rg` for `State.Viewport`/`Ui.Responsive`/`ThemeMode`; manual resize/theme matrix |
| 5 | same; plus manual single-scroll and split-pane checks at 390/768/1024/1544/1920 |
| 6 | same; plus manual nav/PWA/theme/live-refresh checks and dead-code `rg` |
| 7 | same; plus `rg` for old shared-module paths |
| 8 | same; plus manual chart/legend/calendar checks in both themes |
| 9 | same; decoder tests and updated transaction/analysis tests |
| 10 | full `just check`; `npm run review` now part of `check-frontend` |

Local-binary policy: use `frontend-web/node_modules/.bin` tools (via npm
scripts or direct paths) for every stage. Do not wrap each command in
`nix develop -c`; if a tool is missing, enter one persistent `nix develop` shell
and keep it alive. The full `just check` is intentionally deferred to stage 10.

## 6. Risks and rollback

Risks:

1. **Viewport semantics.** Replacing `State.Viewport` with `Ui.Env` could change
   breakpoints if code switches to `Element.Device` classes. Mitigation: keep
   all existing thresholds in `Ui.Viewport`; use `viewport.width` for layout and
   `device` only for capability decisions. Test 1024 and 1544 explicitly.
2. **Page scroll ownership.** Removing page-local `height fill`/`scrollbarY`
   from analytics and list pages could introduce double scrollbars or lose
   scroll position. Mitigation: migrate one page group at a time, keep the app
   content wrapper as the single-page scroll owner, and give workspace panes
   independent scroll containers. Manual matrix in stage 5.
3. **Large import churn.** Deleting `Ui.elm` touches most view/page modules.
   Mitigation: stage 2 keeps a facade and stage 3 deletes it only after all
   imports move; compile after each commit.
4. **Theme CSS timing.** Moving theme variables from `index.html` to an Elm
   `<style>` node can flash the wrong background before hydration. Mitigation:
   keep the early `data-theme` script, keep a minimal pre-hydration background
   fallback in `main.css`, and verify dark-mode first paint.
5. **Workspace focus/selection.** Split panes mounted with the wrong keys can
   lose scroll offsets or selection. Mitigation: use `Element.Keyed`; keep
   `Region.navigation`/`Region.mainContent` attributes; test narrow-to-wide and
   back navigation.
6. **Transaction ADT compatibility.** Unknown backend labels must not fail
   decoding. Mitigation: `Unknown String` fallback plus a decoder test.
7. **Dead bootstrap removal.** Removing the only `/bootstrap` request is a
   network-behavior change even though no live screen uses it. Mitigation:
   separate commit; revert if a deployment depends on that request.
8. **`Ui.Color.mix` clamping.** The old local helpers clamped inside; Calco's
   `mix` does not. Mitigation: clamp call-site ratios (all current callers
   already do) and keep a test for palette contrast.
9. **`Ui.Skeleton` boundary.** Moving dimensioned skeleton shapes out of `Ui`
   would violate the boundary script unless new tokens are added. Mitigation:
   keep dimensioned shapes in `Ui.Skeleton`, remove dead exports only, and
   revisit after `Ui.DesignTokens` grows.
10. **PWA/service worker.** CSS extraction changes hashed assets; dev
    unregistration already handles development. Verify production build and a
    reload after deploy.

Rollback:

- Every stage is one or more isolated commits; revert in reverse order with
  `git revert <commit>` (do not rewrite shared history).
- The `Ui.elm` facade in stage 2 is the main rollback seam: restoring the
  facade and the previous imports reverts stage 3 without touching the component
  implementations.
- `Ui.Env` lands before page signatures; a shim for `State.Viewport` in the
  first sub-stage makes a partial rollback possible.
- Page scaffold/workspace changes are split by page group; revert only the
  affected group if a layout regression appears.
- Chrome extraction starts as a wrapper over `View.Shell`; the rename happens in
  a separate commit, so either can be reverted independently.
- CSS move is a single commit; reverting restores the `index.html` style block.

## 7. Out of scope

- Access/auth, sessions, login, admin, profile menus. Hedger has no auth in the
  app; this refactor must not introduce Calco's `Access.elm`.
- I18n, translations, language selection, or a `Language` field in `Ui.Env`.
- Store/caching/request deduplication. Hedger keeps `RemoteData` and
  `Data.Paginated` behavior and the existing API request lifecycle.
- Backend changes of any kind: Haskell code, endpoints, DTOs, JSON field names,
  classification/effect wire labels, valuation semantics, or query behavior.
- Product behavior changes: routes, redirects, PWA install/service-worker
  behavior, live revision refresh, page content, or visual redesign.
- New dependencies, Elm version changes, or Calco-only libraries.
- Domain identifier newtypes (account path, commodity, period, date) and the
  `Cost.costType` ADT.
- Browser end-to-end or screenshot test infrastructure; manual smoke checks are
  listed instead.
- Extracting every large page into `View.*` modules; only the analytics split and
  `Page.Investments` sectioning are in scope.

## 8. Decisions needing orchestrator approval

1. **`Ui.Env` includes `viewport` as well as `device`.** This deviates from
   Calco's `{ theme, device, language }` because Hedger's 1544/700/960
   breakpoints and chart widths are width-driven. Recommendation: approve.
2. **Delete the dead bootstrap fetch/UI and `Api.Bootstrap`.** No live screen
   uses the bootstrap payload after the placeholder is removed.
   Recommendation: delete in stage 6; the backend endpoint remains.
3. **Keep dimensioned skeleton shapes in `Ui.Skeleton`.** The UI boundary script
   requires raw dimensions to live under `Ui/`; full extraction to `View/`
   requires new design tokens. Recommendation: remove dead exports and rename
   `kpiCard` to `summaryCard`, defer full extraction.
4. **Place `Formatting` and `RemoteData` under `Lib`, not `Data`.** They are
   generic pure helpers/types, matching Calco's `Lib` role. Recommendation:
   approve.
5. **Add `PaneWidth` to `Ui.Workspace`.** Calco's equal-pane workspace does not
   cover Hedger's fixed Investments navigator or 2:3 master/detail panes.
   Recommendation: approve the small API extension.
6. **Add `Ui.Page.scaffold`.** Calco exposes `pageContainer`/`pageHeader` only;
   Hedger duplicates the header-band + container wrapper in every page.
   Recommendation: add the scaffold as a Hedger-specific composition primitive.
7. **Keep existing `Ui.Button` function names/signatures.** Calco uses a
   `ButtonConfig`/`textButton` API; renaming everything would add churn without
   structural benefit. Recommendation: keep names, split the module.
8. **Enable `elm-review` in `just check-frontend` in stage 10.** The baseline
   has three review errors (two production, one test); they are fixed in stage 1
   so the gate can be added safely at the end. Recommendation: approve.
