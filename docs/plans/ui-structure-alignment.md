# UI Structure Alignment Plan (revised)

Status: implemented and complete (2026-10-23)
Branch: `refactor/ui-structure-alignment`
Scope: `frontend-web/` only
Reference: Calco `frontend-web` Elm frontend

Outcome: all nine stages landed, including the optional Stage 8 transaction
presentation ADT. The aligned module structure, check gates, and documentation
are in place; the plan history below is retained unchanged.

This plan aligns Hedger's Elm UI structure with Calco's module families while
preserving Hedger's products and behavior. Every stage leaves the tree
compiling and the test suite passing. Full `just check` is run only at the end.

Baseline at planning time: format validation, `elm make`, 69 tests, and the Vite
build pass. `elm-review` reports three errors: unused
`View.Analytics.categoriesForDirection` (and its now-unused helper
`activityDirections`), a list-concatenation simplification in
`View.Transaction`, and one in `tests/ThemeTest.elm`.

## 1. Goals and constraints

The audit findings are unchanged: split `Ui.elm`, remove app state from the UI
layer, consolidate page scaffolds/split panes, move shared modules to their
owning layers, split `View.Analytics`, remove duplicated presentation helpers,
unify theme mode, type transaction effects, and make checks/dead code
consistent.

Binding constraints:

- No temporary `Ui.elm` re-export facade. Elm cannot re-export imported values
  or constructors, so each family moves with all of its call sites.
- No duplicate `ContentLayout` shim. Viewport/layout math moves once and all
  users update in the same commit.
- Preserve unknown theme-mode rejection in the persisted-preferences decoder.
- Preserve Investments' current Compact/Medium/Wide behavior and page scroll.
- `Ui.Workspace` owns layout/scroll only; pages choose visible panes.
- Keep the bootstrap fetch/API. Remove only dead bootstrap/placeholder view
  code.
- `View.QueryPanel` does not import `Api.Error`.
- No `Lib -> Data` dependency. Generic number formatting and domain display
  formatting have separate owners.
- Manual dead-export sweep. Do not add `NoUnused.Exports` in this refactor.
- Keep a minimal pre-hydration theme fallback in `index.html`.

## 2. Target structure

```text
frontend-web/src/
  Main.elm                       # composition root
  Chrome.elm                     # app chrome assembly: nav + PWA notices
  Page.elm
  Route.elm                      # parsing/serialization + Route.title
  Preferences.elm
  Pwa.elm
  Ports.elm
  Api/                           # unchanged ownership; Bootstrap kept
  Component/
    Analysis.elm                 # moved from Page/Shared/Analysis
  Data/
    Formatting.elm               # domain/quantity/period display formatting
    Transaction.elm              # owns TransactionEffect
    ...
  Lib/
    Number.elm                   # generic numeric formatting only
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
  main.css
  main.js
```

Delete after their moves: `src/Ui.elm`, `src/Ui/Responsive.elm`,
`src/State/Viewport.elm` (and `src/State/`), `src/View/Shell.elm`,
`src/View/Formatting.elm`, `src/View/Analytics.elm`, `src/RemoteData.elm`,
`src/Page/Shared/`.

Keep `Api/Bootstrap.elm` and `Api.Client.getBootstrap`.

### Dependency rules

```text
Main -> Chrome / Route / Page
Chrome -> View.Chrome / Pwa / Ui / Route
Page -> Component / Data / Api / Ui / View / Lib
Component -> Data / Api types / Ui / View / Lib
View -> Ui / Data / Lib
Api -> Api.Client / Data decoders / Lib.Json
Data -> other Data / Lib
Lib -> no Data, Api, Page, or View
Ui -> Ui.Theme / Ui.DesignTokens / Ui.Common / Ui.Viewport / Data
```

`Data.Formatting` may import `Lib.Number`. `Lib.Number` must not import
`Data`. `View.QueryPanel` is stateless and must not import API modules.

### Deliberate differences from Calco

- `Ui.Env` is `{ theme, device, viewport }`; Hedger has no `Language`.
  Layout uses `Ui.Viewport` because the existing 1544/700/960 thresholds and
  chart widths are width-driven. `device` is retained for capability decisions.
- No `Access`, `Store`, `I18n`, or menu state.
- `Ui.Workspace` supports fixed/weighted panes for Hedger's 2:3 master/detail
  layouts. It does not guess which panes are active.

## 3. `Ui.elm` function-to-module map

Internal helpers move with their caller and stay private.

| Current export | New module |
| --- | --- |
| `FinancialTone(..)`, `StatusTone(..)`, `badge`, `chip`, `financialBadge`, `legendButton`, `legendToggleButton`, `statusBadge`, `wrappingStatusBadge` | `Ui.Chip` |
| `PeriodLabelKind(..)`, `layout`, `pageContainer`, `pageHeader`, `pageHeaderBand`, `periodNavigation` | `Ui.Page` |
| `accentDot`, `card`, `cardWith`, `formSection`, `hairline`, `rowSurfaceAttrs`, `section` | `Ui.Surface` |
| `backButton`, `dangerButton`, `ghostButton`, `iconButton`, `primaryButton`, `secondaryButton` | `Ui.Button` |
| `dateInput`, `fieldLabel`, `searchInputInline`, `segmentedControl`, `segmentedOption`, `wideSegmentedControl` | `Ui.Input` |
| `emptyState`, `errorBanner`, `loading` | `Ui.Feedback` |
| `hoverBackground`, `hoverBorder`, `touchTargetHeight`, `truncatedText` | `Ui.Common` |
| private `buttonBase`, `iconButtonBase` | `Ui.Button` |
| private `inputAttrs`, `inputBaseAttrs`, `segmentedSurface` | `Ui.Input` |
| private `chipAttrs`, `coloredPill`, `legendSurface`, `legendToggleSurface`, `statusToneColors` | `Ui.Chip` |
| private `optionalWithDivider`, `optionalFooter` | `Ui.Surface` |
| private `periodLabelWidth` | `Ui.Page` |

Supporting moves:

| Current | New owner |
| --- | --- |
| `Ui.Typography.uiClass` / `minWidthZero` | `Ui.Common` |
| `Ui.Css.htmlClass` | Delete; use `Ui.Common.uiClass` |
| `Ui.Css.toCssColor` | `Ui.Common.toCssColor` |
| `Ui.Responsive.*`, `State.Viewport.Viewport` | `Ui.Viewport` |
| `View.Analytics` blend/alpha helpers, `Page.Investments.mixColor` | `Ui.Color.mix` / `withAlpha` |
| `View.Analytics`/`Page.Investments` chunk/pad helpers | `Lib.ListExtra` |
| `View.Analytics.decimalFloat` | `Data.Decimal.toFloat` (default zero) |
| `Page.Investments.investmentDecimalFloat` | `Maybe.andThen Data.Decimal.toFloat` (preserve `Nothing`) |
| `Page.Investments.investmentRateFloat` | Keep local `Maybe.map ((*) 100)` |
| `View.Analytics.money`, `moneyFloat`, `axisMoney`, period helpers | `Data.Formatting` |
| `Page.Investments.investmentMoney`, `investmentAxisMoney`, period helper | `Data.Formatting` |
| `View.Formatting` generic float rounding | `Lib.Number` |
| `View.Formatting` quantity/commodity/percentage display | `Data.Formatting` |
| `View.Analytics` palette/tone/calendar/legend types | `View.Analytics.Types` |
| `Page.Shared/Analysis` | `Component/Analysis` |
| `Page.Shared/QueryPanel` | `View/QueryPanel` |
| `RemoteData` | `Lib.RemoteData` |

## 4. Standard stage gate

Run from the repository root unless noted:

```bash
cd frontend-web
./node_modules/.bin/elm-format src tests --validate
npm run check                 # UI boundary + format + elm make + elm-test
npm run review
npm test
npm run build
cd ..
just check-frontend-line-length
```

`npm run check` prepends `frontend-web/node_modules/.bin` to `PATH`. Use local
binaries; enter one persistent `nix develop` shell only if a tool is missing.
Do not wrap every command in `nix develop -c`. Full `just check` is stage 9 only.

## 5. Stages

### Stage 1 — Foundation, viewport ownership, and review cleanup

Goal: move low-level helpers and responsive math once, with all users updated,
and make `elm-review` clean without shims.

Create:

- `Ui/Common.elm`: `uiClass`, `minWidthZero`, `toCssColor`.
- `Ui/Color.elm`: `mix`, `withAlpha`.
- `Ui/Viewport.elm`:
  - `Viewport`, `ContentLayout(..)`;
  - `contentPadding = DesignTokens.space16`;
  - `usableContentWidth` using `Ui.DesignTokens.sidebarWidth`, not literal 232;
  - `contentLayout`, `isCompact`, `usesBottomNavigation`, `usesMasterDetail`.
  - Preserve exact thresholds: bottom nav `< 1544`, content layout `< 700` /
    `< 960` on usable width.

Change all viewport users in the same commit:

- `Page/NetWorth`, `Page/Cashflow`, `Page/Investments`, `Page/Account`,
  `Page/AccountRegister`, `Page/Transactions`, `Page/Shared/Analysis`,
  `View/Shell`, `tests/FoundationTest`:
  `Ui.Responsive.*` -> `Ui.Viewport.*`.
- `Main`, `Page.elm`, `View/Shell`, all pages: `State.Viewport.Viewport` ->
  `Ui.Viewport.Viewport`.
- `Ui.elm`'s two `Responsive.contentPadding` uses -> `Ui.Viewport`.
- Delete `Ui/Responsive.elm` and `State/Viewport.elm`.
- `Ui.Page` is not created yet, so there is no temporary `Ui -> State`
  dependency.

Helper moves and call sites:

- `Ui/Typography.elm`: remove exported `uiClass`/`minWidthZero`; internally
  import `Ui.Common` and use `Common.minWidthZero` / `Common.uiClass` in its
  remaining text helpers so behavior is unchanged.
- `Ui/Skeleton.elm`: import `uiClass` from `Ui.Common`.
- `Ui/Css.elm`: delete `htmlClass`; move `toCssColor` to `Ui.Common`.
  `themeCssVariables` imports `Common.toCssColor`.
- `Ui/Chart/Ring.elm`: `Css.htmlClass` -> `Common.uiClass`;
  `Css.toCssColor` -> `Common.toCssColor`.
- `Ui/Chart.elm`, `Chart/Cartesian.elm`, `Chart/Donut.elm`,
  `Chart/Sparkline.elm`, `Ui/Icon.elm`: `Ui.Css.toCssColor` ->
  `Ui.Common.toCssColor`.
- `Ui/Chart.elm:159` currently uses `Typography.minWidthZero`; change that
  specific use to `Ui.Common.minWidthZero` along with the other call sites.
- `Ui.elm`: `Css.toCssColor` -> `Common.toCssColor`; `Typography.uiClass` ->
  `Common.uiClass`; `Responsive.contentPadding` -> `Ui.Viewport`.
- Every `Typography.uiClass`/`Typography.minWidthZero` call site -> `Ui.Common`:
  `Main`, `View/Shell`, `View/Transaction`, `View/Analytics`,
  `Page/Account`, `Page/AccountRegister`, `Page/Cashflow`,
  `Page/Investments`, `Page/NetWorth`, `Page/Settings`,
  `Page/Transactions`, `Page/Shared/Analysis`.
- `Page/Investments.mixColor` and `View.Analytics.blendColors` -> `Ui.Color.mix`.
  `View.Analytics.withAlpha` -> `Ui.Color.withAlpha`.

Review cleanup:

- Delete both `View.Analytics.categoriesForDirection` **and**
  `View.Analytics.activityDirections`; the latter is only used by the former.
  Remove imports made unused by the deletion (`ActivityDirection`,
  `ActivityMovement`, `DirectionBreakdown` if no longer used).
- Simplify the list concatenation in `View/Transaction.transactionDetail` and
  `tests/ThemeTest.elm`.

Viewport tests (extend `tests/FoundationTest.elm`):

- `contentLayout` across 731/732 (Compact/Medium) and 991/992
  (Medium/Wide).
- `usableContentWidth` at 732/992 (700/960) and `usesMasterDetail` at
  991/992.
- `usesBottomNavigation` at 1543/1544, and `usableContentWidth` at those widths
  to cover the sidebar/bottom-nav transition.
- Height invariance remains covered.
- No property test without separate approval; these are focused examples.

Verify: standard gate, plus `npm run review` must be clean and:

```bash
test ! -e src/Ui/Responsive.elm
test ! -e src/State/Viewport.elm
rg -n 'Ui\.Responsive|State\.Viewport' src tests
```

Expected: no output.

Commits:

- `refactor(frontend): own viewport math in Ui`
- `refactor(frontend): add UI common and color helpers`
- `chore(frontend): clear baseline elm-review errors`

### Stage 2 — Split `Ui.elm` family by family and migrate call sites

Goal: delete `Ui.elm` without a facade. `Ui.elm` shrinks as each family moves;
each family commit updates every call site and compiles.

Order: Common -> Surface -> Button -> Input -> Chip -> Feedback -> Page ->
delete `Ui.elm`.

`Ui.Common` is already created in Stage 1 for `uiClass`, `minWidthZero`, and
`toCssColor`. In Stage 2 it also takes `touchTargetHeight`,
`hoverBackground`, `hoverBorder`, and `truncatedText` out of `Ui.elm`. Remaining
`Ui.elm` internals (`rowSurfaceAttrs`, `buttonBase`, `iconButtonBase`,
`segmentedOption`) import `Ui.Common` until their owning family moves.

When a family moves, any remaining `Ui.elm` definition that calls a moved
helper imports the new module (for example, `Ui.emptyState` uses
`Ui.Surface.card` until Feedback moves). Those internal references disappear
with their owning family. This keeps every family commit compiling without
re-exporting.

Per-family call-site map:

| Family | Update in the same commit |
| --- | --- |
| `Ui.Common` | `View/Shell`; `View/Analytics`; `Page/Investments`; `Page/Account`; `Page/AccountRegister` |
| `Ui.Surface` | `Main`; `View/Transaction`; `View/Analytics`; `Page/Shared/Analysis`; `Page/Shared/QueryPanel`; `Page/NetWorth`; `Page/Cashflow`; `Page/Investments`; `Page/Account`; `Page/AccountRegister`; `Page/Transactions`; `Page/Settings` |
| `Ui.Button` | `View/Shell`; `Page/Shared/QueryPanel`; `Page/NetWorth`; `Page/Cashflow`; `Page/Investments`; `Page/Account`; `Page/AccountRegister`; `Page/Transactions` |
| `Ui.Input` | `Page/Shared/QueryPanel`; `Page/Shared/Analysis`; `Page/Investments`; `Page/Settings`; `Page/AccountRegister` (date inputs) |
| `Ui.Chip` | `Main`; `View/States`; `View/Transaction`; `View/Analytics`; `Page/Shared/QueryPanel.elm:88`; `Page/Investments`; `Page/Account`; `Page/AccountRegister`; `Page/NetWorth`; `Page/Transactions` |
| `Ui.Feedback` | `Main`; `View/States`; `View/Analytics`; `Page/Shared/Analysis`; `Page/Shared/QueryPanel`; `Page/NetWorth`; `Page/Cashflow`; `Page/Investments`; `Page/Account`; `Page/AccountRegister`; `Page/Transactions` |
| `Ui.Page` | `Main`; `Page/NetWorth`; `Page/Cashflow`; `Page/Investments`; `Page/Account`; `Page/AccountRegister`; `Page/Transactions`; `Page/Settings`; `Page/Shared/Analysis` |

Constructor/type call sites (must all move with `Ui.Chip`/`Ui.Page`):

- `Ui.StatusPrimary`, `StatusWarning`, `StatusSuccess`, `StatusDanger` ->
  `Chip.Status*`: `Main`, `Page/Investments`, `Page/AccountRegister`,
  `View/States`, `Page/Account`, `Page/NetWorth`, `Page/Transactions`.
- `Ui.FinancialIncome`, `FinancialExpense`, `FinancialEquity`,
  `FinancialBorrowing`, `FinancialRepayment`, `FinancialAsset`,
  `FinancialNeutral`, `Ui.FinancialTone` -> `Chip.*`: `View/Transaction`.
- `Ui.MonthPeriodLabel`, `Ui.FiscalYearPeriodLabel`, `Ui.PeriodLabelKind` ->
  `Page.*`: `Page/Shared/Analysis`.
- `Ui.StatusTone`/`Ui.FinancialTone` type references move with the same files.

Additional explicit call sites:

- `Main` uses `Ui.emptyState` (Feedback), `Ui.Skeleton.loading` (real
  `Ui.Skeleton` module, not `Ui.elm`), `Ui.badge`/`Ui.statusBadge`/`Ui.Status*`
  (Chip), `Ui.card`/`Ui.errorBanner`/`Ui.loading`, and `Ui.Page` helpers.
  `Ui.Skeleton.loading` stays until stage 5 and is removed with the dead
  bootstrap view.
- `Main` must import `Html` in stage 5 for the theme style node; add it to the
  stage 5 file list, not here.

After the Page family:

- Delete `Ui.elm`.
- Remove the `Ui.elm` special case from `scripts/check-ui-boundary.mjs`.
- Delete never-used `Ui.Chip.wrappingStatusBadge` during the Chip commit or in
  the manual sweep (it has no call sites).
- `rg -n '^import Ui$' src tests` must be empty.
- Keep direct module imports such as `Ui.Theme`, `Ui.Icon`, `Ui.Typography`,
  `Ui.Skeleton`, `Ui.Chart`, `Ui.Calendar`.

Verify: standard gate after each family commit; final `test ! -e src/Ui.elm`.

Commits: one per family, e.g.
`refactor(frontend): extract Ui.Surface` ... `refactor(frontend): delete Ui facade`.

### Stage 3 — `Ui.Env`, theme mode, and page signatures

Goal: remove app state from the UI layer and make `Ui.Theme.Mode` the single
mode type, while preserving persisted-preference validation.

Create `Ui/Env.elm`:

```elm
type alias Env =
    { theme : Theme
    , device : Device
    , viewport : Viewport
    }

withTheme : Env -> Theme -> Env
withViewport : Env -> Viewport -> Env
resize : Env -> Int -> Int -> Env
```

`resize` updates the measured viewport and `Element.classifyDevice`.

Record-update trap: `{ model.env | theme = ... }` is invalid Elm. Use
`Ui.Env.withTheme`, `withViewport`, and `resize` at every update site.

Theme mode:

- Add `Ui.Theme.modeFromStringResult : String -> Result String Mode`; keep
  `modeToString`.
- `Preferences` stores `mode : Ui.Theme.Mode`; wire field remains
  `"themeMode"`.
- Decoder uses `modeFromStringResult` and **fails on unknown values**, matching
  current behavior. Add tests for `"light"`, `"dark"`, and an unknown value.
- Update `tests/FoundationTest.elm`: import `Ui.Theme exposing (Mode(..))`,
  read `preferences.mode`, and expect `Ui.Theme.Dark` in the legacy-preference
  test.
- `Main` flags use `modeFromStringResult |> Result.withDefault Light`.
- Rename `Main.Flags.initialThemeMode` -> `initialMode`,
  `Page.Settings.Model.themeMode` -> `mode`,
  `Page.Settings.Msg.ThemeModeSelected` -> `ModeSelected`, and
  `Page.OutMsg.SetThemeMode` -> `SetTheme`. Update the `main.js` Elm flags key
  to `initialMode`; keep the persisted JSON field `"themeMode"`.
- Delete `Preferences.ThemeMode`, duplicate serializers, and
  `Preferences.theme`.
- `Page.Settings`/`OutMsg` use `Ui.Theme.Mode`.
- `Main.init` builds the initial `Env` with
  `Ui.Theme.forMode initialMode`, `Element.classifyDevice { width =
  flags.viewportWidth, height = flags.viewportHeight }`, and the matching
  `Viewport`. `Main.view` uses `model.env.theme`, never `Preferences.theme`.
- `Main.ConfigLoaded` updates **both** `preferences` and `env.theme` via
  `Ui.Env.withTheme`.
- `Main.applyPageOutMsg` on theme change updates preferences, `env.theme`, and
  `Ports.storeConfig`.

Env threading:

- `Main.Model`: `env : Env` replaces `viewport`; keep the bootstrap state.
- `Page.Context = { route : Route, env : Env }`.
- Update every `Main.pageContext` call site: `Main.init`, `UrlChanged`,
  `JournalChanged`, and `GotPageMsg`; also `Main.view`'s page call.
- `Page.elm` `init`/`update`/`view` pass `context.env`; update all page view
  calls. `Page.view` has eight branches (`AccountValue`, `Accounts`,
  `Transactions`, `AccountRegister`, `NetWorth`, `Cashflow`, `Investments`,
  `Settings`); `Page.init` updates Settings' mode source; `Page.update` passes
  `Env` to Account.
- Page signatures:
  - `Page.NetWorth.view`, `Page.Cashflow.view`, `Page.Investments.view`,
    `Page.Account.view`/`valueView`, `Page.AccountRegister.view`,
    `Page.Transactions.view`, `Page.Settings.view` become `Env -> Model ->
    Element Msg`.
  - `Page.Account.update` receives `Env` (its `UpdateProps` becomes `{ env :
    Env }` or `Env`).
  - `View.Shell.view : Env -> { notice, navigation, page } -> Element msg`.
  - `Page/Shared/Analysis.view : Env -> Config -> Report -> Model ->
    Element Msg`; `controlsView` may stay `Theme`.
- `Page/Shared/Analysis` already uses `Ui.Viewport` after stage 1; verify no
  `Responsive.*` references remain and update its view signature here.
- After this stage, run:

  ```bash
  rg -n '\bThemeMode\b|Preferences\.themeMode|\.themeMode\b' \
    src --glob '*.elm' tests --glob '*.elm'
  rg -ni 'thememode' src/Preferences.elm src/main.js tests
  ```

  The first command must be empty. The second may only find the persisted
  JSON/localStorage key `themeMode`; no Elm type or field.
- `test ! -e src/State/Viewport.elm`, `test ! -e src/Ui/Responsive.elm`.

Verify: standard gate; manual resize at 390/768/1024/1543/1544/1920; theme
toggle/reload; unknown saved theme is rejected.

Commits:

- `refactor(frontend): add Ui.Env`
- `refactor(frontend): preserve theme mode validation`
- `refactor(frontend): thread Ui.Env through pages`
- `refactor(frontend): remove page context duplication`

### Stage 4 — Standard page scaffold and constrained Workspace

Goal: remove duplicated scaffolds and split panes for Account/AccountRegister/
Transactions without changing Investments or compact AccountRegister headers.

`Ui.Page.scaffold`:

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

It renders `pageHeaderBand` + `pageHeader` + `pageContainer body`; it adds no
local scrolling. Single pages scroll through the app content wrapper.

Use it for: NetWorth, Cashflow, Settings, Account, Transactions, and
AccountRegister `detailPage` (standard back/title header). AccountRegister
`listPage` keeps its custom `registerHeader` because the wide-selected master
calls `listPage theme True` and must retain the compact back/refresh header.

Exceptions:

- **AccountRegister list header stays custom.** Its current header is a
  back/refresh row above the title. Keep `registerHeader` for `listPage` in
  both narrow and wide-selected master (`listPage theme True`); only
  `detailPage` uses `scaffold`. Do not silently replace the compact header.
- **Investments stays on its current page wrapper and
  `investmentMasterDetail`.** It keeps compact stack and Medium/Wide
  navigator+history behavior. Do not apply `Ui.Workspace` to Investments.
  It may adopt `scaffold` only in a separate parity follow-up; this plan does
  not change its page-level scroll.

`Ui.Workspace` contract:

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

- Workspace owns layout and scroll only. It never filters or guesses active
  panes.
- Pages pass exactly the panes that should be visible for the current layout:
  one pane on narrow/compact, two panes when `Ui.Viewport.usesMasterDetail` and
  a detail is selected.
- One pane renders in normal page flow (app content scroll); two panes render a
  keyed row with independent `height fill` scroll containers and
  `min-height: 0`.
- Panes dropped on narrow layouts are unmounted, so their scroll offsets are
  not retained. This is intentional and documented in the module comment.
- Keys: `accounts`/`account-detail`, `register`/`register-detail`,
  `transactions`/`transaction-detail`.

Apply Workspace only to:

- `Page.Account` wide selected: master `Portion 2`, detail `Portion 3`.
- `Page.AccountRegister` wide selected: master `Portion 2` keeps
  `listPage theme True` (its custom compact header), detail `Portion 3`.
  `listPage`/`detailPage` lose their local `height fill` and `scrollbarY`;
  Workspace supplies the pane scroll wrapper.
- `Page.Transactions` wide selected: master `Portion 2`, detail
  `Portion 3`.

When a branch adopts `Ui.Page.scaffold` or `Ui.Workspace` (or keeps a custom
header wrapper such as AccountRegister `listPage`), remove the `page-scroll`
class **and** its local `Element.height Element.fill` and `Element.scrollbarY`
attributes from the single-pane branch and from the Workspace pane content.
Single pages scroll through app content; Workspace panes own height and scroll.
Investments is the exception: it keeps its current wrapper and scroll.

`page-scroll` removal checklist (remove only after the page uses scaffold or
Workspace):

- `Page/NetWorth`: dashboard + transaction detail.
- `Page/Cashflow`: dashboard + transaction detail.
- `Page/Settings`: view.
- `Page/Account`: `view`, `valueView`, `accountDetailPage`, `detailForPath`.
- `Page/AccountRegister`: `listPage`, `detailPage`.
- `Page/Transactions`: `listPage`, `detailPage`.
- `Main`: dead placeholder branch in stage 5.
- `Page/Investments`: **keep** `page-scroll` for now; it stays on its current
  wrapper.
- Do not delete the `.page-scroll` selector from `index.html` while
  Investments (or Main's placeholder) still uses it. Remove page-specific
  selectors `.accounts-wide-layout` / `.transactions-wide-layout` when their
  last users move.

Verify: standard gate plus manual matrix 390/768/1024/1544/1920:

- Account/Register/Transactions: one scrollbar on narrow, independent pane
  scroll on wide, selection/back works.
- AccountRegister compact header retains back + refresh row.
- Investments at Medium still shows navigator + history; Compact still stacks;
  page scroll unchanged.
- No double scrollbars.

Commits:

- `refactor(frontend): add Ui.Page scaffold`
- `refactor(frontend): add constrained Ui.Workspace`
- `refactor(frontend): adopt scaffolds in standard pages`
- `refactor(frontend): use Workspace for account and transaction splits`
- `refactor(frontend): remove obsolete page CSS classes`

### Stage 5 — Chrome extraction, dead view cleanup, document CSS

Goal: make `Main` a composition root without deleting the bootstrap API.

Create:

- `Chrome.elm`:

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

  Move `shellNavigation`, `destination`, `routeIsActive`, `shellNotice`,
  `installationNotice`, `shellAction`, `dismissAction`.

- `View/Chrome.elm` via `git mv src/View/Shell.elm`; update the module name and
  keep the existing notice/destination types. It takes `Env`.
- `src/main.css` for document shell, safe-area, app shell, UI motion/skeleton
  classes, and shared CSS.

Change:

- `Route.title` moves from `Main.pageTitle`.
- Keep `Main.Model.bootstrap`, `BootstrapLoaded`, `RefreshBootstrap`,
  `Api.getBootstrap`, and `JournalChanged`'s bootstrap refresh. Keep
  `Api/Bootstrap.elm`.
- Remove only dead view code: `bootstrapStatus`, `bootstrapSummary`,
  `placeholder`, `isRealPage`, the placeholder branch in `pageView`, and
  `Page.PlaceholderPage`.
- Delete `Ui.Skeleton.loading` (only the dead bootstrap view used it).
  `Ui.Chip.badge` becomes unused; remove it in the stage 9 manual sweep.
- `Main.view` prepends the theme style node; add `Html` to `Main`'s imports:

  ```elm
  Html.node "style" []
      [ Html.text (":root { " ++ Ui.Css.themeCssVariables theme ++ " }") ]
  ```

- `main.js` imports `./main.css`.
- `index.html` keeps:
  - the early `data-theme` boot script;
  - a minimal pre-hydration `:root` block with `--ui-page-bg`,
    `--ui-surface`, `--ui-primary`, `--ui-primary-soft`, `--ui-focus-ring`,
    `--ui-card-shadow`;
  - the matching `:root[data-theme="dark"]` overrides.
  Elm injects the full `Ui.Css.themeCssVariables`; the index fallback only
  prevents a wrong first paint.
- Do not remove `.page-scroll` while Investments still uses it.

Verify: standard gate; manual nav/PWA/live-refresh/theme first-paint checks.
Also `test ! -e src/View/Shell.elm` and `rg -n 'isRealPage|PlaceholderPage' src`
should be empty. `Api.Bootstrap` must still exist.

Commits:

- `refactor(frontend): extract Chrome from Main`
- `refactor(frontend): remove dead placeholder view code`
- `refactor(frontend): move document CSS and inject theme variables`

### Stage 6 — Shared modules, query panel, and formatting ownership

Goal: put reusable TEA/views/generic types in their owning layers and remove
`Lib -> Data` coupling.

Moves:

| From | To |
| --- | --- |
| `Page/Shared/Analysis.elm` | `Component/Analysis.elm` |
| `Page/Shared/QueryPanel.elm` | `View/QueryPanel.elm` |
| `RemoteData.elm` | `Lib/RemoteData.elm` |
| `View/Formatting.elm` | split: `Lib/Number.elm` + `Data/Formatting.elm` |

Formatting ownership:

- `Lib/Number.elm` (generic only):
  `roundedString`, `fixedRoundedString`.
- `Data/Formatting.elm` (domain/quantity/period display):
  `commodityDisplaySymbol`, `formatBalance`, `formatFullBalance`,
  `formatFullAmount`, `formatFullValue`, `formatAmountDisplay`, `compactValue`,
  `formatPercentage`, `formatRatePercentage`, `formatDecimalDisplay`,
  `formatMaybeDecimalDisplay`, `formatFloatDisplay`, `formatAxisValue`,
  `formatDecimalAxis`, `shortPeriod`, `shortPeriodWithYear`,
  `periodDisplayName`.
  It may import `Lib.Number`, `Data.Decimal`, `Data.Quantity`, `Data.Calendar`.
- Add `Data.Decimal.toFloat : Decimal -> Float` (default zero) for
  `View.Analytics.decimalFloat`; `Page.Investments.investmentDecimalFloat`
  wraps it in `Maybe.andThen`, preserving `Nothing`.
- Tests: `FormattingTest` imports `Lib.Number` for rounding and
  `Data.Formatting` for display behavior.

Exact formatting replacements (preserve current output):

| Current | Replacement | Behavior |
| --- | --- | --- |
| `View.Analytics.money commodity decimal` | `Data.Formatting.formatDecimalDisplay commodity decimal` | Decimal + symbol + compact suffix |
| `View.Analytics.moneyFloat commodity float` | `Data.Formatting.formatFloatDisplay commodity float` | Float + symbol |
| `View.Analytics.axisMoney commodity float` | `Data.Formatting.formatAxisValue commodity float` | **No symbol**; sign + compact value/suffix; commodity still selects INR L/Cr scale |
| `Page.Investments.investmentMoney commodity maybeDecimal` | `Data.Formatting.formatMaybeDecimalDisplay commodity maybeDecimal` | Symbol; `—` for Nothing |
| `Page.Investments.investmentAxisMoney commodity float` | `Data.Formatting.formatDecimalAxis commodity float` | Symbol included |
| `View.Analytics.shortPeriod` | `Data.Formatting.shortPeriod` | `2026-03` -> `03` |
| `Page.Investments.shortInvestmentPeriod` | `Data.Formatting.shortPeriodWithYear` | `2026-03` -> `03/26` |
| `Page.Investments` `Formatting.formatPercentage` | `Data.Formatting.formatPercentage` | Decimal percentage; same output |
| `Page.Investments` `Formatting.formatRatePercentage` | `Data.Formatting.formatRatePercentage` | Rate decimal -> percentage; same output |
| `View.Analytics.periodName` | `Data.Formatting.periodDisplayName` | Month name for `YYYY-MM`, otherwise raw |
| `View.Analytics.decimalFloat` | `Data.Decimal.toFloat` | Defaults to zero. |
| `Page.Investments.investmentDecimalFloat` | `Maybe.andThen Data.Decimal.toFloat` | Preserves `Nothing` for a missing Decimal. |
| `Page.Investments.investmentRateFloat` | keep local `Maybe.map ((*) 100)` | Preserves `Nothing` and the percentage scaling. |

Direct `Formatting.roundedString` uses move to `Lib.Number.roundedString`:
`View/Analytics.elm:151`, `View/Analytics.elm:568`,
`Page/Investments.elm:569`, and `Page/Investments.elm:1293`. The Kpi and
Category modules import `Lib.Number` for these calls.

`Page/Investments` percentage/rate display moves to `Data.Formatting`:
`formatPercentage` at lines 615 and 1082, and `formatRatePercentage` at lines
1073 and 1283.

`View.QueryPanel`:

- `Config.queryError : Maybe String`.
- `Page.Cashflow`/`Page.Transactions` pass `ApiError.message` (or Nothing) and
  no longer pass `ApiError`.
- `View.QueryPanel` must not import `Api.Error`.

`Component.Analysis.view` takes `Env`; `controlsView` may stay `Theme`.
Update `Page.NetWorth`, `Page.Cashflow`, tests, and all import paths. Update
`Lib.RemoteData` imports in `Main`, `View.States`, `Page.NetWorth`,
`Page.Cashflow`, `Page.Investments`, `Page.Transactions`, and
`tests/FoundationTest`. Delete `Page/Shared/`.

Verify: standard gate plus:

```bash
test ! -e src/Page/Shared
test ! -e src/View/Formatting.elm
test ! -e src/RemoteData.elm
rg -n 'Page\.Shared|View\.Formatting|^import RemoteData' src tests
```

Expected: no output. `rg -n 'Api\.Error' src/View/QueryPanel.elm` must be empty.

Commit: `refactor(frontend): move shared modules and split formatting`

### Stage 7 — Split analytics, dedupe helpers, prune skeleton

Goal: replace `View.Analytics` with focused modules without changing card
appearance.

Create:

- `Lib/ListExtra.elm` with `chunksOf`/`padToMultiple`.
- `View/Analytics/Types.elm`: `Palette(..)`, `ActivityTone(..)`,
  `CalendarDisplay(..)`, `LegendState(..)`, `DirectionSpec`.
- `View/Analytics/Palette.elm`: `paletteColor`, `otherColor`,
  `activityColor`, `activityCategoryColor`, `flowMovementColors`,
  `directionSpecsFor`, and `scaleColor`. `Palette` may import only
  `Ui.Chart.Ring` for segment types; it must never import
  `View.Analytics.Activity.Ring`.
- `View/Analytics/Kpi.elm`: `kpiCard` (NetWorth) and `metricCard`
  (Investments); imports `Lib.Number` for rounded percentage strings.
- `View/Analytics/Timeline.elm`: the two charts and selected-period detail.
- `View/Analytics/Category.elm`: `categoryCard`, `depthControl`; imports
  `Lib.Number` for rounded share percentages.
- `View/Analytics/Legend.elm`: `chartLegend` and activity legends.
- `View/Analytics/Activity.elm`: month/year calendars and transaction lists.
- `View/Analytics/Activity/Ring.elm`: `directionSegments`, `ringAccent`,
  `ringEmptyColor`, `presentDirections`, `monthRingCenter`, and the ring view /
  segment construction. Ring imports `Types` and `Palette`; it does not import
  `Activity`.

Type ownership to avoid cycles:

- `Types` owns the shared presentation types, including `DirectionSpec`.
- `Activity.DayCellConfig` stays private to `Activity.elm`.
- `Legend.ActivityCategoryLegendConfig` stays private to `Legend.elm`.
- `Activity.elm` imports `Ring` and `Legend`; `Ring` imports `Types` and
  `Palette`; neither imports `Activity`.
- `Component.Analysis` imports `Types` (allowed: Component -> View).

KPI parity:

- `kpiCard` (NetWorth): 42px icon tile, `mutedBody` title,
  `kpiValue`/`financeNumber` value, sparkline and comparison.
- `metricCard` (Investments): `compactControlVisualHeight` (36px) icon tile,
  `mutedCaption` title, `kpiValue`/`financeNumber` value, no sparkline or
  comparison.
- Share only a private icon-tile helper parameterized by size. Do not merge the
  two public cards or change typography/tile sizes.

Dedupe:

- `chartLegend` moves to `Legend` and replaces the duplicate in
  `Page.NetWorth` and `Component.Analysis`.
- `Data.Formatting` replaces money/period helpers.
- `Lib.ListExtra` replaces `chunks`/`padToMultiple`.
- `Ui.Color` replaces color blending.
- Add `Ui.Feedback.warningList : Theme -> List String -> Element msg` in
  commit 3 and use it from `Activity`/`Legend`. Keep `View.Analytics.warningList`
  as a temporary alias until commit 4, when `Component.Analysis` and
  `Page.Investments` switch and the alias plus `View.Analytics.elm` are
  deleted.

Skeleton:

- Manual sweep only: remove unused `metricCard`, `datePair`, `loading`, and
  export-only `bar` if no remaining shape uses it.
- Rename `kpiCard` to `summaryCard` and update NetWorth/Investments.
- Keep `summaryCard`, `chartCard`, `listRow`, `inputBar`, `chipRow`; raw
  dimensions stay under `Ui/` because of the boundary script.

`Page.Investments`:

- Add `-- MODEL` / `-- UPDATE` / `-- VIEW`, group model/init, Msg/update/effects,
  and view helpers.
- Keep `investmentMasterDetail` and its page scroll; no Workspace.
- Replace `investmentMetricCard`, money/period/chunk helpers as above.

`View.Analytics.elm` is deleted in commit 4 only. Per-commit call-site table
(each commit compiles):

| Commit | Creates / moves | Call sites updated |
| --- | --- | --- |
| 1. types/palette/flow colors | `Types`, `Palette` | `Page/NetWorth`, `Page/Cashflow`, `Component/Analysis`, `tests/AnalysisTest`, `tests/ThemeTest` |
| 2. KPI/charts/category | `Kpi`, `Timeline`, `Category` | `Page/NetWorth`, `Component/Analysis`, `Page/Investments` if `metricCard` moves |
| 3. activity/legend | `Activity`, `Activity/Ring`, `Legend`; add `Ui.Feedback.warningList`; keep `View.Analytics.warningList` alias | `Component/Analysis` |
| 4. dedupe/delete | remove `View.Analytics.warningList` alias; delete `View.Analytics.elm` | `Component.Analysis`, `Page/NetWorth` (chartLegend), `Page/Investments` (warningList/metricCard) |

Verify: standard gate; manual chart/legend/calendar/KPI checks in both themes at
compact/medium/wide. `test ! -e src/View/Analytics.elm`.

Commit messages:

- `refactor(frontend): split analytics types and palette`
- `refactor(frontend): split analytics charts and category views`
- `refactor(frontend): split activity views and legends`
- `refactor(frontend): dedupe analytics helpers`
- `refactor(frontend): organize Investments page sections`

### Stage 8 — Transaction presentation ADT (late, optional)

Goal: type transaction effects without blocking the structural refactor. If
schedule risk appears, drop this stage and keep the current strings.

`Data.Transaction`:

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

- `effectDecoder` accepts all current labels and returns `Unknown raw` for
  unknown ones.
- `Api.Transaction.presentationDecoder` uses it for both fields.
- `View.Transaction.classificationTone`/`effectLabel` become total;
  `Unknown _` keeps the current fallback label (`"Transfer"`).
- `Page.Transactions.trailingSummary` filters `Unclassified`.
- Update transaction/analysis test fixtures and add one unknown-label decoder
  test.
- Preserve wire labels and backend behavior.

Verify: standard gate.

Commit: `refactor(frontend): type transaction presentation effects`

### Stage 9 — Dead-code sweep, check gates, docs

Manual dead-export sweep (do **not** claim `elm-review` finds exposed unused
exports; `NoUnused.Exports` is not enabled):

- Check removed files with `test ! -e` / `find`; use `rg` only for imports:
  - `test ! -e src/Ui.elm`
  - `test ! -e src/Ui/Responsive.elm`
  - `test ! -e src/State/Viewport.elm`
  - `test ! -e src/View/Shell.elm`
  - `test ! -e src/View/Formatting.elm`
  - `test ! -e src/View/Analytics.elm`
  - `test ! -e src/RemoteData.elm`
  - `test ! -e src/Page/Shared`
  - `rg -n '^import (Ui|RemoteData|View\.Shell|View\.Formatting)$|^import Page\.Shared' src tests`
- Remove now-unused exposed values manually: `Ui.Chip.badge`,
  `Ui.Skeleton` leftovers, and dead `Ui.Typography` helpers
  (`delimitedSectionTitle`, `navLabel`, `listItemTitle`,
  `truncatedListItemTitle`, `wrappedListItemTitle`) if still unused.
- Check `Ui.elm` and `Ui/` are handled by `scripts/check-ui-boundary.mjs`
  without a deleted-file special case.

Gates:

- `package.json`: add `npm run review` to `check`.
- `justfile`: add `check-frontend-review` and include it in `check-frontend`.
- Update `docs/local-development.md` around the frontend task list so
  `just check-frontend` is documented as UI boundary, format, line length,
  elm-review, build, tests.

Docs:

- Update `docs/frontend.md` tree, dependency graph, module ownership, page
  structure, design system, testing commands, and "Known debt". Remove the
  classification/effect Known Debt item only if Stage 8 lands; otherwise keep
  it unchanged.
- Update the frontend section of `docs/architecture.md` for
  `Main -> Chrome / Route / Page`, `Page -> Component`, `Ui.Env`, `Ui.Page`,
  `Ui.Workspace`, `Data.Formatting`, and `Lib.Number`.
- Mark this plan's outcome at the top.

Verify:

```bash
cd frontend-web
./node_modules/.bin/elm-format src tests --validate
npm run check
npm run review
npm test
npm run build
cd ..
just check-frontend-line-length
just check
```

Commits:

- `chore(frontend): enforce elm-review and prune dead UI code`
- `docs(frontend): document aligned UI module structure`

## 6. Verification summary

| Stage | Automated gate | Extra |
| --- | --- | --- |
| 1 | standard | review clean; viewport threshold tests; `test ! -e` viewport/responsive files |
| 2 | standard after each family | constructor call-site audit; `test ! -e src/Ui.elm` |
| 3 | standard | theme decoder unknown test; precise `ThemeMode`/`.themeMode` check plus persisted-key check; `test ! -e` old viewport files |
| 4 | standard | manual 390/768/1024/1544/1920; Investments medium parity; AccountRegister compact header |
| 5 | standard | nav/PWA/live refresh/dark first paint; `Api.Bootstrap` still present |
| 6 | standard | `test ! -e` old shared files; `rg` imports; QueryPanel no `Api.Error` |
| 7 | standard | manual KPI/chart/legend/calendar parity; `test ! -e` old analytics file |
| 8 | standard | transaction/analysis tests; unknown effect decoder |
| 9 | full `just check` | manual dead-export sweep; `test ! -e` + import rg; docs |

## 7. Risks and rollback

Risks:

1. **Viewport math move.** All `ContentLayout` users update in stage 1; no
   duplicate type. Threshold tests cover 731/732, 991/992, 1543/1544.
2. **Ui.elm split.** No facade; each family commit updates all call sites. If a
   family fails, revert that commit only; earlier families stay.
3. **Scroll ownership.** Standard scaffold removes page-local scroll; single
   pages use app content, Workspace panes scroll independently. Investments
   keeps its current wrapper. Manual matrix required.
4. **Workspace contract.** Pages choose panes; Workspace never filters. Panes
   dropped on narrow are unmounted and lose scroll offsets. Test back/selection.
5. **AccountRegister compact header.** Kept custom; scaffold only standard
   headers.
6. **Theme fallback.** Keep minimal `index.html` variables; Elm injects full
   tokens. Verify dark first paint.
7. **Unknown theme values.** Result parser preserves rejection; test it.
8. **Formatting split.** Exact replacement table prevents `axisMoney`/symbol or
   `Maybe Decimal` behavior changes. `Lib.Number` never imports `Data`.
9. **KPI dedupe.** Separate cards preserve tile/typography differences.
10. **Bootstrap.** Fetch/API stays; only dead view code is removed.

Rollback: each stage/family is a separate commit. Revert in reverse order with
`git revert`. Stage 1 is independent; stage 2 family commits are independent;
stage 4 page groups can be reverted individually; Chrome, CSS, formatting split,
and analytics split each have their own commits. Keep Investments' old wrapper
until a separate parity change.

## 8. Out of scope

- Access/auth, i18n, Store/caching, profile/admin/barcode products.
- Backend changes: Haskell, endpoints, DTOs, JSON fields, wire effect labels,
  query/valuation behavior.
- Product behavior changes: routes, PWA/service worker/install, live refresh,
  page content, visual redesign.
- New dependencies, Elm upgrades, `NoUnused.Exports`.
- Domain identifier newtypes and `Cost.costType` ADT.
- Browser e2e/screenshot infrastructure.
- Migrating Investments to `Ui.Page.scaffold` or `Ui.Workspace`; deferred until
  a parity task proves Compact/Medium/Wide and scroll behavior.
- Extracting every large page into `View.*`; only Analytics split and
  Investments sectioning are included.

## 9. Decisions

The orchestrator decisions are incorporated as binding:

- no temporary facade;
- no `ContentLayout` shim;
- preserve unknown theme rejection;
- preserve Investments Medium two-pane behavior and scroll;
- Workspace layout/scroll only, pages choose panes;
- AccountRegister keeps its compact header;
- QueryPanel takes `Maybe String`;
- `Lib.Number` + `Data.Formatting`; no `Lib -> Data`;
- keep bootstrap API, remove dead view only;
- minimal theme fallback in `index.html`;
- manual dead-export sweep;
- transaction ADT late and optional.

No new product decisions are required. Remaining deferred decision: whether to
migrate Investments to scaffold/Workspace in a separate parity task.

## 10. Adversarial review resolutions

| Finding | Resolution |
| --- | --- |
| P0-1 stage 1 review not clean | Stage 1 deletes both `categoriesForDirection` and `activityDirections`; review clean retained. |
| P0-2 helper call sites missed | Stage 1 explicitly updates `Ui/Skeleton`, `Ui/Typography` internals, `Ui/Css.themeCssVariables`, `Ui/Chart/Ring`, `Ui/Icon`, and all `Typography.uiClass/minWidthZero` users. |
| P0-3 facade impossible | Stage 2 has no facade. Families move one commit at a time with all call sites; `Ui.elm` is deleted at the end. Constructor call sites are listed. |
| P0-4 ContentLayout shim | Stage 1 moves `ContentLayout`/viewport math once and updates all pages, `View/Shell`, `Page/Shared/Analysis`, and tests; no duplicate type. |
| P0-5 Investments regression | Investments is excluded from Workspace and keeps its current page wrapper/scroll and Compact/Medium/Wide `investmentMasterDetail`. |
| P1-6 unknown theme rejection | `Ui.Theme.modeFromStringResult`; Preferences decoder fails on unknown; tests added. |
| P1-7 ConfigLoaded | Stage 3 explicitly updates `preferences` and `env.theme` via `Ui.Env.withTheme`. |
| P1-8 Stage 3 call-site map | Stage 2 family map includes Main's `emptyState`, `Skeleton.loading`, `Status*`, and all tone/type constructor sites. |
| P1-9 page calls/pageContext | Stage 3 lists all `Main.pageContext` call sites, `Page.elm` calls, and `Analysis` Env updates. `Responsive.*` uses already moved to `Ui.Viewport` in Stage 1. |
| P1-10 Workspace contract | Workspace owns layout/scroll only; pages pass visible panes; dropped panes unmounted, no scroll retention. |
| P1-11 AccountRegister header | Compact header stays custom; scaffold only standard headers. |
| P1-12 page-scroll | Per-page checklist; Main placeholder removed in stage 5; `.page-scroll` selector retained while Investments uses it. |
| P1-13 QueryPanel | `queryError : Maybe String`; View does not import `Api.Error`. |
| P1-14 formatting layering | `Lib.Number` generic only; `Data.Formatting` domain display; RemoteData in `Lib`; no `Lib -> Data`. |
| P1-15 type ownership | `Types` owns `DirectionSpec`; `DayCellConfig` private to Activity; `ActivityCategoryLegendConfig` private to Legend. |
| P1-16 formatting behavior | Exact replacement table preserves symbol/no-symbol, Decimal/Maybe/Float, and period outputs. |
| P1-17 KPI visuals | Separate `kpiCard`/`metricCard`; tile sizes and typography documented; only private tile helper shared. |
| P1-18 dark first paint | Minimal `:root` and `:root[data-theme="dark"]` fallback kept in index.html; Elm injects full variables; `Html` import added to Main. |
| P2-19 no NoUnused.Exports | Dead-export sweep is explicitly manual; no claim review finds exposed unused values. |
| P2-20 file checks | `test ! -e`/`find` for deleted files; `rg` only for imports. |
| P2-21 theme search | Stage 3 uses a precise `ThemeMode`/`.themeMode` Elm check plus a separate persisted `themeMode` key check. |
| P2-22 line length | `just check-frontend-line-length` is in every standard gate. |
| P2-23 boundary tests | Stage 1 adds 731/732, 991/992, 1543/1544 tests including sidebar/bottom-nav transition. |
| P2-24 sidebar width | `Ui.Viewport` uses `Ui.DesignTokens.sidebarWidth`. |
| P2-25 Ui -> State dependency | `Ui.Viewport` exists before `Ui.Page`; no `Ui -> State` import. |
| P2-26 record update trap | `Ui.Env.withTheme`/`withViewport`/`resize` mandated; `{ model.env | ... }` called out as invalid. |
| P2-27 local development docs | Stage 9 updates `docs/local-development.md` frontend task list for `just check-frontend` contents. |
| R2-1 family order/Common | Stage 2 order is Common -> Surface -> Button -> Input -> Chip -> Feedback -> Page; Common moves `touchTargetHeight`/hover/truncatedText with Shell/Analytics/Investments/Account/AccountRegister call sites and notes remaining `Ui.elm` internals importing it. |
| R2-2 Chart.elm:159 | Stage 1 explicitly changes `Ui/Chart.elm:159` from `Typography.minWidthZero` to `Ui.Common.minWidthZero`. |
| R2-3 Chip QueryPanel | Stage 2 `Ui.Chip` row includes `Page/Shared/QueryPanel.elm:88`. |
| R2-4 FoundationTest mode | Stage 3 imports `Ui.Theme exposing (Mode(..))`, reads `preferences.mode`, and expects `Ui.Theme.Dark` in the legacy test. |
| R2-5 theme search | Stage 3 renames `initialThemeMode`/`themeMode` fields, updates the `main.js` flags key to `initialMode`, and uses a precise old-type check plus a separate persisted-key check. |
| R2-6 decimal split | Formatting table splits `View.Analytics.decimalFloat` (default zero) from `Page.Investments.investmentDecimalFloat` (`Maybe.andThen`, preserves Nothing); `investmentRateFloat` stays `Maybe.map ((*) 100)`. |
| R2-7 direct number calls | Stage 6 lists `roundedString` uses at Analytics 151/568 and Investments 569/1293, and percentage/rate uses; Kpi/Category import `Lib.Number`. |
| R2-8 warningList ordering | Commit 3 adds `Ui.Feedback.warningList` and keeps `View.Analytics.warningList` as an alias; commit 4 switches call sites and deletes the alias/View.Analytics.elm. |
| R2-9 per-commit analytics table | Stage 7 adds the four-commit table with exact call sites and states each commit compiles. |
| R2-10 AccountRegister master | Wide-selected master keeps `listPage theme True` (custom compact header); Workspace supplies scroll and the page loses local scrollbar/height. |
| R2-11 remove scroll attrs | Stage 4 removes `page-scroll`, `height fill`, and `scrollbarY` from migrated single-pane branches (including custom-header AccountRegister) and Workspace pane content, not just the CSS class. |
| R2-12 Palette/Ring split | Palette owns `directionSpecsFor`, `scaleColor`, `activityColor`, `paletteColor`; Ring owns `directionSegments`, `ringAccent`, `ringEmptyColor`, `presentDirections`, `monthRingCenter`; Palette never imports Activity/Ring. |
| R2-13 Main Env init/view | Stage 3 states `Main.init` builds `Env` with `Ui.Theme.forMode` and `Element.classifyDevice`; `Main.view` uses `model.env.theme`. |
| R2-14 conditional Known Debt | Stage 9 removes the classification/effect Known Debt item only if Stage 8 lands; otherwise keeps it. |
