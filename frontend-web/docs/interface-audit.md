# Frontend interface audit — abstractions with bad interfaces

Audit of `src/**/*.elm` (~16k LOC, 65 files) for abstractions whose *interfaces*
invite bugs or friction — not runtime bugs, not formatting. Findings are grouped
by cross-cutting theme first (issues multiple areas share), then by area. Each
finding lists the abstraction, what's wrong, why it bites callers, and a concrete
better interface.

---

## Priority shortlist (fix these five first)

1. **`type Classification` decoded once** (Theme A) — smallest change, kills the most silently-drifting `case` sites.
2. **Opaque `Commodity` / `AccountPath` / `Period`** used in requests + responses (Theme B) — the pervasive root cause; `AccountPath` already exists, just adopt it.
3. **Collapse `Ui.elm` component families**, delete ~16 dead exports, split the module — removes ~15 wrappers and a fake-Device hack.
4. **A uniform `Page` contract** (shared `Context`, uniform `init`/`update`/`OutMsg`, standardize on `RemoteData`) — removes the per-page special-casing in `Page.elm`.
5. **Hide `Analysis`'s `Msg`/`Effect`** and extract the duplicated host chrome — stops NetWorth/Cashflow drifting as copy-paste twins.

---

## Cross-cutting themes

### A. Stringly-typed `classification` enum, re-parsed in ≥3 places
`Data.Transaction.Presentation.classification : String` is a closed enum modeled as a
bare string. It's re-matched with magic strings and a silent `_ -> default` in
`View/Transaction.elm:16` (`classificationTone`) and `:47` (`classificationLabel`),
and again in `Page/Transactions.elm:904`. The two view decoders have **already
drifted** — `classificationLabel` handles `"unclassified"` and defaults to
`"Transfer"`; `classificationTone` has neither. A typo compiles; adding a backend
variant nudges no call site.

**Fix:** one `type Classification` in `Data.Transaction`, decoded once;
`classificationTone`/`classificationLabel` become total. Same treatment for the
other decoded-string enums the codebase *already* handles inconsistently:
`Investments.Warning.kind`, `Transaction.Cost.costType` (`== "total"` checked at
`View/Transaction.elm:211`), and the `"Other"` aggregate-bucket sentinel checked in
~7 spots in `View/Analytics.elm`.

### B. Primitive obsession on domain identifiers — commodity / account / period / date
The single most-repeated smell, flagged by all five slices. Commodity, account path,
period key, and date all travel as raw `String`/`Float` across the entire boundary:
`getAccounts : Maybe String`, `Investments.Request`, `AnalyticsRequest`, every
`Response.displayCommodity`, `Posting.account`, `Route.Account String`, `Analysis`
view args, etc. Nothing stops passing an account where a commodity is expected.
Worst single instance: `View/Formatting.elm:122` `compactValue`'s param is *named*
`commodity` but is actually compared against the display symbol `"₹"` — the correct
usage is invisible and the name misleads.

**Fix:** opaque `Commodity`, `AccountPath`, `Period` newtypes with smart
constructors, used in **both** requests and response records. Note
`Data.Account.AccountPath` already exists (`Data/Account.elm:15`) — but is bypassed
everywhere as raw String (`Route`, `Posting`, `BreakdownItem`), so it buys nothing
today. Adopt it at the boundaries.

### C. Leaked constructors re-introduce illegal states the decoder ruled out
Several opaque-*intended* types export `(..)`, letting callers fabricate
unvalidated values:

- `Data/Date.elm:6` exposes `Date(..)`; `Data/ActivityCalendar.elm:75` builds `Date { ... }` directly, bypassing `validDate`.
- `Data/Time.elm` exposes `Month(..)`; same file destructures it and reconstructs via string round-trips with a `1970-01` silent fallback.
- `Data/Account.elm:29` exposes `AccountNode(..)` (7-field god record, with `directBalances`/`subtreeBalances` duplicated between it and nested `Valuation`).
- `Pwa.elm:1-10` exposes its entire 4×2×2 state machine (`InstallHintState(..)` etc.); `View/Shell.elm:78-105` pattern-matches the internal combinations, so "which banner to show" lives in the view.

**Fix:** keep these opaque, expose smart constructors + accessors + intent queries
(e.g. `Pwa.currentBanner : Model -> Banner`).

---

## Area-specific findings

### `Ui.elm` — a 1583-line grab-bag (structural, high severity)
One module exposes ~90 functions spanning buttons, inputs, cards, chips, badges,
modals, drawers, dropdowns, list rows, layout, and raw attribute helpers. **~16
exports are dead** (never called outside `Ui.elm`): `chips`, `coloredPill`,
`completionDropdown`, `completionRow(WithLeading)`, `optionRow(WithLeading)`,
`dangerIconButton`, `disabledIconButton`, `disabledPrimaryButton`,
`ghostButtonWithLabel`, `inputWithSuffix`, `removableChip(WithLeading)`,
`searchInput`, `selectDropdown`, `suggestionList`.

Sub-patterns:

- **Variant explosion around a hidden good abstraction.** `primaryButton`/`secondaryButton`/`dangerButton`/`ghostButton`/`ghostButtonWithLabel`/`disabledPrimaryButton` are all thin wrappers over private `buttonBase` (`:1412`) — which is already the right config-record shape. Disabled is encoded as a *separate function* instead of `onPress : Maybe msg`. Same for `iconButton`/`dangerIconButton`/`disabledIconButton` — and `disabledIconButton` (`:508`) **fabricates a fake `{ class = Phone, orientation = Portrait }` Device** to satisfy a signature it structurally lacks. Same for four `badge` functions where tone + wrapping should be fields.
- **`x`/`xWith` and `x`/`xWithLeading` pairs** (`card`/`cardWith`, `completionRow(WithLeading)`, `optionRow(WithLeading)`, `removableChip(WithLeading)`, `searchInput`/`searchInputInline`) — the second member only sets an already-`Maybe` field. The name describes the implementation delta, not intent.
- **Leaked elm-ui plumbing:** `cardWith`'s `List (Attribute msg)` escape hatch and `rowSurfaceAttrs : Theme -> Bool -> List (Attribute msg)` (`:1317`, boolean-blind + returns an attribute bag, not a component) defeat the point of the library — callers reach into elm-ui directly.
- **Two segmented-control mental models:** `segmentedControl` (takes pre-built children, caller assembles) vs `wideSegmentedControl` (takes data).

**Fix:** split into `Ui.Button`/`Ui.Input`/`Ui.Card`/`Ui.Badge`/`Ui.Overlay`/`Ui.Layout`/`Ui.Menu`;
collapse each family to one function with a `Variant`/`Tone` union and
`onPress : Maybe msg`; delete dead exports. `Ui.Chart.Cartesian`'s
`Config`/`Series(..)`/`Domain(..)` design is the template.

### Page layer — no uniform contract (high severity)
There is no `Page` abstraction; `Page.elm` hand-wires each page because their
contracts drift gratuitously:

- **`init` shapes disagree:** some return `(Model, Cmd)`, others a bare `Model` needing a separate `beginLoading` + `fetch` dance (`Transactions`, `AccountRegister`, `Account`) with a publicly-exposed `Request` token that exists only to be threaded back.
- **`update` shapes disagree:** four pages return `(Model, Cmd, OutMsg)`, three return `(Model, Cmd)`; `Account.update` uniquely takes an `UpdateProps` record — the sole reason the container's update branches can't be uniform.
- **`view` shapes disagree:** most are `Theme -> Viewport -> Model`; `Settings` is `Theme -> Device -> Model`; `Account` has a *second* entry point `valueView` selected by route (`Page.elm:268`), so render decisions live outside the model.
- **Three incompatible loading representations:** `RemoteData` (NetWorth/Cashflow/Investments) vs a `Paginated` 8-variant `Status` that's *re-projected back* onto `RemoteData` for the view (`Transactions.elm:416`) vs a hand-rolled 6-variant `AccountsState` (`Account.elm:60`) that reimplements `RemoteData`'s stale-carrying semantics.
- **`PlaceholderPage` (`Page.elm:29`) is an unreachable Model state** forcing dead `_ ->` branches.

**Fix:** one `Page` record-of-functions contract with a shared `Context`
(viewport/theme/device) passed to every `update`, `OutMsg` uniformly present,
`init` always `(model, Cmd)`, `Request` opaque; standardize on `RemoteData`.

### `Page.Shared.Analysis` — fictional encapsulation + copy-paste twins (high severity)
Exposes a bespoke 9-function protocol plus `Effect(..)` and `Msg(..)`, so hosts must
interpret effects into `Cmd`s, hand-classify them with `isReportEffect` to decide
loading state, and construct/match Analysis's own `Msg` constructors. Consequence:
`performEffect`/`isReportEffect`/`analysisContext`/`responsiveGrid`/`chartLegend`/`transactionView`
are near-verbatim duplicated between `NetWorth.elm` and `Cashflow.elm` (and
`chartLegend` a third time inside Analysis). `Config` is also passed twice — via
`Context.config` to `update` and again as a bare arg to `view` — so the two can
desync.

**Fix:** hide `Msg`/`Effect`; expose `init/update/view/subscriptions` + `setQuery`
and an opaque `OutMsg`; let Analysis own its report `RemoteData`; fold the shared
host chrome into one module.

### Api facade — inconsistent request-type modeling
No predictable rule for callers: `getInvestments` takes a named `Investments.Request`
(good), `getTransactions` an anonymous `{query, cursor}`, `getAccountRegister` an
anonymous **5-field all-String record duplicated verbatim** in signature and URL
builder (`Client.elm:88`), `getNetWorth`/`getCashflow` share one
`Analytics.AnalyticsRequest` (so a request shaped for one screen is structurally
valid for the other), `getAccounts` a bare `Maybe String`. `AnalyticsRequest` also
has type-identical `positivePeriod`/`negativePeriod` and `positiveDepth`/`negativeDepth`
twins (trivially swappable). `Error.code : String` is a stringly-typed enum matched
against a hardcoded magic-string list (`isQueryError`, `Error.elm:89`).
`transactionsUrl` is exported with zero external callers while sibling URL builders
are correctly private. `Analytics.Report`/`Timeline` (view-models, assembled in
pages) live in an `Api` module.

**Fix:** every endpoint takes `<Module>.Request`, returns `<Module>.Response`; split
`Analytics` into `Api.NetWorth`/`Cashflow`/`Activity`; a `Pane` sub-record for the
twin fields; an `Error.Code` union. (The `toMsg` callback convention *is* consistent
— leave it.)

### Positional-argument blindness (bug-inviting, medium)
- `Ui/Chart.elm:191` `roundedBar : Color -> Float -> Float -> Float -> Float -> Float -> Svg` — **five consecutive Floats**; `layout` takes four bare Ints; `hitTarget` two adjacent Strings. This is an internal toolkit published as public API and only consumed by `Chart.Cartesian`.
- `View/Analytics.elm` private helpers: `dayCell` (**10 positional args**), `activityMonthRing` (8), `categoryRow`/`activityDayRing`/`activityCategoryLegend` (7). Same-typed values (`day`/index, multiple Bools) transpose silently. The *public* Analytics functions use config records — the internals abandon that discipline.
- `Ui/Chart/Donut.elm` selects/colors by `Int` list index (reorder = wrong slice) while `Cartesian` uses a stable `key : item -> String`.

**Fix:** record configs; give Donut Cartesian's key-based contract.

### Boolean blindness & unit confusion (medium)
- `Pwa.fromFlags : Bool -> Bool -> Model` called as `fromFlags flags.isStandalone flags.isIos` (`Main.elm:84`) — swap type-checks, silently mis-detects platform. → record arg.
- `kpiCard`'s `decreaseIsPositive : Bool` → `type ChangeDirection = HigherIsBetter | LowerIsBetter`.
- `Formatting.formatPercentage` vs `formatRatePercentage` differ only by a hidden `1` vs `100` multiplier on the same input type — picking wrong is a silent 100× error. → `Ratio`/`Percentage` types.
- `Revision.ensureMatching : Maybe String -> Maybe String -> ...` — two swappable same-type args (`Data/Revision.elm:10`).

### Smaller / lower-severity
- `RemoteData.value` — weak name for "freshest data (current or stale)"; rename `latest`. (Union design itself is sound.)
- `Preferences.version : Int` exposed but always forced to `1` — representable illegal states; make `Preferences` opaque, handle schema version inside `encode`/`decoder`.
- `Data.Quantity` exposes the record alias *and* redundant `commodity`/`quantity` accessors — pick one.
- `Data.Decimal` exposes only `toString`/`fromString`, forcing a lossy `toString >> String.toFloat >> withDefault 0` round-trip duplicated across `View/Formatting` and `View/Analytics`; add `Decimal.toFloat`.
- `Lib.Time` operates on `"YYYY-MM"` strings with a buried `String.left 7` magic (`groupByKey`); `Lib.Text.formatOptionalFloat` does no formatting despite the name.
- `Main.bootstrapStatus` (`:294`) duplicates the `Freshness`→badge mapping that `View.States.freshnessBanner` already owns; `Main.isRealPage _ = True` is vestigial.
- `Freshness` and `Completeness` share `Partial`/`Unavailable` variant names — confusable at call sites.

---

## Noted as already good (baseline for the fixes above)
- `Data.Paginated` — clean opaque state machine.
- `Data.Decimal`, `Data.AccountType`, `Data.Status`, `Data.Granularity` — proper opaque/enum types with decoders.
- `Ui.Chart.Cartesian` — `Config`/`Series(..)`/`Domain(..)` variant+record API; the template for the rest of the chart layer.
- `Ui.Skeleton`, `Ui.DesignTokens`, `Ui.Responsive`, `Ui.Chart.Sparkline` — small, cohesive, single-purpose.
- `QueryPanel.Config` — clean msg-generic record interface (aside from the raw `baseId : String`).
- The Api `toMsg` callback convention and `expectApiJson`/`responseToResult` error plumbing.
