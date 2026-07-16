# Phase 3 eLedger porting checklist

The Phase 3 source baseline is eLedger commit
`5fc65471d629a6ba4972880d188a2b3cc2b22956`. The source worktree had unrelated
local changes when the port began; Hedger only used the committed foundation
files listed below.

## Copied with minimal changes

| eLedger source | Hedger destination | Notes |
| --- | --- | --- |
| `src/Ui.elm` | `frontend-web/src/Ui.elm` | Reusable components and state primitives. |
| `src/Ui/Css.elm` | `frontend-web/src/Ui/Css.elm` | CSS-class helpers. |
| `src/Ui/DesignTokens.elm` | `frontend-web/src/Ui/DesignTokens.elm` | Spacing and radius tokens. |
| `src/Ui/Icon.elm` | `frontend-web/src/Ui/Icon.elm` | Shared SVG icon set. |
| `src/Ui/Theme.elm` | `frontend-web/src/Ui/Theme.elm` | Light and dark semantic themes. |
| `src/Ui/Typography.elm` | `frontend-web/src/Ui/Typography.elm` | Typography hierarchy and wrapping. |
| `src/Lib/Text.elm` | `frontend-web/src/Lib/Text.elm` | Generic text helpers used by `Ui`. |
| `review/*` | `frontend-web/review/*` | Elm review configuration. |

## Adapted for Hedger

| eLedger source/concept | Hedger destination | Adaptation |
| --- | --- | --- |
| `src/Route.elm` | `frontend-web/src/Route.elm` | Hedger placeholder routes; no hledger account wire type. |
| `src/View/Shell.elm` | `frontend-web/src/View/Shell.elm` | Hedger identity and placeholder navigation. |
| `src/View/Charts.elm` | `frontend-web/src/View/Charts.elm` | Reduced to a generic prepared-series primitive; no accounting calculations. |
| `src/State/Pwa.elm` | `frontend-web/src/Pwa.elm` | Hedger ports and install/update copy. |
| `src/main.js` | `frontend-web/src/main.js` | Hedger-only storage/cache names and no eLedger business cache. |
| `public/sw.js` | `frontend-web/public/sw.js` | Hedger cache identity and explicit `/api/*` bypass. |
| PWA manifest | `frontend-web/public/manifest.webmanifest` | Hedger name, description, start route, and icon. |

## Hedger-owned foundation

- `Api.*` decodes bootstrap data, response metadata, and stable error envelopes.
- `Data.Decimal` keeps API decimal quantities as validated strings.
- `RemoteData` retains stale values while refreshing or after a refresh failure.
- `Preferences` versions and migrates browser-local settings.
- `Api.Revision` rejects mixed known journal revisions.
- `/review` is the component and visual-fixture gallery for Phase 3.
