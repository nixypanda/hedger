# Handoff: elm-animator continuity pass

Status: ready for a new agent  
Base branch: `feat/css-affordance-motion`  
Base commit: `7712081` (`Add CSS affordance motions and refusal feedback`)  
Suggested branch: `feat/elm-animator-continuity`

## Read first

- `AGENTS.md`
- `docs/frontend.md`
- `frontend-web/src/Ui/Motion.elm`
- `frontend-web/src/main.css`
- `frontend-web/src/View/Chrome.elm` and `frontend-web/src/Chrome.elm`
- `frontend-web/src/Ui/Notice.elm`
- `frontend-web/src/Ui/Workspace.elm`
- `frontend-web/src/Page/Transactions.elm`
- `frontend-web/src/Page/AccountRegister.elm`
- `frontend-web/src/Component/Analysis.elm`
- `frontend-web/src/Main.elm`

Start from the CSS branch:

```sh
git switch feat/css-affordance-motion
git switch -c feat/elm-animator-continuity
```

If the CSS branch has already been merged, branch from `main` instead. Do not mix the CSS pass and the animator pass in one commit series unless the team explicitly asks.

## What is already done

The CSS-only affordance pass is committed and reviewed. Do not redo or replace it:

- `Ui.Motion` data-attribute hooks: press, busy, disabled, nudge, reveal, chevron, flip, tab.
- Motion tokens, focus-visible ring, press scale, nudge/reveal keyframes, chart hover/focus, donut dimming, spinner.
- Period-boundary refusal state and replayable nudge.
- Load-more blocked state and replayable nudge in Transactions and Account Register.
- Account tree/category/legend reveals.
- Mobile list/detail currently has enter-only CSS reveals; there is no true outgoing-pane animation.

The full frontend gate passes on the base commit: `just check-frontend` with 88 tests.

## Goal

Add `mdgriffith/elm-animator` 2.0 only where CSS cannot express the interaction:

1. **Exit animation before unmount** — notices/toasts dismiss with an exit animation instead of disappearing instantly.
2. **True mobile master/detail push-pop** — the outgoing list and incoming detail are both visible during the transition; back reverses direction.
3. **Optional route/page continuity** — only if the first two are solid. Do not make it a prerequisite.

The animator is for continuity and retained exit state. It is not for press feedback, nudges, chevrons, hover, loading loops, or chart selection; those stay CSS.

## Non-goals

- Do not migrate existing CSS motion to elm-animator.
- Do not animate financial values or KPI numbers.
- Do not morph chart geometry on data changes.
- Do not add ambient/parallax animation.
- Do not change pagination, filtering, or selection semantics.
- Do not use motion as the only signal for disabled/error state.
- Do not put `Animator` directly into `Page.*`/`View.*` modules. The UI boundary and module layering still apply.

## Important constraints

- Elm 0.19.2, elm-ui 1.1.8.
- `mdgriffith/elm-animator` 2.0.0 is the current version. The 1.x `Animate.Element` examples do **not** apply.
- elm-animator 2.0 still generates CSS. Use it only when Elm must own the timeline.
- `frontend-web/scripts/check-ui-boundary.mjs` forbids raw `class`/`style` outside `Ui.*`. Keep animator wrappers and generated CSS inside `Ui.*`.
- `frontend-web/elm-srcs.nix` is consumed by `flake.nix` via `fetchElmDeps`. Adding a dependency requires updating it as well as `elm.json`.
- `Ui/Notice.elm` sets `transform: translateX(-50%)` inline for centering. Do not animate `transform` on the toast itself; animate an inner wrapper or animate opacity only.
- `Ui/Chart/Ring.elm` and `Ui/Chart/Donut.elm` have inline/SVG transforms. Do not apply animator transforms to those nodes.
- `Ui/Workspace.elm` panes are independently scrolling and unmount when removed. Do not animate height or scroll position; animate transform/opacity on wrappers.
- Existing `Ui.Motion` reduced-motion handling is CSS-only. Animator timelines need their own reduced-motion path.

## Recommended architecture

### 1. Dependency and wrapper

Add:

- `mdgriffith/elm-animator` 2.0.0 as a direct dependency.
- Its transitive `mdgriffith/elm-bezier` 2.0.0 to `elm-srcs.nix`.
- `avh4/elm-color` is already present as an indirect dependency.

Create `frontend-web/src/Ui/Animation.elm` as the only Elm module that imports `Animator`/`Animator.Timeline`. Expose a small, typed wrapper, for example:

```elm
type Transition state

init : state -> Transition state
to : { reducedMotion : Bool } -> Duration -> state -> Transition state -> Transition state
update : Time.Posix -> Transition state -> Transition state
isRunning : Transition state -> Bool
current : Transition state -> state
previous : Transition state -> state
arrived : Transition state -> state
arrivedAt : state -> Time.Posix -> Transition state -> Bool
```

Keep the exact names flexible. The important points:

- Pages/views never import `Animator` directly.
- `to` snaps instead of animating when reduced motion is requested (or uses `Animator.ms 0`).
- The wrapper can render an animation for a state or expose the current/previous pair for two-pane rendering.

Do not create a generic “animation framework” unless the two use cases need it. Two small wrappers are fine.

### 2. Time routing from Main

`Main` should own the animation-frame subscription and route ticks to the owning state:

- Add `Main.AnimationTick Time.Posix`.
- Change `subscriptions` from `subscriptions _` to `subscriptions model`.
- Subscribe to `Browser.Events.onAnimationFrame AnimationTick` only while `Pwa.isAnimating model.pwa || Page.isAnimating model.page` (or any active timeline).
- Add `AnimationTick Time.Posix` to `Pwa.Msg` and `Page.Msg`.
- `Main.AnimationTick` calls the PWA update and the current page update with the tick, reusing the existing page/out-context plumbing.
- Each owner exposes `isAnimating` from `Ui.Animation.isRunning`.

This foundation should be done before the notice and pane workers start. It prevents both workers from editing `Main.elm`.

### 3. Reduced-motion flag

Add a `reducedMotion : Bool` flag from `frontend-web/src/main.js`:

```js
const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
```

Thread it through Elm flags. Prefer putting it in `Ui.Env.Env` so views can choose a duration without a second config record. The animator wrapper should snap transitions when it is `True`; do not rely on a global CSS override alone, because the timeline still needs to arrive so retained exiting elements can unmount.

### 4. Notice exit design

Current flow:

- `Chrome.shellNotice` derives a `View.Chrome.Notice msg` from `Pwa.Model`.
- `View.Chrome.noticeView` renders `Ui.Notice.view`.
- Dismissal currently makes the notice `Nothing` immediately.

Recommended shape:

- Add a small PWA-level notice state machine, not view-shaped data:
  - `NoNotice`
  - `NoticeVisible NoticeKind`
  - `NoticeDismissing NoticeKind`
- `NoticeKind` should identify the source (service-worker update, iOS install hint, install prompt), not contain `msg` or view markup.
- Use a `Ui.Animation.Transition` on visibility (or a `Bool` timeline).
- On dismiss, transition `NoticeVisible -> NoticeDismissing` and keep rendering the same notice until the exit timeline arrives.
- On arrival, move to `NoNotice`.
- Repeated dismiss clicks during the exit must be idempotent.
- Reduced motion should go straight to `NoNotice`.

Rendering constraints:

- Keep the fixed-position wrapper and its `translateX(-50%)` centering intact.
- Put the animator wrapper **inside** the fixed toast, or animate opacity only, so the centering transform is not replaced.
- Keep `role`, `aria-live`, and action focus behavior. Do not leave a dismissed notice focusable; apply `inert` to the exiting toast once it is no longer the active notice.
- Test install-prompt dismissal and service-worker update-dismissal separately; both currently share the generic notice path.

Files likely touched: `Pwa.elm`, `Chrome.elm`, `View/Chrome.elm`, `Ui/Notice.elm` (or a new `Ui/NoticeTransition.elm`), `PwaTest.elm`.

### 5. Mobile master/detail push-pop

Current flow:

- `Page.Transactions.view` switches between list, split view, and detail.
- `Page.AccountRegister.view` switches between list and detail.
- `Component.Analysis.Mode` switches between dashboard and transaction detail for Net Worth/Cashflow.
- `Ui.Workspace.view` lays out independently scrolling panes and unmounts dropped panes.

Recommended scope:

- Start with Transactions and Account Register. They are self-contained mobile list/detail flows.
- Use a transition state such as `ListOnly | DetailOnly` (and keep desktop `Split` as a non-animated layout or a small crossfade).
- During the transition, render both panes:
  - outgoing pane gets `inert` and `aria-hidden="true"`, and must not receive focus or scroll changes;
  - incoming pane enters from the right on open and from the left on back;
  - use transform + opacity only, roughly 220ms;
  - do not animate width, height, or `scrollTop`.
- Use `Ui.Workspace` if possible, or a wrapper around it; do not duplicate its scroll-container responsibilities.
- When the timeline arrives, drop the outgoing pane and clear the previous selection state.
- Preserve `Page.Transactions.select`’s visible-row validation and `Page.AccountRegister`’s entry selection.
- Focus management: focus the detail heading or Back button on open; on back, focus the originating row if practical. At minimum, return focus to a stable control and never leave focus inside an inert outgoing pane.
- Reduced motion: swap panes immediately, no transform.

Files likely touched: `Ui/Workspace.elm` (or new `Ui/Transition.elm`), `Page/Transactions.elm`, `Page/AccountRegister.elm`, `Page.elm` (tick forwarding/isAnimating), tests.

The analytics transaction detail (`Component.Analysis.Mode`) is a second phase. Reuse the same wrapper; do not invent a second transition system.

### 6. Optional route transition

Only after notices and panes are stable. A route crossfade needs Main to retain the previous route/page. It is higher risk and lower value than the two core continuations. Keep it out of the first merge unless there is spare time.

## Orchestration plan

Use the same pattern as the CSS pass: one foundation worker, disjoint feature workers, independent reviewers, then the coordinator runs the full gate.

| Step | Owner | Files | Depends on |
|---|---|---|---|
| 0. Preflight | Coordinator | none | Base branch clean |
| 1. Dependency + `Ui.Animation` + Main tick routing + reduced-motion flag | Worker A | `elm.json`, `elm-srcs.nix`, `Ui/Animation.elm`, `Ui/Env.elm`, `Main.elm`, `main.js`, `index.html`, maybe `Pwa.elm`/`Page.elm` tick stubs | none |
| 2. Notice exit | Worker B | `Pwa.elm`, `Chrome.elm`, `View/Chrome.elm`, notice wrapper, `PwaTest.elm` | Step 1 |
| 3. Transactions/Register push-pop | Worker C | `Ui/Workspace.elm` or `Ui/Transition.elm`, `Page/Transactions.elm`, `Page/AccountRegister.elm`, `Page.elm`, tests | Step 1 |
| 4. Analytics transaction detail | Worker D | `Component/Analysis.elm`, `Page/NetWorth.elm`, `Page/Cashflow.elm`, tests | Step 3 (reuse wrapper) |
| 5. Optional route crossfade | Worker E | `Main.elm`, `View/Chrome.elm`, `Ui/Page.elm` | Steps 2–3 stable |
| 6. Integration | Coordinator | all | Steps 1–5 |

Reviewers work in parallel after their workstream:

- Reviewer A: dependency, Nix, `Ui.Animation`, `Main` subscription lifecycle, reduced motion.
- Reviewer B: notice state machine, exit retention, repeated dismiss, focus/live regions.
- Reviewer C: pane layout, scroll, focus, inert outgoing pane, mobile/desktop breakpoints.
- Reviewer D: analytics detail parity and no duplicated transition systems.
- Reviewer E: route transition performance/focus, only if implemented.

## Worker brief template

Use this shape for every implementation subagent:

```text
Work in /Users/nixypanda/Documents/finance/hedger on branch feat/elm-animator-continuity.
Shared working tree: do not create branches, commit, checkout, or edit outside the listed files.

Read: AGENTS.md, docs/frontend.md, docs/plans/elm-animator-handoff.md.

Task: <one focused workstream>.

Files: <exact disjoint list>.

Constraints:
- Use elm-animator 2.0 through Ui.Animation only.
- Do not migrate existing CSS motion.
- Do not change pagination/filter/selection semantics.
- Respect reduced motion; snap when the flag is true.
- Do not animate layout properties or scroll positions.
- Keep the UI boundary: no raw class/style outside Ui.*.
- Add focused example tests where state transitions are testable.
- Do not add property tests without user approval.

Validate:
- elm-format --yes on changed files.
- cd frontend-web && npm run check:ui-boundary
- npm run review
- focused tests if practical.

Do not run the full repo gate; the coordinator will.

Report: changed files, design decisions, tests run, known limitations.
```

## Reviewer brief template

```text
Review only; do not edit files.
Branch feat/elm-animator-continuity, files: <list>.

Check:
- behavior matches the handoff acceptance criteria;
- no duplicated transition system;
- no layout/scroll/focus regressions;
- reduced motion snaps and still unmounts retained elements;
- no elm-animator imports outside Ui.*;
- no regressions to selection/pagination semantics;
- accessibility: aria-live, inert outgoing panes, focus restoration;
- tests/checks claimed by the worker actually pass.

Run read-only checks if useful. Report prioritized concrete issues with file/line refs.
```

## Acceptance criteria

- Notices fade/slide out before unmount; dismissal is idempotent; reduced motion unmounts immediately.
- No notice is left focusable after dismissal.
- Mobile Transactions and Account Register open detail with a push from the right and reverse on back.
- During push/pop, the outgoing pane is inert and does not steal focus or scroll.
- After the transition, the previous pane is unmounted and selection state is correct.
- Desktop split view remains usable; no double scrollbars or width jumps.
- Reduced motion gives an immediate state change with no transform animation.
- `Ui.Animation` is the only module importing elm-animator.
- `frontend-web/elm-srcs.nix` contains the new package hashes and Nix builds pass.
- `just check-frontend` passes.
- If Nix files changed, run `nix build .#hedger-frontend` and reserve `nix flake check` for final CI verification.

## Manual verification matrix

Run the app with `just dev` (backend + frontend).

- Desktop wide: Transactions list -> detail split; back; scroll list; select another row.
- Compact/mobile width: Transactions list -> detail push; back pop; bottom navigation still works.
- Account Register: same open/back flow, including date filters and Load more.
- Net Worth/Cashflow transaction detail if step 4 is implemented.
- PWA update notice: “Later” dismisses with exit; “Update” remains actionable until the route reload.
- PWA install prompt: “Dismiss” exits; repeated clicks during exit do nothing.
- Reduced motion enabled (macOS System Settings or Chrome DevTools rendering emulation): no transform motion; state changes still complete.
- Resize while a transition is running; deep-link/reload; safe-area insets on iOS if available.
- Keyboard: focus does not enter the outgoing pane; Back returns focus sensibly.

## Known gaps that are not this task

These were found during the CSS pass and are intentionally deferred:

- SVG chart hit targets have `tabindex`/`role` but no Enter/Space handler.
- The refresh button uses `inert`, so its busy state is hidden from assistive tech.
- The legend expander has no `aria-expanded`; its text changes between “+N more” and “show less.”
- Activity detail reveal is mount-triggered; it can replay after a data reload.
- `data-ui-tab` is redundant with the pressable transition and may be removed later.
- New refusal/blocked branches have no focused tests yet.

Do not fold these into the animator work unless the user explicitly asks.

## Handoff report format

When returning the work, report:

- branch name and commits;
- dependency/Nix changes;
- exact animation ownership (notices, panes, route);
- tests and checks run;
- manual verification notes;
- known limitations or follow-ups;
- confirmation that existing CSS motion was not migrated or regressed.
