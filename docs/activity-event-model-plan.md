# Activity event model and product behavior plan

_Branch: `plan/activity-event-model` · 2026-09-27 · Status: backend implemented; Elm migration remains a separate step_

## Outcome

Keep the four Activity views (asset, liability, income, expense), while making
the movements within each view explicit and impossible to mix up in Haskell and
Elm. A journal transaction may contribute to several views. The app will also
recognize transfers, debt refinancing and forgiveness, and income/expense
reversals without presenting them as unrelated increases or decreases.

This plan covers Activity and the transaction labels that explain its results.
The existing net-worth and cashflow calculations continue to use their current
domain modules. Their numbers should only change if an independently identified
accounting bug is fixed; the Activity work is not a rewrite of those reports.

## Product contract

| View | Movement | Example | User-visible result |
| --- | --- | --- | --- |
| Assets | Acquire | Buy a fund with bank cash | Fund acquisition; cash funding does not become a disposal |
| Assets | Dispose | Sell a fund for bank cash | Fund disposal; cash proceeds do not become an acquisition |
| Assets | Transfer | Move money between bank accounts, or exchange holdings | One transfer with named source and destination; show its value once |
| Liabilities | Borrow | Draw a loan or charge a credit card | Borrowed principal, even if the same transaction has an expense or acquisition |
| Liabilities | Repay | Pay loan principal | Repaid principal; an interest posting is separately an expense |
| Liabilities | Refinance | Replace an old loan with a new loan | One linked refinance, showing old principal repaid and new principal borrowed |
| Liabilities | Forgive | Lender cancels debt | Debt reduction labelled forgiven, never labelled paid off |
| Income | Post | Salary receipt, accrued interest, realised gain | Income from income/gain postings, whether or not cash moved |
| Income | Reverse | Correct or reverse an income posting | Deduct from income and identify the reversal |
| Expenses | Post | Groceries paid with cash or credit | Expense from expense postings, regardless of funding |
| Expenses | Reverse | Merchant refund or expense correction | Deduct from expenses and identify the refund/reversal |

Opening balances, equity-only movements, valuation changes without postings,
and unrecognized or ambiguous transactions are outside Activity. They remain
visible through the appropriate balance, performance, or transaction view.
Unknown movements must not be silently called transfers. A transaction that has
both an activity and an otherwise excluded effect still contributes its valid
activity (for example, an asset purchase funded by a liability).

The four views are **projections**, not four mutually exclusive transaction
classes. Examples:

- Buying a fund using a loan produces an asset acquisition and a borrowing.
- Selling a fund at a gain produces a disposal and income from the gain posting.
- Paying a loan with interest produces a repayment and an interest expense.
- Refinancing a loan produces one linked refinance with repayment and borrowing
  legs. It does not add a third amount to either total.
- A credit-card purchase produces an expense and an increase in card debt.
- A cash-to-cash transfer produces one asset transfer; it changes neither total
  assets nor net worth.

### Amount and display rules

1. Show posted income/expenses, reversals, and net (`posted - reversed`)
   separately. A refund does not become positive income. If posted and reversed
   amounts cancel, the day/month still has activity and its transactions remain
   selectable.
2. Show asset acquisition, disposal, and transfer as distinct amounts. A
   transfer has source and destination account breakdowns; its total is counted
   once, not once per endpoint. Cash-to-cash transfers appear here, so rename
   the current "Asset allocation" Activity title to "Asset activity". The
   net-worth allocation breakdown elsewhere retains its current meaning.
3. Show borrowed, repaid, and forgiven principal separately. For a refinance,
   include the two legs in borrowed and repaid totals, and show a separate
   **linked refinance explanation**, not an additive third total. Also show
   `net debt movement = borrowed - repaid - forgiven` where useful.
4. Ring/calendars must use nonnegative segment magnitudes and label each segment
   with its meaning. Income/expense rings split posted from reversed; display
   net totals as signed numbers. Empty states depend on the presence of events,
   not just a zero net value. Tooltips, legends, and accessible labels expose
   the same distinction without relying on color alone.
5. Account breakdowns retain account identity and display depth. Asset transfer
   endpoints are labelled `From` and `To`; they must not be summed together as
   a category total. A category that contains both posted and reversed flows
   displays both contributions and its signed net.
6. A transaction drill-down still uses the complete transaction DTO with all
   postings. Its presentation shows all recognized effects, rather than a
   single dominant label. Existing transaction selection by object is retained;
   no Hedger-specific transaction identifier or summary/detail endpoint is
   added.

### Boundaries and non-goals

- The app does not edit journals, infer tax treatment, or infer a debt's legal
  status. `Forgiven` means the journal has a debt reduction without repayment;
  any related income appears only if the journal has an income/gain posting.
- A price change without a posting belongs in valuation/performance, not
  Activity. A posted adjustment that cannot be distinguished from a transfer
  or forgiveness is reported as unresolved; do not guess.
- Opening balances remain excluded from Activity, even if their postings look
  like acquisitions or borrowing.
- The main net-worth and cashflow timelines are not recalculated from the new
  Activity model. Their existing reconciliation remains a verification target.

## Current implementation and gaps

| Location | Current behavior | Required change |
| --- | --- | --- |
| `Domain/Internal/Transaction.hs` | One dominant `TransactionClassification` per transaction; mixed and liability-to-liability cases collapse to `TransactionTransfer` or `Unclassified` | Keep a suitable summary for legacy uses if needed, but derive Activity from posting-level effects; do not use the single label as the Activity gate |
| `Domain/Activity.hs` | Independent `ActivityKind`, generic `Increase/Reallocate/Decrease`, and every bucket contains all direction fields | Introduce kind-specific event and aggregate types; derive totals and categories from valid events |
| `Domain/Activity.hs` | Asset calculation recognizes noncash asset buy/sell/reallocation; ordinary cash-to-cash transfer is omitted | Detect and value cash transfers; include source and destination |
| `Domain/Activity.hs` | Debt borrow/repay based on one classification; liability-to-liability movements excluded | Detect borrow, repay, refinance, and forgiveness from liability legs and funding/counterpart context |
| `Domain/Activity.hs` | Income/expense posting totals may be negative; flow direction is `Nothing` | Preserve posted and reversed legs instead of netting before projection |
| `Application/Analytics.hs` | Same `ActivityBucket` record for all kinds; asset/liability ignore Activity query | Project typed buckets; make query behavior explicit and consistent with the selected product semantics |
| `Interface/HTTP/Endpoints/Analytics.hs` | Generic Activity JSON bucket (`total`, `increase`, `decrease`, `reallocated`, `directions`) | Emit a tagged, kind-specific Activity response with named movement fields and linked effects |
| `Interface/HTTP/Endpoints/Transactions.hs` | `presentation.classification` is one string | Expose the full set of recognized activity labels plus a fallback for non-Activity entries |
| `Data/Analytics.elm`, `Api/Analytics.elm` | Generic bucket and direction decoder accept invalid kind/direction combinations | Decode the tagged union into four Elm variants; reject mismatched shapes |
| `View/Analytics.elm`, `Data/ActivityCalendar.elm` | Direction labels are selected by tone; rings use absolute magnitudes and a generic empty bucket | Render each variant and its honest signed/gross values; construct a typed empty bucket for the requested kind |
| `Data/Transaction.elm`, `Api/Transaction.elm` | Presentation is one classification string | Model and decode a structured list of effects |
| `View/Transaction.elm`, `Page/Transactions.elm` | One classification badge | Render multiple effect labels and clear fallback labels |

There is a pre-existing query asymmetry: income and expense Activity use the
user's transaction query, while asset and liability Activity read all snapshot
transactions. In this work, apply the same query to all four views so the
selected filter has one meaning throughout the analysis page. This is a
product-visible change for asset and liability calendars and should be covered
by an endpoint test.

## Proposed domain model

The exact names can be refined during implementation, but the shape and
guarantees below are required. Keep hledger transactions and account names as
the ledger vocabulary; small opaque types only protect actual invariants.

```haskell
data PositiveAmount       -- constructor private; amount > 0
data AssetAccount         -- constructor private; inferred asset or cash
data NonCashAssetAccount  -- constructor private; noncash asset
data LiabilityAccount     -- constructor private; liability
data IncomeAccount        -- constructor private; revenue or gain
data ExpenseAccount       -- constructor private; expense

data FlowChange = Posted PositiveAmount | Reversed PositiveAmount

data AssetMovement
    = Acquired NonCashAssetAccount PositiveAmount
    | Disposed NonCashAssetAccount PositiveAmount
    | Transferred AssetAccount AssetAccount PositiveAmount

data LiabilityMovement
    = Borrowed LiabilityAccount PositiveAmount
    | Repaid LiabilityAccount PositiveAmount
    | Refinanced LiabilityAccount PositiveAmount
                 LiabilityAccount PositiveAmount
    | Forgiven LiabilityAccount PositiveAmount

data Activity
    = AssetActivity AssetMovement
    | LiabilityActivity LiabilityMovement
    | IncomeActivity IncomeAccount FlowChange
    | ExpenseActivity ExpenseAccount FlowChange
```

Do not export transfer/refinance constructors without a checked construction
path. The path must reject identical source and destination accounts. An
unambiguous one-to-one transfer/refinance may be represented as a linked event;
multiple candidate sources/destinations must be represented as separate known
legs with an unresolved-link condition, rather than an invented pairing.

Use a result equivalent to
`classifyTransaction :: Snapshot -> Transaction -> ClassificationResult`,
where `ClassificationResult` contains a list of valid activity events and a
structured list of unresolved conditions. A transaction may have zero events.
No domain constructor contains display prose, `SnapshotMetadata`, HTTP data,
or an Elm-shaped bucket. Use a pure classification function for account/sign
relationships and a pure valuation/projector for display-commodity amounts;
the application only resolves inputs and assembles read models.

Activity report results are a sum (`AssetFigures | LiabilityFigures |
IncomeFigures | ExpenseFigures`) with distinct bucket payloads. A flow bucket
has posted/reversed/net data; an asset bucket has acquisitions/disposals/
transfers and transfer endpoints; a liability bucket has borrowed/repaid/
forgiven totals and linked refinances. Do not store a kind tag independently
from an unrestricted payload. Derive all additive totals from event legs in one
place; never set a `total` and its component fields independently.

### Classification rules to implement and review against journal fixtures

1. Infer account groups using `Account.accountInfo` and hledger's declared
   account types, with the existing name fallback where necessary. Decide the
   cash/noncash boundary once. Preserve hledger's `Gain` as income.
2. Identify flow postings by account group and sign: normal income is a credit,
   its reversal a debit; normal expense is a debit, its reversal a credit.
   Work at posting level so opposite signs in one transaction do not cancel
   before classification.
3. Treat cash as a funding/settlement leg when paired with income, expense,
   asset acquisition/disposal, or liability borrowing/repayment. Do not report
   it as an extra acquisition/disposal. A cash debit and cash credit between
   distinct cash accounts is a transfer.
4. Treat noncash asset increases/decreases as acquisitions/disposals when
   funded/settled by cash, income/equity, or liability, and treat matched
   noncash-to-noncash pairs as transfers. Restrict pairing to an unambiguous
   source and destination. Keep separately posted fees/expenses as flows.
5. Treat liability credits as borrowing and debits as debt reductions. A
   liability debit funded by cash is a repayment. A liability debit paired
   directly with an income/gain posting and no cash settlement is forgiveness.
   A debit and credit of distinct liabilities in one transaction form a
   refinance if pairing is unambiguous. Ordinary charge reversals/credit-card
   refunds need an explicit rule so they are not mislabelled loan repayments or
   forgiveness; use the expense reversal posting where present.
6. Apply opening-balance detection before emitting events. Do not classify
   unknown, multi-source, or non-balancing situations by a blanket fallback.
   Produce a structured unresolved condition that the HTTP layer can explain.
7. Convert all legs at the transaction date using the current valuation
   mechanism. If conversion is missing, keep the event out of numeric totals,
   retain the affected transaction and structured warning, and avoid a partial
   transfer/refinance in which only one endpoint was converted. If source and
   destination values differ because of prices, fees, or commodities, retain
   both values; do not manufacture a gain, acquisition, or disposal from the
   difference. Specify the chart's transfer volume consistently (source value).
8. Group by existing month/fiscal-year keys and display depth. Deduplicate
   transaction objects within a bucket while preserving newest-first order.
   A transaction with two effects appears once in the drill-down list.

Classification is accounting interpretation, not purely a type-level task.
The opaque types prevent nonsensical *represented results*; fixtures and
focused tests establish whether the parser chooses the right result. If the
journal does not provide enough information, preserve uncertainty explicitly.

## Application, HTTP, and frontend contract

Keep `POST /api/v1/activity/query` and the four `kind` request values. Change
its response to a tagged union: the top-level `kind` determines one concrete
bucket shape. Each bucket keeps `key`, `startDate`, account breakdowns,
transactions, and metadata context; its movement fields are kind-specific.
Use decimal strings as today. Example shape (illustrative field names):

```json
{
  "kind": "liability",
  "buckets": [
    {
      "key": "2026-09-27",
      "borrowed": "40000",
      "repaid": "40000",
      "forgiven": "0",
      "netDebtMovement": "0",
      "refinances": [
        { "from": "liabilities:old-loan", "to": "liabilities:new-loan",
          "repaid": "40000", "borrowed": "40000" }
      ],
      "transactions": ["complete transaction DTOs, not identifiers"]
    }
  ]
}
```

The real schema must use actual transaction JSON objects and include existing
metadata, range, period, commodity, warnings, and fiscal start. Define all four
response shapes in one contract table before coding. Replace generic Activity
fields instead of publishing both old and new independently settable totals.
The backend and Elm frontend ship together, so migrate both in one change; if
there are external API consumers, document the response break in release notes.

For transaction list/account-register/detail DTOs, replace or augment the
single `presentation.classification` with a structured list of recognized
effects. Derive the list from the same pure classifier used by Activity so its
labels cannot contradict the calendars. Preserve a clear opening/unknown/
equity fallback. No numeric conversion is needed just to label an effect.
Update the transaction JSON decoder and the existing transaction-page fixtures;
the backend mapper is shared by the Activity response and transaction list.

Elm `Data.Analytics` uses a custom type for the four response/bucket variants,
and `Api.Analytics` decodes by top-level kind. Build a separate view function
for each bucket variant behind the shared month/year layout. The year filler in
`Data.ActivityCalendar` must accept the selected kind and create the matching
empty bucket. Flow calendars show posted, reversed, and net; asset and liability
calendars show named movements and linked event details. Transaction views show
multiple effect labels with readable text and accessible presentation.

## Delivery sequence

1. **Lock examples and contract.** Write representative journal fixtures for
   every row of the product table, including mixed transactions, fees, partial
   repayment, different refinance amounts, reversals, opening balances,
   ambiguous pairings, missing conversion, and declared/inferred account
   types. Record the expected events and four-view projections. Settle exact
   endpoint field names and empty-state behavior.
2. **Introduce the pure domain model.** Add opaque amount/account values,
   movement sums, smart constructors, and a structured unresolved result.
   Update the Cabal module list if `Domain.Internal.Activity` is introduced.
   Keep validation at construction boundaries and calculations pure.
3. **Classify transaction effects.** Extract reusable posting-level logic
   under `Domain.Internal.*`, share it between Activity and transaction
   presentation, and implement safe matching for transfers/refinancing. Remove
   Activity's dependency on the one-label `classificationFor` gate.
4. **Calculate typed reports.** Convert and bucket events; derive totals and
   category views, handle missing conversion and zero-net reversals, and ensure
   transaction deduplication. Keep UI grouping such as top-N/Other in
   `Application.Analytics`, not in the domain.
5. **Migrate application/API together.** Apply query filters to all four kinds,
   construct the tagged HTTP response, update transaction presentation, and
   replace generic DTO mapping. Keep complete posting DTOs in lists.
6. **Migrate Elm.** Decode kind-specific payloads, update empty buckets, rings,
   legends, selected-period details, labels, transaction badges, responsive
   layout and accessible names. Rename the Activity title on the net-worth
   page to "Asset activity".
7. **Verify and document.** Update backend/frontend architecture and feature
   docs, add a product-facing note about what changed in calendars and API,
   and check representative journals against expected totals and transaction
   lists.

## Verification and acceptance

Focused backend examples/regressions belong in `Domain/ActivitySpec.hs` and
`Domain/TransactionSpec.hs`; API boundary cases belong in the selective
HTTP endpoint tests. Elm decoder and calendar/view logic belong in
`frontend-web/tests/ActivityCalendarTest.elm`, `AnalysisTest.elm`, and a
focused decoder test if needed. Keep tests that exceed three lines in named
functions, per repository instructions. Golden/round-trip checks cover the
new JSON shapes; do not duplicate the domain algorithm in tests.

Candidate Hedgehog properties, each requiring the user's explicit go-ahead
**before implementation** under `AGENTS.md`:

1. Every emitted amount is positive and every event account has the required
   group, with generated valid journals and appropriate shrinking.
2. For a bucket, posted minus reversed equals net; borrowed minus repaid minus
   forgiven equals net debt movement. Transfer/refinance links contribute only
   their underlying legs to additive totals.
3. Grouping events into days/months preserves the set of transaction objects
   and does not duplicate a mixed transaction in a bucket's drill-down.
4. Changing display depth only regroups category amounts; it does not change
   event kind, movement totals, or missing-conversion warnings.

These should test laws with genuinely varied generated postings, not a copied
implementation as an oracle. Add focused examples for classification choices
whose meaning is more legible in journal form. A benchmark for the new
performance-sensitive Activity classification/aggregation should use generated
inputs and the supplied large journal fixtures, with setup and forcing outside
the timed action. Correctness checks precede the benchmark.

During implementation, enter one persistent `nix develop` shell; use narrow
backend/frontend checks while iterating, run `just format` after Haskell
changes, and run full `just check` before handoff. `nix flake check` is needed
only if Nix configuration changes. The planning document itself changes no
compiled code and does not require a build.

Acceptance means:

- No Haskell or Elm value can pair an income/expense bucket with an asset or
  liability movement, or a liability bucket with a transfer/reallocation field.
- All product examples above have the stated Activity outcome and labels.
- Reversals and zero-net activity remain visible; all numeric totals reconcile
  with their non-overlapping legs.
- Mixed transactions can appear in relevant views while appearing once per
  view's drill-down; refinancing and transfers are not double-counted.
- Ambiguous/unconverted entries produce explicit diagnostics rather than
  misleading movement labels or partial values.
- Net-worth and cashflow main reports retain their existing behavior; the
  revised Activity API and four Elm views work together, including filtering,
  period selection, and empty states.
