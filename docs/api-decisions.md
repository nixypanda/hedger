# Release 1 API decisions

These decisions are fixed for Release 1 unless a contract version changes.

## Transaction identity

Newly identified transactions use an explicit `hedger-id` tag whose value is
a canonical lowercase UUID. A legacy transaction without that tag receives a
derived ID of the form `sha256-v1:<hex>`. The hash input is a versioned,
canonical encoding of the root-relative source file, source span, date,
status, code, description, tags, and postings. Absolute paths never enter the
ID or API. A source edit may change a derived legacy ID; adding `hedger-id`
makes it durable. Duplicate explicit IDs make the snapshot invalid.

## Signs and magnitudes

Posting and balance DTOs preserve hledger's signed quantities: assets and
expenses normally have positive debit balances; liabilities, equity, and
income normally have negative credit balances. Screen projections expose
named flow or headline magnitudes (for example `expense`, `income`, and
`liabilityValue`) as non-negative decimal strings. Direction is represented by
the field name or an explicit enum, never by requiring the browser to infer it
from an account type. Missing values are `null` with completeness metadata,
not zero.

## Valuation

The default is period-end market valuation in the configured display
commodity. Conversion uses the latest price on or before the period boundary;
future prices are forbidden. Missing prices or costs produce unknown/partial
results and structured warnings rather than zeroes.

## Latest period

Implicit "latest" ranges end at the earlier of the wall-clock date and the
latest journal date when the journal contains future-dated entries; for an
inactive historical journal they end at its latest journal date. Explicit
ranges are never rewritten. This keeps historical ledgers useful while
preventing future entries from silently moving the current reporting window.

## Fiscal year

`fiscalYearStartMonth` defaults to `4` (April), is returned by bootstrap, and
is applied by the backend.

## Account matching

An account register returns postings owned by the exact account only. Account
filters match the selected account and all descendants. Multiple include
filters use OR; a transaction is excluded when any posting matches an excluded
subtree.
