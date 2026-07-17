# Release 1 API decisions

These decisions are fixed for Release 1 unless a contract version changes.

## Transaction payloads

Transaction list responses contain complete transaction DTOs, including every
posting needed to render the selected transaction. There is no transaction
summary DTO, separate transaction-detail fetch, or Hedger-specific transaction
identifier. The frontend retains the selected transaction from the list it
already has.

## Transaction queries

The final transaction-list contract is
`GET /api/v1/transactions?query=...&limit=...&cursor=...`. The backend parses
and evaluates the single `query` text value with the repository's pinned
`hledger-lib` version. An absent or empty query matches all transactions.
`limit` defaults to `50` and is capped at `200`; `cursor` is an opaque
continuation token bound to the query and journal revision. There is no JSON
request DTO for transaction queries. Successful responses remain JSON API
responses.

Phase 4A may temporarily expose `GET /api/v1/transactions` without query or
pagination so the complete DTO and frontend can be built first. Phase 4B
replaces that all-at-once behavior with the final limited contract before
Release 1.

There is no parallel Hedger-owned structured JSON filter model for dates,
amounts, commodities, accounts, statuses, or other transaction fields. The
frontend provides a text field with draft/apply/clear behavior, parser errors,
and concise query-language help. Advanced relationships between posting
criteria use hledger's own `expr:`, `any:`, and `all:` semantics.

Relative date expressions use one backend-selected reference date that is
reported in response metadata and retained by continuation cursors. Query
options that alter report behavior rather than select transactions are rejected.
Invalid queries return a stable validation error without logging or returning
the complete query text. The server caps query length, clients URL-encode the
value, and access logs and telemetry redact the `query` parameter.

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
