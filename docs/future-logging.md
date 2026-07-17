# Future logging considerations

Hedger currently targets a trusted homelab deployment. Release 1 therefore uses
`wai-extra`'s stock `logStdout` request logger behind a narrow WAI redaction
adapter. It emits conventional access logs while replacing the hledger `query`
parameter value with `[REDACTED]` and avoids a project-specific logging
abstraction.

The `HEDGER_LOG_LEVEL`/`--log-level` configuration is retained for future
application and journal-loading diagnostics, but it does not filter the stock
HTTP access log in Release 1.

Revisit structured logging only when an operational requirement justifies it,
for example centralized log ingestion, dashboards, alerting, or correlation
across services. At that point evaluate an off-the-shelf logging package and a
small WAI adapter. Useful fields may include request ID, method, route, status,
duration, response size, journal revision, reload duration, and cache outcome.

If Hedger is exposed beyond a trusted network, also define a redaction policy.
Do not log journal contents, transaction descriptions, request/response bodies,
authorization headers, or sensitive query values. Request-ID generation and
stable JSON error normalization are API behavior and should remain separate
from whichever logging implementation is selected.
