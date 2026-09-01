#!/usr/bin/env python3
"""End-to-end latency measurement for the Hedger analytics endpoints.

Drives real `hedger-server` processes over HTTP and reports median and spread for
every analytics endpoint in the cache states that matter: cold, warm cached, warm
on an uncached dimension (split three ways, see below), and
first-request-after-an-append. Also reports reload-to-ready and memory.

This exists because every other analytics performance figure in `docs/` is a
component or benchmark-harness number, and the harness and the server have been
measured disagreeing by ~4x on the same code. Only the server answers whether the
caching achieved its goal. Results are recorded under "End-to-end server
measurements" in `docs/analytics-performance-caching-plan.md`.

Method notes that matter for reading the output:

* Multiple samples of the *cold* state require restarting the server, because
  only the first request of a process is cold. Same for the commodity switch.
  `--cold-runs` and `--commodity-runs` control those.
* A request on a different dimension of the same revision is three different
  measurements, not one, because whether it can use the checkpoints depends on
  which dimension moved:
  - `hits` vary report depth (or granularity and the commodity alias for
    investments), which no checkpoint key contains, so they miss the response
    cache and can still resume from checkpoints.
  - `misses` vary the hledger query using sentinel queries that match every
    transaction. Every net-worth and cashflow checkpoint key contains the query,
    so these do *identical work* while missing the checkpoints, which is what
    makes the pair a control rather than two unrelated numbers.
    `check_equal_work` asserts the reports really are identical.
  - `commodity` switches the display commodity, which is in every checkpoint key
    and in the response dimension, so it misses everything.
* `readCacheMetrics` and `readRebuildMetrics` have no HTTP surface, so cache hits
  here are inferred from latency plus that control, never read. Do not add an
  endpoint for them without asking.
* GHC block-buffers stdout when it is not a terminal, so the server runs on a pty
  or its log lines would only arrive when the process exits.

Usage:

    python3 scripts/measure_analytics_latency.py --size 50000
    python3 scripts/measure_analytics_latency.py --size 5000 --samples 5

Never point this at port 8080; a developer backend serving a private journal
lives there and the script refuses that port.
"""

import argparse
import json
import os
import pathlib
import pty
import shutil
import statistics
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request

REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parent.parent

FIXTURES = {
    "5000": "examples/performance/india-us-portfolio-medium-5000.journal",
    "50000": "examples/performance/india-us-portfolio-large-50000.journal",
}

# The developer's own backend serves a private journal here.
FORBIDDEN_PORT = 8080

REBUILD_LOG_PREFIX = "Rebuilt derived analytics data"
LISTENING_LOG_PREFIX = "Started server on port"


# --- request definitions ----------------------------------------------------
#
# `base` is the dimension the eager warm covers: monthly, the configured default
# display commodity, root scope, no query, default depth. `hits` vary only the
# report depth, which no checkpoint key contains, so each one misses the
# response cache and can still resume from a checkpoint. `misses` vary the query
# or scope, which every checkpoint key contains, so each one misses both.


def analytics_body(**overrides):
    body = {
        "granularity": "monthly",
        "query": None,
        "displayCommodity": None,
        "positivePeriod": None,
        "negativePeriod": None,
        "positiveDepth": None,
        "negativeDepth": None,
    }
    body.update(overrides)
    return body


def activity_body(**overrides):
    # A fiscal-year period must be stated as the configured fiscal-year start
    # month in YYYY-MM form, which `hedger.example.yaml` sets to April.
    body = {
        "kind": "expense",
        "range": "year",
        "period": "2026-04",
        "query": None,
        "displayCommodity": None,
        "depth": None,
    }
    body.update(overrides)
    return body


def investments_body(**overrides):
    body = {"granularity": "monthly", "displayCommodity": None, "accountPrefix": None}
    body.update(overrides)
    return body


# Category depth is validated to 1..4 and defaults to 2, so five *distinct*
# dimension keys need the two sides varied independently. Depth is a projection
# over unclipped cached rows and appears in no checkpoint key, which is what makes
# these the response-cache-miss/checkpoint-hit samples.
DEPTH_PAIRS = [(1, 1), (3, 3), (4, 4), (1, 3), (3, 1)]
DEPTHS = [1, 3, 4]

# Queries that match every transaction, so each costs the same as the unfiltered
# base request while differing textually. `QueryKey.text` and `ColumnKey.query`
# are part of every net-worth and cashflow checkpoint key, so these miss the
# checkpoints while doing identical work. That equal-work property is what makes
# the hit/miss pair a control rather than two unrelated numbers, and the script
# asserts the reported net worth is unchanged to keep it honest.
SENTINEL_QUERIES = [f"not:desc:zzzzq{index}" for index in range(1, 6)]

# Investments has no query and no depth dimension, so its only checkpoint-key
# scoping is the account prefix, and a narrower prefix genuinely does less work.
# That cell is therefore not an equal-work control and is labelled as such.
SCOPES = [
    "assets:equity",
    "assets:debt",
    "assets:liquid",
    "assets:hybrid",
    "assets:equity:index",
]

# Fiscal years inside both fixtures' spans; the medium fixture ends 2028-12-31,
# so a later year would be trivially cheap for being outside the journal.
ACTIVITY_PERIODS = ["2023-04", "2024-04", "2025-04", "2027-04", "2028-04"]

# A display commodity other than the configured default. It appears in every
# checkpoint key and in every response dimension, so switching misses everything.
ALTERNATIVE_COMMODITY = "USD"

ENDPOINTS = [
    {
        "name": "net-worth/query",
        "path": "/api/v1/net-worth/query",
        "base": analytics_body(),
        "hits": [
            analytics_body(positiveDepth=positive, negativeDepth=negative)
            for positive, negative in DEPTH_PAIRS
        ],
        "misses": [analytics_body(query=query) for query in SENTINEL_QUERIES],
        "equal_work_misses": True,
        "commodity": analytics_body(displayCommodity=ALTERNATIVE_COMMODITY),
    },
    {
        "name": "cashflow/query",
        "path": "/api/v1/cashflow/query",
        "base": analytics_body(),
        "hits": [
            analytics_body(positiveDepth=positive, negativeDepth=negative)
            for positive, negative in DEPTH_PAIRS
        ],
        "misses": [analytics_body(query=query) for query in SENTINEL_QUERIES],
        "equal_work_misses": True,
        "commodity": analytics_body(displayCommodity=ALTERNATIVE_COMMODITY),
    },
    {
        "name": "investments/query",
        "path": "/api/v1/investments/query",
        "base": investments_body(),
        # Only three dimension keys resolve to the warm set's checkpoint keys: an
        # explicit default commodity (the aliasing `dimensionsFor` documents),
        # yearly granularity (whose period ends are a subset of the cached month
        # ends), and both together. There is no depth dimension to vary.
        "hits": [
            investments_body(granularity="yearly"),
            investments_body(displayCommodity="INR"),
            investments_body(granularity="yearly", displayCommodity="INR"),
        ],
        "misses": [investments_body(accountPrefix=scope) for scope in SCOPES],
        "equal_work_misses": False,
        "commodity": investments_body(displayCommodity=ALTERNATIVE_COMMODITY),
    },
    {
        "name": "activity/query",
        "path": "/api/v1/activity/query",
        "base": activity_body(),
        # Activity resumes from no checkpoint cache at all: neither
        # `Application.Analytics.activity` nor the eager warm touches one. Both
        # columns therefore measure the same thing, a full recompute behind the
        # response cache, and neither is evidence about the checkpoints.
        "hits": [activity_body(displayCommodity="INR")]
        + [activity_body(depth=depth) for depth in DEPTHS],
        "misses": [activity_body(period=period) for period in ACTIVITY_PERIODS],
        "equal_work_misses": False,
        "commodity": activity_body(displayCommodity=ALTERNATIVE_COMMODITY),
    },
]

APPEND_ACCOUNTS = ("expenses:food:self", "assets:cash:bank:neev")


# --- server process ---------------------------------------------------------


class Server:
    """A `hedger-server` child process with timestamped stdout and RSS sampling."""

    def __init__(self, binary, config, journal, port, log_path):
        self.binary = binary
        self.config = config
        self.journal = journal
        self.port = port
        self.log_path = log_path
        self.process = None
        self.lines = []  # (monotonic seconds, text)

    def __enter__(self):
        self.log_path.parent.mkdir(parents=True, exist_ok=True)
        log = open(self.log_path, "w")
        self.started_at = time.monotonic()
        # A pty, not a pipe. GHC's stdout is block-buffered when it is not a
        # terminal, so over a pipe every log line only arrives when the process
        # exits, which makes log timestamps useless for measuring a rebuild.
        self._master_fd, slave_fd = pty.openpty()
        self.process = subprocess.Popen(
            [
                str(self.binary),
                "--config",
                str(self.config),
                "--journal",
                str(self.journal),
                "--port",
                str(self.port),
                # `ps -o rss` needs an entitlement macOS does not grant here, so
                # memory comes from the RTS instead. `-s` goes to stderr at a
                # clean exit, and its TASKS line also confirms `-N` was honoured.
                "+RTS",
                "-s",
                "-RTS",
            ],
            cwd=str(REPOSITORY_ROOT),
            stdout=slave_fd,
            stderr=slave_fd,
        )
        os.close(slave_fd)
        self._log_file = log
        self._reader = threading.Thread(target=self._read_output, daemon=True)
        self._reader.start()
        return self

    def __exit__(self, *_):
        if self.process and self.process.poll() is None:
            # SIGTERM is the graceful path: Warp closes the socket, `runServer`
            # returns, and the RTS prints its `-s` summary on the way out.
            self.process.terminate()
            try:
                self.process.wait(timeout=30)
            except subprocess.TimeoutExpired:
                self.process.kill()
        self._reader.join(timeout=10)
        os.close(self._master_fd)
        self._log_file.close()
        return False

    def _read_output(self):
        pending = ""
        while True:
            try:
                chunk = os.read(self._master_fd, 8192)
            except OSError:
                break
            if not chunk:
                break
            pending += chunk.decode("utf-8", "replace")
            while "\n" in pending:
                line, pending = pending.split("\n", 1)
                stamp = time.monotonic()
                self.lines.append((stamp, line.rstrip("\r")))
                self._log_file.write(line + "\n")
                self._log_file.flush()

    def rts_stats(self):
        """Parse the `+RTS -s` summary. Only meaningful after a clean exit."""
        stats = {}
        for _, text in self.lines:
            stripped = text.strip()
            if "maximum residency" in stripped:
                stats["max_residency_bytes"] = int(
                    stripped.split()[0].replace(",", "")
                )
            elif "total memory in use" in stripped:
                stats["peak_memory_mib"] = int(stripped.split()[0].replace(",", ""))
            elif stripped.startswith("TASKS:"):
                stats["tasks"] = stripped
            elif "bytes allocated in the heap" in stripped:
                stats["allocated_bytes"] = int(stripped.split()[0].replace(",", ""))
        return stats

    def wait_for_line(self, prefix, since=0.0, timeout=600.0):
        """Return the monotonic timestamp of the first line after `since`."""
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            for stamp, text in list(self.lines):
                if stamp > since and prefix in text:
                    return stamp
            if self.process.poll() is not None:
                raise RuntimeError(f"server exited before logging {prefix!r}")
            time.sleep(0.02)
        raise TimeoutError(f"timed out waiting for {prefix!r}")

    def wait_until_serving(self, timeout=900.0):
        """Monotonic time of the first served response.

        `runServer` completes the boot reload and derived rebuild *before* Warp
        binds the socket, so nothing is served until the rebuild is done and this
        doubles as the boot-rebuild measurement without reading a log line.
        """
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if self.process.poll() is not None:
                raise RuntimeError("server exited before it began serving")
            try:
                self.get("/api/v1/health/ready", timeout=5)
                return time.monotonic()
            except (urllib.error.URLError, ConnectionError, TimeoutError, OSError):
                time.sleep(0.05)
        raise TimeoutError("server never became ready")

    def _url(self, path):
        return f"http://127.0.0.1:{self.port}{path}"

    def get(self, path, timeout=1800.0):
        request = urllib.request.Request(self._url(path), method="GET")
        started = time.perf_counter()
        with urllib.request.urlopen(request, timeout=timeout) as response:
            payload = response.read()
        return time.perf_counter() - started, payload

    def post(self, path, body, timeout=1800.0):
        encoded = json.dumps(body).encode()
        request = urllib.request.Request(
            self._url(path),
            data=encoded,
            method="POST",
            headers={"Content-Type": "application/json"},
        )
        started = time.perf_counter()
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                payload = response.read()
        except urllib.error.HTTPError as failure:
            # The error envelope names the rejected field, which is the only way
            # to tell a bad request body in this script from a server fault.
            raise SystemExit(
                f"POST {path} returned HTTP {failure.code}: "
                f"{failure.read().decode('utf-8', 'replace')}\nbody was {encoded.decode()}"
            ) from failure
        return time.perf_counter() - started, payload

    def metadata(self):
        _, payload = self.get("/api/v1/bootstrap")
        return json.loads(payload)["metadata"]


# --- statistics -------------------------------------------------------------


class Measurement:
    def __init__(self, label, seconds, response_bytes=0, notes=""):
        self.label = label
        self.seconds = list(seconds)
        self.response_bytes = response_bytes
        self.notes = notes

    @property
    def median_ms(self):
        return statistics.median(self.seconds) * 1000

    @property
    def min_ms(self):
        return min(self.seconds) * 1000

    @property
    def max_ms(self):
        return max(self.seconds) * 1000

    def cell(self, expected=None):
        if not self.seconds:
            return "n/a"
        if len(self.seconds) == 1:
            return f"{format_ms(self.median_ms)} (n=1)"
        suffix = ""
        if expected is not None and len(self.seconds) != expected:
            suffix = f" (n={len(self.seconds)})"
        return (
            f"{format_ms(self.median_ms)} "
            f"[{format_ms(self.min_ms)}–{format_ms(self.max_ms)}]{suffix}"
        )


def format_ms(value):
    if value >= 10_000:
        return f"{value / 1000:.1f} s"
    if value >= 1000:
        return f"{value / 1000:.2f} s"
    if value >= 10:
        return f"{value:.0f} ms"
    return f"{value:.1f} ms"


# --- phases -----------------------------------------------------------------


def measure_series(server, path, bodies, samples):
    """One timing per distinct body, never cycling.

    Reusing a body would make the second request a response-cache hit and
    silently turn a miss measurement into a hit measurement, so the sample count
    of an uncached cell is capped by how many distinct dimensions exist. That cap
    is reported rather than papered over.
    """
    timings = []
    payload_size = 0
    for body in bodies[:samples]:
        seconds, payload = server.post(path, body)
        timings.append(seconds)
        payload_size = max(payload_size, len(payload))
    return timings, payload_size


def measure_cold(context, cold_runs, commodity_runs):
    """First-request latency per endpoint, one sample per server start.

    Cold and commodity-switch both have to be the *first* request of their kind
    in a process, so both are sampled by restarting rather than by repeating.
    `commodity_runs` is separate because a display-commodity switch is minutes of
    work on the large fixture and would otherwise dominate the whole run; it is
    an extra state, not one of the four the measurement is required to cover.
    """
    collected = {endpoint["name"]: [] for endpoint in ENDPOINTS}
    commodity = {endpoint["name"]: [] for endpoint in ENDPOINTS}
    boot_rebuilds = []
    sizes = {}
    for run in range(cold_runs):
        log_path = context["log_dir"] / f"cold-{run}.log"
        with make_server(context, log_path) as server:
            serving = server.wait_until_serving()
            boot_rebuilds.append(serving - server.started_at)
            for endpoint in ENDPOINTS:
                seconds, payload = server.post(endpoint["path"], endpoint["base"])
                collected[endpoint["name"]].append(seconds)
                sizes[endpoint["name"]] = len(payload)
            if run < commodity_runs:
                for endpoint in ENDPOINTS:
                    seconds, _ = server.post(endpoint["path"], endpoint["commodity"])
                    commodity[endpoint["name"]].append(seconds)
                    report_progress(
                        f"    commodity switch {endpoint['name']} "
                        f"{format_ms(seconds * 1000)}"
                    )
            report_progress(f"  cold run {run + 1}/{cold_runs} done")
    return collected, commodity, boot_rebuilds, sizes


def measure_residency(context):
    """RTS memory for the state the question is about: booted, warmed, idle.

    Deliberately its own process. The dimension sweep and the commodity switch
    allocate heavily and would make a peak figure say nothing about what the
    caches cost a server sitting on the dashboard's default view.
    """
    log_path = context["log_dir"] / "residency.log"
    with make_server(context, log_path) as server:
        server.wait_until_serving()
        for endpoint in ENDPOINTS:
            server.post(endpoint["path"], endpoint["base"])
    return server.rts_stats()


def measure_warm_states(context, samples):
    """Warm cached, warm uncached-hit and warm uncached-miss in one process."""
    results = {}
    log_path = context["log_dir"] / "warm.log"
    with make_server(context, log_path) as server:
        server.wait_until_serving()
        metadata = server.metadata()
        for endpoint in ENDPOINTS:
            name = endpoint["name"]
            # Prime, so the repeat below is a response-cache hit rather than the
            # cold request.
            _, baseline = server.post(endpoint["path"], endpoint["base"])
            cached, size = measure_series(
                server, endpoint["path"], [endpoint["base"]] * samples, samples
            )
            hit, _ = measure_series(server, endpoint["path"], endpoint["hits"], samples)
            miss, _ = measure_series(
                server, endpoint["path"], endpoint["misses"], samples
            )
            if endpoint["equal_work_misses"]:
                check_equal_work(server, endpoint, baseline)
            results[name] = {
                "warm-cached": Measurement(name, cached, size),
                "warm-uncached-hit": Measurement(name, hit),
                "warm-uncached-miss": Measurement(name, miss),
            }
            report_progress(f"  warm states for {name} done")
        http_floor = measure_series_get(server, "/api/v1/health/ready", samples)
        results["_http_floor"] = Measurement("health/ready", http_floor)
        results["_metadata"] = metadata
    return results


def check_equal_work(server, endpoint, baseline):
    """Fail loudly if a `misses` variant is not the equal-work control claimed.

    The whole inference from column 3a against 3b rests on the two doing the same
    amount of work, so a sentinel query that quietly stopped matching every
    transaction would invalidate the conclusion rather than just the numbers.
    """
    expected = json.loads(baseline)
    expected.pop("metadata", None)
    for body in endpoint["misses"]:
        _, payload = server.post(endpoint["path"], body)
        actual = json.loads(payload)
        actual.pop("metadata", None)
        if actual != expected:
            raise SystemExit(
                f"{endpoint['name']}: the miss variant {body!r} is not the "
                "equal-work control it is documented to be — it returned a "
                "different report, so columns 3a and 3b are not comparable"
            )


def measure_series_get(server, path, samples):
    return [server.get(path)[0] for _ in range(samples)]


def measure_appends(context, samples, date_mode):
    """Reload-to-ready and first-request-after-append, one sample per append."""
    reloads = []
    first_request = {endpoint["name"]: [] for endpoint in ENDPOINTS}
    log_path = context["log_dir"] / f"append-{date_mode}.log"
    with make_server(context, log_path) as server:
        server.wait_until_serving()
        for endpoint in ENDPOINTS:
            server.post(endpoint["path"], endpoint["base"])
        for index in range(samples):
            previous_revision = server.metadata()["revision"]
            append_transaction(context["journal"], date_mode, index)
            written = time.monotonic()
            wait_until_fresh(server, previous_revision)
            reloads.append(time.monotonic() - written)
            for endpoint in ENDPOINTS:
                seconds, _ = server.post(endpoint["path"], endpoint["base"])
                first_request[endpoint["name"]].append(seconds)
            report_progress(
                f"  append {index + 1}/{samples} ({date_mode}) "
                f"reload {format_ms(reloads[-1] * 1000)}"
            )
    return reloads, first_request


def wait_until_fresh(server, previous_revision, timeout=900.0):
    """Wait for a new snapshot revision whose derived data has caught up.

    Both halves are needed: `fresh` alone is still true for the revision that was
    already published, so the revision must have moved before freshness means the
    rebuild for *this* edit finished.
    """
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        metadata = server.metadata()
        if metadata["revision"] != previous_revision and metadata["freshness"] == "fresh":
            return metadata
        time.sleep(0.02)
    raise TimeoutError("derived revision never caught up with the snapshot")


def append_transaction(journal, date_mode, index):
    """Append one valid transaction. `date_mode` decides where in history."""
    if date_mode == "today":
        date = time.strftime("%Y-%m-%d")
    else:
        # Past the fixture's last transaction, which is future-dated, so the
        # chain diverges at the very end rather than mid-history.
        date = f"2073-02-{index + 1:02d}"
    debit, credit = APPEND_ACCOUNTS
    entry = (
        f"\n{date} * latency measurement append {index}\n"
        f"    {debit}          {index + 1}.0000 INR\n"
        f"    {credit}\n"
    )
    with open(journal, "a") as handle:
        handle.write(entry)
        handle.flush()
        os.fsync(handle.fileno())


# --- driver -----------------------------------------------------------------


def make_server(context, log_path):
    return Server(
        binary=context["binary"],
        config=context["config"],
        journal=context["journal"],
        port=context["port"],
        log_path=log_path,
    )


def report_progress(message):
    print(message, file=sys.stderr, flush=True)


def locate_binary(explicit):
    if explicit:
        return pathlib.Path(explicit)
    result = subprocess.run(
        ["cabal", "list-bin", "--enable-optimization=2", "hedger-server"],
        cwd=str(REPOSITORY_ROOT / "backend"),
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise SystemExit(
            "could not locate hedger-server. Build it first with\n"
            "  cd backend && cabal build --enable-optimization=2 hedger-server\n"
            f"cabal said: {result.stderr.strip()}"
        )
    return pathlib.Path(result.stdout.strip())


def render(report):
    out = []
    out.append(f"### {report['fixture_label']}")
    out.append("")
    out.append(
        f"Binary `{report['binary']}` (`-O2`, `-with-rtsopts=-N`), "
        f"journal working copy of `{report['fixture']}`, "
        f"{report['samples']} samples per cell, {report['cold_runs']} server "
        "starts for the cold column."
    )
    out.append("")
    out.append(
        "| Endpoint | 1. Cold | 2. Warm cached | 3a. Warm, response miss, "
        "checkpoint keys shared | 3b. Warm, response miss, checkpoint keys "
        "changed | 3c. Commodity switch | 4a. After append (today) | "
        "4b. After append (journal tail) |"
    )
    out.append("|---|---:|---:|---:|---:|---:|---:|---:|")
    for endpoint in ENDPOINTS:
        name = endpoint["name"]
        expected = report["samples"]
        cells = [
            report["cold"][name].cell(report["cold_runs"]),
            report["warm"][name]["warm-cached"].cell(expected),
            report["warm"][name]["warm-uncached-hit"].cell(expected),
            report["warm"][name]["warm-uncached-miss"].cell(expected),
            report["commodity"][name].cell(report["commodity_runs"]),
            report["append_today"][name].cell(expected)
            if report.get("append_today")
            else "n/a",
            report["append_tail"][name].cell(expected)
            if report.get("append_tail")
            else "n/a",
        ]
        out.append(f"| `{name}` | " + " | ".join(cells) + " |")
    out.append("")
    out.append(f"Median [min–max]; `n` is stated where fewer samples exist.")
    out.append("")
    out.append(
        f"* HTTP floor (`GET /api/v1/health/ready`): {report['http_floor'].cell()} — "
        "the routing, JSON and loopback cost with no analytics compute behind it."
    )
    out.append(
        f"* Boot: process start to first served response, which is also the boot "
        f"reload plus derived rebuild, because Warp binds the socket only after "
        f"`rebuildAfterReload` returns: {report['boot'].cell()}."
    )
    if report.get("reload_today"):
        out.append(
            f"* Reload-to-ready, append dated today: {report['reload_today'].cell()} "
            "(file write to a new snapshot revision whose `metadata.freshness` is "
            "`fresh` again; includes the 100 ms watcher debounce)."
        )
    if report.get("reload_tail"):
        out.append(
            f"* Reload-to-ready, append at the journal tail: "
            f"{report['reload_tail'].cell()}."
        )
    out.append("* " + describe_memory(report["residency"]))
    out.append(f"* Response metadata on a warm server: `{report['metadata']}`.")
    out.append(
        "* Largest response bodies: "
        + ", ".join(
            f"`{name}` {size / 2**20:.2f} MiB"
            for name, size in report["sizes"].items()
        )
        + "."
    )
    return "\n".join(out)


def describe_memory(stats):
    if not stats:
        return "Memory: `+RTS -s` produced no summary (the server did not exit cleanly)."
    parts = []
    if "peak_memory_mib" in stats:
        parts.append(f"{stats['peak_memory_mib']} MiB peak total memory in use")
    if "max_residency_bytes" in stats:
        parts.append(
            f"{stats['max_residency_bytes'] / 2**20:.0f} MiB maximum residency"
        )
    if "allocated_bytes" in stats:
        parts.append(f"{stats['allocated_bytes'] / 2**30:.1f} GiB allocated")
    detail = f" RTS reports `{stats['tasks']}`." if "tasks" in stats else ""
    return (
        "Memory of a booted, eagerly warmed server that has served the four "
        "default requests and nothing else, from `+RTS -s` at a clean exit: "
        + ", ".join(parts)
        + "."
        + detail
    )


def run(size, args, log_dir):
    fixture = REPOSITORY_ROOT / FIXTURES[size]
    workdir = pathlib.Path(tempfile.mkdtemp(prefix=f"hedger-latency-{size}-"))
    journal = workdir / fixture.name
    context = {
        "binary": args.binary,
        "config": REPOSITORY_ROOT / args.config,
        "journal": journal,
        "port": args.port,
        "log_dir": log_dir,
    }
    report = {
        "fixture_label": f"{int(size):,}-transaction fixture",
        "fixture": FIXTURES[size],
        "binary": args.binary.name,
        "samples": args.samples,
        "cold_runs": args.cold_runs,
        "commodity_runs": args.commodity_runs,
    }

    report_progress(f"[{size}] cold runs and commodity switches")
    shutil.copy(fixture, journal)
    cold, commodity, boots, sizes = measure_cold(
        context, args.cold_runs, args.commodity_runs
    )
    report["cold"] = {name: Measurement(name, values) for name, values in cold.items()}
    report["commodity"] = {
        name: Measurement(name, values) for name, values in commodity.items()
    }
    report["boot"] = Measurement("boot", boots)
    report["sizes"] = sizes

    report_progress(f"[{size}] memory of a warmed idle server")
    report["residency"] = measure_residency(context)

    report_progress(f"[{size}] warm states")
    warm = measure_warm_states(context, args.samples)
    report["warm"] = {e["name"]: warm[e["name"]] for e in ENDPOINTS}
    report["http_floor"] = warm["_http_floor"]
    report["metadata"] = warm["_metadata"]

    for mode in ("today", "tail"):
        if mode not in args.append_modes:
            continue
        report_progress(f"[{size}] appends dated {mode}")
        shutil.copy(fixture, journal)
        reloads, first = measure_appends(context, args.samples, mode)
        report[f"reload_{mode}"] = Measurement("reload", reloads)
        report[f"append_{mode}"] = {
            name: Measurement(name, values) for name, values in first.items()
        }

    shutil.rmtree(workdir, ignore_errors=True)
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--size",
        action="append",
        choices=sorted(FIXTURES),
        help="fixture to measure; repeatable, defaults to both",
    )
    parser.add_argument("--port", type=int, default=8137)
    parser.add_argument("--samples", type=int, default=5)
    parser.add_argument(
        "--cold-runs",
        type=int,
        default=5,
        help="server starts, one cold sample per endpoint each",
    )
    parser.add_argument(
        "--commodity-runs",
        type=int,
        default=2,
        help=(
            "of those server starts, how many also measure a display-commodity "
            "switch; it costs minutes per sample on the large fixture"
        ),
    )
    parser.add_argument("--config", default="hedger.example.yaml")
    parser.add_argument("--binary", help="path to hedger-server; located via cabal if omitted")
    parser.add_argument(
        "--append-modes",
        default="today,tail",
        help="comma-separated subset of today,tail (empty to skip the append phase)",
    )
    parser.add_argument("--output", help="write the markdown report here as well as stdout")
    parser.add_argument("--log-dir", help="keep server logs here instead of a temp dir")
    args = parser.parse_args()

    if args.port == FORBIDDEN_PORT:
        raise SystemExit(
            f"refusing port {FORBIDDEN_PORT}: a developer backend with a private "
            "journal conventionally runs there"
        )
    args.binary = locate_binary(args.binary)
    args.append_modes = [m for m in args.append_modes.split(",") if m]
    sizes = args.size or sorted(FIXTURES, key=int)

    log_dir = pathlib.Path(args.log_dir) if args.log_dir else pathlib.Path(
        tempfile.mkdtemp(prefix="hedger-latency-logs-")
    )
    log_dir.mkdir(parents=True, exist_ok=True)
    report_progress(f"server logs in {log_dir}")

    sections = []
    for size in sizes:
        (log_dir / size).mkdir(parents=True, exist_ok=True)
        report = run(size, args, log_dir / size)
        sections.append(render(report))
        print(sections[-1], flush=True)
        print(flush=True)

    if args.output:
        pathlib.Path(args.output).write_text("\n\n".join(sections) + "\n")


if __name__ == "__main__":
    main()
