# Phase 4A transaction response baseline

Measured on 2026-07-17 in the pinned Nix development shell on x86_64 macOS
15.5 with GHC 9.10.3. The benchmark used `tasty-bench` in wall-clock mode with
a 10% target standard deviation and a five-second per-test timeout.

| Workload | Complete DTO projection | Projection plus JSON encoding | JSON size |
| --- | ---: | ---: | ---: |
| Shared fixture (13 transactions, 27 postings) | 42.1 µs ± 14 µs | 104 µs ± 13 µs | 8,345 bytes |
| Scaled representative workload (1,300 transactions, 2,700 postings) | 4.07 ms ± 452 µs | 9.61 ms ± 1.7 ms | 823,115 bytes |

The scaled workload repeats the shared fixture's valid finalized transactions
100 times before timing. Input parsing and generation are outside the timed
actions. This makes the baseline reproducible without adding private or very
large journal data to the repository. It measures the Phase 4A complete DTO
projection and JSON response construction that Phase 4B pagination will
replace.

Reproduce the timing with:

```console
cd backend
cabal bench --offline snapshot-bench --benchmark-options='--stdev 10 --timeout 5s --time-mode wall --hide-progress'
```

## Phase 4B query baseline

The Phase 4B benchmarks parse the hledger query, apply hledger transaction
matching, order matches, take the first 50 complete transactions, and project
their DTOs. Journal loading, parsing, finalization, and forcing happen in the
`tasty-bench` environment before the timed action.

| Fixture | Account query `acct:assets` | Boolean query `expr:'(acct:expenses OR desc:Salary)'` |
| --- | ---: | ---: |
| Shared fixture (13 transactions) | 64.3 µs ± 1.9 µs | 79.0 µs ± 4.6 µs |
| `india-us-portfolio-large-50000.journal` (50,000 transactions) | 93.3 ms ± 7.6 ms | 115 ms ± 9.2 ms |

These timings were measured on the same pinned environment and date described
above. Reproduce an individual large-fixture case from `backend/` with, for
example:

```console
cabal run snapshot-bench -- -p large-50000.account --hide-progress
```
