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
