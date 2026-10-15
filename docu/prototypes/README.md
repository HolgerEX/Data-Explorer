# Candidate preprocessing prototypes (NOT approved)

`preprocessing_candidate.R` is the cache (RDS keyed by md5 of file identity,
in-memory LRU) and hierarchical power-of-two summary code (count/sum/min/max with
extrema timestamps, gap-aware bins) plus viewport selection/query from
`Sins-boku/Data-Explorer` `feature/preprocessing` @ f3e6e2f. It is not sourced by
`app.R`. Per `docu/preprocessing.md`, the storage/cache architecture and the
resolution-generation strategy need user approval after comparison; the
alternatives (e.g. Arrow/Parquet or DuckDB storage; per-viewport on-demand
aggregation or M4/LTTB downsampling) have NOT been prototyped or benchmarked.

Measured so far (one run, sandbox, `Rscript docu/prototypes/bench_candidate.R 1e6`,
1 series, 1-min cadence, 1e6 rows, 41.9 MB CSV): read+normalise 13.5 s,
build 13 summary levels 11.2 s; raw object 96 MB, summaries 50 MB. Larger sizes,
parallelism, memory under concurrent sessions and rendering were not measured.

Candidate tests: `Rscript docu/prototypes/test-candidate.R` (4 pass).

Comparative benchmark of storage/cache and resolution alternatives: `bench_compare.R`, results and recommendation in `RESULTS.md`.
