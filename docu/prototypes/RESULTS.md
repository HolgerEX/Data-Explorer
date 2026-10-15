# Decision-gate benchmark report (measured; NOT an approval)

Nothing here is wired into `app.R`. Script: `docu/prototypes/bench_compare.R`
(self-contained, data.table only, seed 42, single-threaded; every stage that
reports peak RSS/cold load runs in its own `Rscript` process).
Environment: sandbox VM, 4 cores, 16 GB RAM, R 4.3.3, data.table 1.14.10, Linux,
disk = sandbox temp dir (type not characterised). One run per scenario; timings
for builds/queries are medians of 3 / 20 repeats, cold loads and writes are single
measurements. **Synthetic data only: the repo contains no real data**
(`data/` and `data_input/` hold only `data_description.txt`).

Scenarios (reproduce: `Rscript docu/prototypes/bench_compare.R S1` / `S2`):
- **S1**: 1 series, 1 000 000 rows, 1-min cadence (694 d), random walk + daily sine + 50 injected +100 spikes.
- **S2**: 10 series, 4 991 360 rows total, 10-min cadence (3472 d), 3 two-day gaps/series, 500 spikes.

Stored statistics per bin: count, sum, min, max. Raw read here is `data.table::fread`
of a clean file, **not** the app's `read_timeseries_file` (earlier measured 13.5 s
read+normalise for 1e6 rows), so "first load" below understates real app first load.

## 3.2 Resolution generation (summaries in memory, built from raw)
Fixed widths A/C: 60 s, 5 min, 15 min, 1 h, 4 h, 1 d, 7 d. Pyramid B: base = 4 x median
cadence, doubled 12 times (13 levels).

| scenario | design | build s | levels | bins total | summary MB | finest-level bins |
|---|---|---|---|---|---|---|
| S1 | A fixed, each from raw | 0.61 | 7 | 1 288 297 | 56.7 | 1 000 000 |
| S1 | B pyramid | 0.15 | 13 | 499 950 | 22.0 | 250 001 |
| S1 | C hybrid (fixed widths merged from previous level) | 0.20 | 7 | 1 288 297 | 56.7 | 1 000 000 |
| S2 | A | 3.18 | 7 | 14 389 910 | 633.2 | 4 991 360 |
| S2 | B | 0.83 | 13 | 2 495 720 | 109.9 | 1 247 874 |
| S2 | C | 1.95 | 7 | 14 389 910 | 633.2 | 4 991 360 |

Query latency (ms, finest level with <=1000 bins across the viewport, window at the end of the data), S2:
full 0 / 1 y 0-0.5 / 1 mo 3-4 / 1 wk 6-15 / 1 d 6-22 for B / A,C; raw subsetting: 55 / 25 / 22 / 22 / 22.
S1: all <=1 ms for A/B/C; raw subset 11 / 8 / 5 / 5 / 4.5. Bins returned per viewport: A/C up to 7 210,
B 380-9 780 (S2). (ms values are coarse: `system.time` resolution is 10 ms-ish, so 0-1 ms = "below timer resolution".)

Accuracy checks (all measured, all passed):
- A and C are bit-identical at every level (min/max/sum/count merge exactly), C builds ~1.6x faster.
- B's coarsest merged level equals direct aggregation from raw.
- Spikes: in daily bins 100 % of injected spikes are visible through bin max, 0 % through bin mean.
- Local-time days: Europe/Vienna days computed from 1-h UTC bins equal days computed from raw (S1 695 d, S2 34 700 series-days),
  so DST is handled if the hour level is the base for local calendar bins. Fixed UTC-epoch 86 400 s bins are not local-day aligned.

Findings/caveats: the fixed 60 s base is no reduction for data at >=1-min cadence (finest-level bins = rows; S2 summary 633 MB
= 5.3x the raw object); a cadence-derived base (B) avoids this. Pyramid levels are not calendar-aligned. Irregular
sampling inside a level was not benchmarked beyond gaps; extrema timestamps (in the source-branch candidate) are not measured here.

## 3.1 Storage / cache (hybrid = persisted summaries + lazily loaded raw)
| scenario | M: memory only, new process s (peak RSS MB) | P: persisted summaries+raw cold load s, gzip / uncompressed (RSS MB) | H: persisted summaries only, cold load s gzip / uncompressed (RSS MB) | persisted MB gzip / uncompressed (summaries+raw) | write s gzip / uncompressed | source CSV MB |
|---|---|---|---|---|---|---|
| S1 | 0.47 (170) | 0.69 / 0.31 (144 / 165) | 0.39 / 0.19 (121) | 42.9 / 85.3 | 3.7 / 0.26 | 31.5 |
| S2 | 3.18 (1025) | 5.62 / 2.74 (785 / 898) | 4.58 / 2.14 (671) | 422 / 794 | 31.4 / 2.35 | 157.4 |

Observations: with fast `fread` input, rebuilding from CSV (M) is *faster* than loading the persisted
summaries, so persistence only pays off when the real parse/normalise step is slow (as with the app's current
loader: 13.5 s per 1e6 rows measured earlier) or for repeated starts; this was not re-measured end to end.
gzip (saveRDS default) costs ~10x write time and ~2x read time for ~45 % less disk. Not measured: concurrent
sessions/cache contention, source-change invalidation, incremental append, Parquet/DuckDB/Arrow (packages not
installable here: `arrow`/`duckdb` are unavailable via apt and CRAN was not tried), browser rendering, `future`/parallel gain.

## Recommendation (needs your approval)
Resolution: B-style cadence-derived pyramid with merge-from-previous-level (statistics count/sum/min/max combine exactly),
plus an hourly UTC level so local-time calendar bins can be derived exactly (the hybrid C idea). Fixed bins only where
calendar alignment is required. Storage: hybrid = persisted uncompressed-or-gzip RDS summaries keyed by source
size/mtime/version + bounded in-memory LRU, raw loaded on demand; consider Parquet/DuckDB only if a benchmark in an
environment that has them shows a benefit.
