# Data Explorer

Shiny app (`app.R`) for plotting tidy long-format environmental data.

## Run
```r
shiny::runApp("app.R")
```
Packages: shiny, ggplot2, dplyr, tidyr, purrr, stringr, readr, tibble, scales, plotly.

## Adding data
Use **Add data** in the sidebar (several files at once). Accepted: `.csv`
(comma or semicolon separated) and `.rds`. Required columns (long format):

| column | meaning |
|---|---|
| `variable` | name of the measured variable |
| `value` | numeric value |
| `datetime` or `date` | time stamp (ISO `2023-05-31 14:00:00` or `31.05.2023 14:00`) |

Aliases such as `timestamp`, `parameter`, `val` are renamed automatically. All
other columns (`logger`, `depth_bottom`, `unit`, ...) are kept and usable for
splitting/colouring. Invalid files show the reason under the upload button.
Accepted files are stored as `.rds` in `data_processed/` (never overwriting an
existing file; your uploaded source is not modified) and are selected
immediately. Tables that are not in long format (wide files, logs) are no longer
rejected: every numeric column becomes a variable, `datetime`/`timestamp`/`date`
is used as time (row number if none), and they get a basic plot. Adjust the time
and value columns in the sidebar ("Apply columns"). Files without any numeric
column are rejected. RDS read failures report the affected
filename and suggest checking that the upload completed and the file is valid.
Datasets in `data/*.rds` are listed too.

## Saved profiles
Once a plot looks right, **Save plot config to Env-Plot** writes a profile
(column mapping + plot settings) to `Env-Plot/profiles/<name>.rds` in the local
checkout. Nothing is committed or pushed; do that in the `Env-Plot` repository
to share it. When a dataset is selected whose columns overlap a saved profile by
at least 80 %, the profile's mapping and settings are applied automatically.
Long-format datasets keep the standard plots; profiles are applied on top.

## Tests
```r
testthat::test_dir("tests/testthat")
```

## Files in `data/` and `data_input/`
`.csv`/`.rds` files placed in `data/` or `data_input/` are listed next to uploads.
Files already in explorer format are used as is; others go through
`R/preprocessing.R` (`discover_data_files`, `infer_timeseries_schema`,
`normalize_timeseries`, `detect_timeseries_gaps`), ported from
`Sins-boku/Data-Explorer` `feature/preprocessing` (f3e6e2f). It infers the
timestamp/value/variable columns, refuses ambiguous schemas with a clear error,
treats `;` CSV files as decimal-comma (same as the upload), parses naive
timestamps in `Europe/Vienna`, and flags gaps (> 5x the median cadence).

## Status
See `docu/preprocessing.md`. Done: upload/validation/storage of CSV/RDS, the
loader/schema/normalisation/gap code above and its tests. Plot construction is
in `R/renderer.R`, adapted from `Sins-boku/Env-Plot`
`feature/preprocessing-renderer` (`dc19987`); `app.R` keeps the UI, filtering,
reactivity, and exports. It preserves the existing plot modes and supports
optional `.plot_group` line grouping and summary-band input columns
(`.is_summary`, `.bin_start`, `.bin_end`, `.min`, `.max`). The app still loads
raw data only, so summary-band rendering is dormant until summary generation is
approved and integrated. Renderer regression tests cover grouping, summary
layers, and pairing same-day dual-isotope samples by timestamp.

**Awaiting approval (decision gates)**: storage/cache architecture and
multi-resolution strategy. The source branch's candidate preprocessing is
kept, NOT wired into the app, in `docu/prototypes/` (with benchmark scripts
and candidate tests); see `docu/prototypes/README.md`. The `Env-Plot` submodule
now points to the private `HolgerEX/Env-Plot` fork; the app uses the renderer
adapted into its own `R/` directory, so CI does not need to fetch the private
submodule. Wide-format files (one column per variable) are deliberately
rejected with a message asking the user to reshape to long format, because
variable names/units would otherwise have to be guessed; a column-picker UI
needs a user decision. Benchmarks and the recommendation awaiting approval:
`docu/prototypes/RESULTS.md`. CI: `.github/workflows/ci.yml` (tests + app startup).
