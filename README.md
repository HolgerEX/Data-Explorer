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
immediately. Datasets in `data/*.rds` are listed too.

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
loader/schema/normalisation/gap code above and its tests. **Awaiting approval
(decision gates)**: storage/cache architecture and multi-resolution strategy.
The source branch's candidate implementation is kept, NOT wired into the app, in
`docu/prototypes/` (with a benchmark script and candidate tests); see
`docu/prototypes/README.md`. The `Env-Plot` submodule pointer is unchanged: its
URL is SSH-only/private and was not retrievable, so the source branch's
renderer integration was not ported. Wide-format import is not implemented.
