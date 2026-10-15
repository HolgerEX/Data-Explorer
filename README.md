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

## Status
See `docu/preprocessing.md`. Done: upload/validation/storage of CSV/RDS into the
explorer's data path. Not done (awaiting design approval, see
`docu/preprocessing.md` decision gates): multi-resolution preprocessing,
storage/cache architecture, resolution-generation strategy, benchmarks, and the
`Env-Plot` submodule integration (the submodule is empty in this checkout and
its URL is SSH-only/private). Wide-format import is not implemented.
