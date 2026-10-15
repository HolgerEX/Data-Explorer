# Data Explorer

An R Shiny application for exploring environmental datasets and creating publication-ready plots. The app supports time-series, depth-profile, and dual-isotope views, with controls for selecting datasets and variables, filtering series and sampling dates, customizing plot appearance, and exporting figures to PNG or PDF.

## Requirements

- R
- The R packages used by the app: `shiny`, `ggplot2`, `dplyr`, `tidyr`, `purrr`, `stringr`, `readr`, `tibble`, `scales`, and `plotly`

Install the packages with:

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "tidyr", "purrr",
  "stringr", "readr", "tibble", "scales", "plotly"
))
```

## Run the app

From the repository root, start the application with:

```r
shiny::runApp()
```

Or open `app.R` in RStudio and select **Run App**. Static plots are the default. Enable **Interactive time series** in the sidebar to use Plotly hover interactions.

## Prepare data

The current app discovers `.rds` files in a directory named `data_processed/` at the repository root. Create that directory and save each dataset there, for example:

```r
dir.create("data_processed", showWarnings = FALSE)
saveRDS(my_data, "data_processed/my_dataset.rds")
```

Each RDS file must contain:

- `variable`: measurement or variable name
- `value`: numeric measurement
- Either `datetime` or `date`

If a dataset has `date` but no `datetime`, the app creates a datetime at noon using the `Europe/Vienna` time zone. Optional columns such as `unit`, `logger`, `tree`, `trm`, `depth_bottom`, and other categorical identifiers can be used for labels, series splitting, filtering, and faceting. The exact columns available determine which controls and plot modes are useful.

> **Input limitation:** Although `data_input/data_description.txt` describes raw CSV or RDS files, the current `app.R` implementation reads only `.rds` files from `data_processed/`. It does not currently load CSV files or read datasets directly from `data_input/`.

## Plot types and exports

- **Time series:** plot measurements over time as lines, points, bars, or combinations; optionally facet variables or use a secondary axis.
- **Depth profile:** compare measurements by `depth_bottom`, with optional facets and date-based coloring.
- **Dual isotope:** plot two selected variables against each other, with optional reference lines and linear regression.

Use the sidebar to customize filters, colors, labels, axes, and plot dimensions. Static plots support brush-and-double-click zoom. Use the **PNG** and **PDF** buttons to export the current plot.

## Repository layout

```text
.
├── app.R                  # Shiny application
├── data/                  # Described as preprocessed RDS data
├── data_input/            # Described as raw CSV/RDS input
├── docu/                  # Project notes and preprocessing brief
└── Env-Plot/              # Git submodule
```

The `Env-Plot` submodule is recorded in Git, but the current `app.R` does not source or call it. Clone with submodules if you also need its contents:

```bash
git clone --recurse-submodules https://github.com/Sins-boku/Data-Explorer.git
```

For more context, see [`docu/baseapp.md`](docu/baseapp.md) and [`docu/preprocessing.md`](docu/preprocessing.md).
