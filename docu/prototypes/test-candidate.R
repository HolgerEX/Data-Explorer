# Candidate tests; run from repo root: Rscript docu/prototypes/test-candidate.R
library(testthat)
source("R/preprocessing.R"); source("docu/prototypes/preprocessing_candidate.R")

test_that("aligned summary levels preserve sufficient statistics and extrema", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  times <- as.POSIXct("2020-01-01", tz = "UTC") + 0:599
  values <- sin(seq_along(times) / 10)
  values[121L] <- 50
  normalized <- normalize_timeseries(
    data.frame(
      datetime = format(times, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      value = values,
      logger = "L1"
    ),
    path,
    list(
      datetime = "datetime",
      value = "value",
      variable = NULL,
      unit = NULL,
      group_cols = "logger",
      timezone = "UTC"
    )
  )
  summaries <- build_resolution_summaries(normalized$data, max_levels = 4L)
  level_zero <- summaries$levels[["0"]]
  level_one <- summaries$levels[["1"]]
  expect_gt(nrow(level_zero), nrow(level_one))

  parent <- level_one[1L, ]
  children <- level_zero[
    level_zero$.series_id == parent$.series_id &
      level_zero$.gap_id == parent$.gap_id &
      level_zero$.bin_start >= parent$.bin_start &
      level_zero$.bin_end <= parent$.bin_end,
  ]
  expect_equal(parent$.count, sum(children$.count))
  expect_equal(parent$.sum, sum(children$.sum), tolerance = 1e-10)
  expect_equal(parent$.min, min(children$.min))
  expect_equal(parent$.max, max(children$.max))
  expect_true(parent$.min_time %in% children$.min_time)
  expect_true(parent$.max_time %in% children$.max_time)
  expect_equal(max(level_zero$.max), 50)
})

test_that("viewport selection uses raw data when feasible and coarser levels otherwise", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  times <- as.POSIXct("2020-01-01", tz = "UTC") + 0:999
  normalized <- normalize_timeseries(
    data.frame(datetime = times, value = seq_along(times)),
    path,
    list(
      datetime = "datetime",
      value = "value",
      variable = NULL,
      unit = NULL,
      group_cols = character(),
      timezone = "UTC"
    )
  )
  summaries <- build_resolution_summaries(normalized$data, max_levels = 8L)
  selection_raw <- select_plot_resolution(
    summaries, times[1L], times[5L], plot_width_px = 100
  )
  expect_identical(selection_raw$level, "raw")

  selection_summary <- select_plot_resolution(
    summaries, times[1L], times[length(times)], plot_width_px = 10
  )
  expect_true(is.numeric(selection_summary$level))
  expect_gt(selection_summary$level, 0)
  expect_error(select_plot_resolution(
    summaries, times[2L], times[1L], plot_width_px = 10
  ), "finite time range")
  expect_error(select_plot_resolution(
    summaries, times[1L], times[2L], plot_width_px = 0
  ), "positive number of pixels")
})

test_that("viewport query returns exact raw edge observations", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  times <- as.POSIXct("2020-01-01", tz = "UTC") + 0:999
  normalized <- normalize_timeseries(
    data.frame(datetime = times, value = seq_along(times)),
    path,
    list(
      datetime = "datetime",
      value = "value",
      variable = NULL,
      unit = NULL,
      group_cols = character(),
      timezone = "UTC"
    )
  )
  summaries <- build_resolution_summaries(normalized$data, max_levels = 8L)
  prepared <- list(
    path = path,
    mapping = list(),
    cache_path = tempfile(fileext = ".rds"),
    summary = summaries,
    raw = normalized$data,
    diagnostics = normalized$diagnostics
  )
  query <- query_timeseries_viewport(
    prepared,
    times[11L] + 0.25,
    times[990L] + 0.25,
    plot_width_px = 10
  )
  expect_gt(query$resolution$level, 0)
  expect_true(all(query$raw$datetime >= times[11L] + 0.25))
  expect_true(all(query$raw$datetime <= times[990L] + 0.25))
  expect_gt(nrow(query$summary), 0L)
})

test_that("persistent summaries are invalidated when source files change", {
  path <- tempfile(fileext = ".csv")
  cache_dir <- tempfile("summary-cache-")
  dir.create(cache_dir)
  on.exit(unlink(c(path, cache_dir), recursive = TRUE))
  writeLines(c(
    "datetime,value",
    "2020-01-01 00:00:00,1",
    "2020-01-01 00:00:01,2"
  ), path)
  first <- load_preprocessed_timeseries(path, cache_dir = cache_dir)
  first_cache <- first$cache_path
  Sys.sleep(1.1)
  writeLines(c(
    "datetime,value",
    "2020-01-01 00:00:00,1",
    "2020-01-01 00:00:01,2",
    "2020-01-01 00:00:02,3"
  ), path)
  second <- load_preprocessed_timeseries(path, cache_dir = cache_dir)
  expect_false(identical(first_cache, second$cache_path))
  expect_equal(sum(second$summary$levels[["0"]]$.count), 3)
})
