plot_input <- function(...) {
  defaults <- list(
    plotmode = "ts",
    mode = "single",
    scheme = "default",
    geom = "line",
    alpha = 1,
    lwd = 0.5,
    psize = 2,
    vars = "temperature",
    sec_var = NULL,
    xvar = NULL,
    yvar = NULL,
    gmwl = FALSE,
    lmwl = FALSE,
    fitline = "none",
    fit_eq = FALSE,
    xlab = "",
    ylab = "",
    ylab2 = "",
    title = "",
    xmin = NA_real_,
    xmax = NA_real_,
    xmin_r = NA_real_,
    xmax_r = NA_real_,
    ymin = NA_real_,
    ymax = NA_real_,
    basesize = 12,
    legcols = 1,
    w = 25
  )
  utils::modifyList(defaults, list(...))
}

renderer_data <- function() {
  datetime <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:3
  data.frame(
    dataset = "test",
    datetime = datetime,
    date = as.Date(datetime),
    variable = "temperature",
    value = c(1, 2, 3, 4),
    unit = "C",
    series = "temperature",
    ckey = "temperature"
  )
}

test_that("time-series paths keep the supplied plot groups separate", {
  data <- renderer_data()
  data$.plot_group <- c("logger-a", "logger-a", "logger-b", "logger-b")

  plot <- env_plot_build_plot(data, plot_input())
  layer <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_length(unique(layer$group), 2)
})

test_that("summary rows render as bands, not as ordinary observations", {
  data <- renderer_data()[1:2, ]
  data$.is_summary <- FALSE
  data$.bin_start <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  data$.bin_end <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  data$.min <- NA_real_
  data$.max <- NA_real_
  summary <- data[1, ]
  summary$value <- 99
  summary$.is_summary <- TRUE
  summary$.bin_start <- summary$datetime - 0.5
  summary$.bin_end <- summary$datetime + 0.5
  summary$.min <- 0
  summary$.max <- 100
  data <- rbind(data, summary)

  plot <- env_plot_build_plot(data, plot_input(geom = "point"))
  layers <- ggplot2::ggplot_build(plot)$data

  expect_equal(nrow(layers[[1]]), 2L)
  expect_equal(nrow(layers[[2]]), 1L)
})

test_that("dual-isotope pairs retain separate samples from the same date", {
  datetime <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + c(0, 0, 60, 60)
  data <- data.frame(
    dataset = "isotopes",
    datetime = datetime,
    date = as.Date(datetime),
    variable = c("d18O", "d2H", "d18O", "d2H"),
    value = c(-10, -70, -9, -65),
    unit = "permille",
    series = "all",
    ckey = "all"
  )
  input <- plot_input(
    plotmode = "dual",
    xvar = "d18O",
    yvar = "d2H",
    vars = NULL
  )

  plot <- env_plot_build_plot(data, input)
  layer <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(nrow(layer), 2L)
})
