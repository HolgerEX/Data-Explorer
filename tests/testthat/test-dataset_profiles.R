source(file.path("..", "..", "R", "data_input.R"), local = FALSE)
source(file.path("..", "..", "R", "dataset_profiles.R"), local = FALSE)

command_log <- function() {
  data.frame(
    datetime = c("2023-05-31 10:00:00", "2023-05-31 10:01:00", "2023-05-31 10:02:00"),
    Command = c("a", "b", "a"),
    `Response Time` = c(1.5, 2.5, 3.5),
    `Send Count` = c(1L, 2L, 3L),
    check.names = FALSE
  )
}

test_that("unknown tables are normalised to long format with a basic mapping", {
  long <- normalize_exploratory_dataset(command_log(), "Logger.csv")
  expect_setequal(unique(long$variable), c("Response Time", "Send Count"))
  expect_equal(nrow(long), 6)
  expect_false(isTRUE(attr(long, "time_is_index")))
  expect_true("Command" %in% names(long))
})

test_that("missing time column falls back to row numbers", {
  data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  long <- normalize_exploratory_dataset(data, "x.csv")
  expect_true(attr(long, "time_is_index"))
  expect_equal(nrow(long), 6)
})

test_that("tables without numeric columns are not explorable", {
  expect_error(
    normalize_exploratory_dataset(data.frame(a = c("x", "y")), "x.csv"),
    "no numeric columns"
  )
})

test_that("profiles are saved and matched by close column overlap", {
  dir <- withr::local_tempdir()
  columns <- names(command_log())
  profile <- build_dataset_profile(
    "logger log", columns,
    list(time_column = "datetime", value_columns = "Response Time"),
    list(geom = "point")
  )
  path <- save_dataset_profile(profile, dir)
  expect_true(file.exists(path))
  profiles <- read_dataset_profiles(dir)

  expect_equal(match_dataset_profile(columns, profiles)$settings$geom, "point")
  expect_equal(
    match_dataset_profile(c(columns, "Extra Column"), profiles, 0.7)$name,
    "logger log"
  )
  expect_null(match_dataset_profile(c("x", "y", "z"), profiles))
  expect_null(match_dataset_profile(columns, list()))

  mapped <- normalize_exploratory_dataset(command_log(), "L.csv", profile$mapping)
  expect_equal(unique(mapped$variable), "Response Time")
})

test_that("uploads outside the long format are stored and loadable", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "Logger.csv")
  utils::write.csv(command_log(), csv, row.names = FALSE)
  res <- ingest_upload(csv, "Logger.csv", file.path(dir, "out"))
  expect_true(res$ok)
  long <- load_exploratory_dataset(res$path, profile_dir = file.path(dir, "none"))
  expect_equal(nrow(long), 6)
  expect_setequal(attr(long, "source_columns"), names(command_log()))
})

test_that("saving a profile without a usable name fails clearly", {
  expect_error(
    save_dataset_profile(list(name = "!!!"), withr::local_tempdir()),
    "profile name"
  )
})
