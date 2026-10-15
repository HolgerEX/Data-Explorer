test_that("CSV normalization preserves duplicate timestamps and reports gaps", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "datetime,value,logger,unit",
    "2020-01-01 00:00:00,1,L1,C",
    "2020-01-01 00:00:01,2,L1,C",
    "2020-01-01 00:00:01,4,L1,C",
    "2020-01-01 00:00:02,NA,L1,C",
    "2020-01-01 00:01:00,100,L1,C",
    "2020-01-01 00:01:01,3,L1,C"
  ), path)
  on.exit(unlink(path))

  result <- read_timeseries_file(path)
  expect_equal(nrow(result$data), 6L)
  expect_equal(result$diagnostics$duplicate_timestamp_rows, 1L)
  expect_equal(result$diagnostics$gap_count, 1L)
  expect_equal(sum(is.na(result$data$value)), 1L)
  expect_equal(result$data$.gap_id[5L], result$data$.gap_id[4L] + 1L)
  expect_true(all(diff(as.numeric(result$data$datetime)) >= 0))
})

test_that("CSV and RDS loaders identify supported schemas", {
  csv <- tempfile(fileext = ".csv")
  rds <- tempfile(fileext = ".rds")
  on.exit(unlink(c(csv, rds)))
  table <- data.frame(
    timestamp = c("2022-03-01 00:00:00", "2022-03-01 00:00:01"),
    value = c(1.5, 2.5),
    variable = c("temperature", "temperature"),
    unit = c("C", "C")
  )
  utils::write.csv(table, csv, row.names = FALSE)
  saveRDS(table, rds)

  expect_equal(read_timeseries_file(csv)$data$value, c(1.5, 2.5))
  expect_equal(read_timeseries_file(rds)$data$variable,
               c("temperature", "temperature"))
})

test_that("ambiguous schemas require an explicit mapping", {
  table <- data.frame(
    timestamp = c("2020-01-01", "2020-01-02"),
    date = c("2020-01-01", "2020-01-02"),
    temperature = c(1, 2),
    pressure = c(10, 11)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(table, path, row.names = FALSE)

  expect_error(read_timeseries_file(path), "Ambiguous datetime columns")
  mapped <- read_timeseries_file(path, list(
    datetime = "timestamp",
    value = "temperature"
  ))
  expect_equal(mapped$data$value, c(1, 2))
})

test_that("timestamps respect the selected timezone", {
  parsed <- parse_timeseries_datetime(
    c("2020-01-01 12:00:00", "2020-07-01 12:00:00"),
    "Europe/Vienna",
    "timezone.csv"
  )
  expect_equal(format(parsed, "%H:%M:%S", tz = "Europe/Vienna"),
               c("12:00:00", "12:00:00"))
  expect_equal(
    as.numeric(diff(parsed), units = "secs"),
    182L * 86400 - 3600
  )
})

test_that("semicolon CSV with decimal commas keeps its values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("timestamp;parameter;val",
               "31.05.2023 14:00;temp;\"1,5\"",
               "01.06.2023 14:00;temp;\"2,5\""), path)
  r <- read_timeseries_file(path, list(value = "val", variable = "parameter"))
  expect_equal(r$data$value, c(1.5, 2.5))
  expect_equal(r$diagnostics$invalid_value_rows, 0L)
  writeLines(c("datetime;value", "2023-05-31 14:00:00;1,5", "2023-05-31 15:00:00;2,5"), path)
  expect_equal(read_timeseries_file(path)$data$value, c(1.5, 2.5))
})

test_that("a datetime column wins over an additional date column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("datetime,date,variable,value",
               "2023-05-31 14:00:00,2023-05-31,t,1",
               "2023-05-31 15:00:00,2023-05-31,t,2"), path)
  r <- read_timeseries_file(path)
  expect_equal(r$schema$datetime, "datetime")
  expect_equal(format(r$data$datetime, "%H", tz = "Europe/Vienna"), c("14", "15"))
})

test_that("files in data_input/ and data/ are discovered and reach the explorer", {
  root <- tempfile("proj-"); dir.create(file.path(root, "data_input"), recursive = TRUE)
  dir.create(file.path(root, "data"))
  on.exit(unlink(root, recursive = TRUE))
  writeLines(c("time,reading", "2023-01-01 00:00:00,1", "2023-01-01 01:00:00,2"),
             file.path(root, "data_input", "a.csv"))
  saveRDS(data.frame(datetime = as.POSIXct("2023-01-01", tz = "UTC") + 0:1,
                     variable = "p", value = 1:2), file.path(root, "data", "b.rds"))
  writeLines("x", file.path(root, "data", "notes.txt"))
  found <- discover_data_files(file.path(root, c("data_input", "data")))
  expect_setequal(found$dataset, c("a", "b"))
  d <- load_timeseries_for_explorer(found$path[found$dataset == "a"])
  expect_true(all(c("dataset", "datetime", "date", "variable", "value", "unit") %in% names(d)))
  expect_equal(d$value, c(1, 2))
  expect_error(read_timeseries_file(file.path(root, "data", "notes.txt")), "Unsupported")
})
