source(file.path("..", "..", "R", "data_input.R"))

write_csv_tmp <- function(txt) { f <- tempfile(fileext = ".csv"); writeLines(txt, f); f }

test_that("valid long CSV is stored as rds and loadable", {
  f <- write_csv_tmp(c("datetime,variable,value,logger",
                       "2023-05-31 14:00:00,temp,1.5,A", "2023-06-01 14:00:00,temp,2.5,A"))
  dest <- tempfile(); r <- ingest_upload(f, "My Data.csv", dest)
  expect_true(r$ok); expect_true(file.exists(r$path))
  expect_equal(basename(r$path), "My_Data.rds")
  d <- readRDS(r$path); expect_s3_class(d$datetime, "POSIXct"); expect_equal(nrow(d), 2)
})

test_that("semicolon CSV, aliases and decimal commas work", {
  f <- write_csv_tmp(c("timestamp;parameter;val", "31.05.2023 14:00;temp;\"1,5\"", "01.06.2023 14:00;temp;\"2,5\""))
  r <- ingest_upload(f, "x.csv", tempfile()); expect_true(r$ok)
  expect_equal(readRDS(r$path)$value, c(1.5, 2.5))
})

test_that("rds input and date-only input work", {
  f <- tempfile(fileext = ".rds")
  saveRDS(data.frame(date = as.Date("2023-01-01") + 0:2, variable = "p", value = 1:3), f)
  expect_true(ingest_upload(f, "p.rds", tempfile())$ok)
})

test_that("missing columns give actionable errors", {
  f <- write_csv_tmp(c("a,b", "1,2"))
  r <- ingest_upload(f, "bad.csv", tempfile())
  expect_false(r$ok); expect_match(r$message, "variable"); expect_match(r$message, "Found columns")
})

test_that("bad extension, bad dates and non-numeric values are rejected", {
  f <- write_csv_tmp("a"); expect_false(ingest_upload(f, "x.xlsx", tempfile())$ok)
  f <- write_csv_tmp(c("datetime,variable,value", "foo,t,1", "bar,t,2"))
  expect_match(ingest_upload(f, "x.csv", tempfile())$message, "dates")
  f <- write_csv_tmp(c("datetime,variable,value", "2023-01-01,t,abc", "2023-01-02,t,def"))
  expect_match(ingest_upload(f, "x.csv", tempfile())$message, "numeric")
})

test_that("existing files are never overwritten and names are sanitised", {
  d <- tempfile(); f <- write_csv_tmp(c("date,variable,value", "2023-01-01,t,1"))
  a <- ingest_upload(f, "../../evil name.csv", d); b <- ingest_upload(f, "../../evil name.csv", d)
  expect_true(a$ok && b$ok); expect_false(a$path == b$path)
  expect_equal(dirname(a$path), d)
})

test_that("stored output satisfies app.R load_dataset()", {
  src <- readLines(file.path("..", "..", "app.R"))
  s <- grep("^load_dataset <- function", src); e <- grep("^# include_variable", src) - 1
  suppressPackageStartupMessages({library(dplyr); library(stringr); library(purrr)})
  eval(parse(text = src[s:e]))
  ID_CANDIDATES <- c("logger")
  f <- write_csv_tmp(c("datetime,variable,value,logger", "2023-05-31 14:00:00,temp,1.5,A"))
  r <- ingest_upload(f, "t.csv", tempfile())
  expect_equal(nrow(load_dataset(r$path)), 1)
})

test_that("variable names like 't' are not guessed as logical", {
  f <- write_csv_tmp(c("datetime,variable,value", "2023-01-01 10:00:00,t,1"))
  expect_equal(readRDS(ingest_upload(f, "t.csv", tempfile())$path)$variable, "t")
})
