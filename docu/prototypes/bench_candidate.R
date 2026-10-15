# Run from repo root: Rscript docu/prototypes/bench_candidate.R [n_rows]
# Times the candidate summary build on synthetic 1-min data (1 series).
source("R/preprocessing.R"); source("docu/prototypes/preprocessing_candidate.R")
n <- as.numeric(commandArgs(TRUE)[1]); if (is.na(n)) n <- 1e6
t <- as.POSIXct("2020-01-01", tz = "UTC") + 60 * seq_len(n)
d <- data.frame(datetime = t, value = cumsum(rnorm(n)), logger = "L1")
f <- tempfile(fileext = ".csv"); data.table::fwrite(d, f)
cat("rows:", n, " csv MB:", round(file.size(f) / 1e6, 1), "\n")
print(system.time(r <- read_timeseries_file(f))["elapsed"])
print(system.time(s <- build_resolution_summaries(r$data))["elapsed"])
cat("levels:", length(s$levels), " level0 rows:", nrow(s$levels[["0"]]), "\n")
cat("raw MB:", round(s$raw_object_bytes / 1e6, 1), " summary MB:", round(as.numeric(object.size(s$levels)) / 1e6, 1), "\n")
