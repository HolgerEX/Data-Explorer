# DISPOSABLE benchmark (NOT approved, not sourced by app.R) for the two decision
# gates in docu/preprocessing.md (3.1 storage/cache, 3.2 resolution generation).
# Self-contained: needs only data.table. Reproducible (fixed seed).
#
# Run from repo root:
#   Rscript docu/prototypes/bench_compare.R S1     # 1 series x 1e6 rows, 1-min
#   Rscript docu/prototypes/bench_compare.R S2     # 10 series x 5e5 rows, 10-min, gaps+spikes
# Optional 2nd arg: row-count scale factor (e.g. 0.1 for a quick smoke run).
#
# Cold-start stages run in separate R processes so peak RSS (VmHWM from
# /proc/self/status, Linux only) is per stage and "new process" really is cold.
#
# Statistics stored per bin: count, sum, min, max (mean = sum/count). Extrema
# timestamps (kept by the candidate in preprocessing_candidate.R) are excluded
# here to keep the three designs comparable.

suppressPackageStartupMessages(library(data.table))
setDTthreads(1)  # single-threaded: parallel gain is NOT part of this comparison

args <- commandArgs(TRUE)
FIXED_W <- c(60, 300, 900, 3600, 14400, 86400, 604800)  # each divides the next

rss_peak_mb <- function() {
  l <- tryCatch(readLines("/proc/self/status"), error = function(e) character())
  v <- grep("^VmHWM:", l, value = TRUE)
  if (length(v)) as.numeric(gsub("[^0-9]", "", v)) / 1024 else NA_real_
}
tm <- function(expr) unname(system.time(expr)["elapsed"])
med <- function(f, n = 5) median(vapply(seq_len(n), function(i) tm(f()), 0))

make_data <- function(scn, scale = 1) {
  set.seed(42)
  if (scn == "S1") { ns <- 1L; n <- round(1e6 * scale); step <- 60 }
  else             { ns <- 10L; n <- round(5e5 * scale); step <- 600 }
  t0 <- as.numeric(as.POSIXct("2015-01-01", tz = "UTC"))
  out <- lapply(seq_len(ns), function(s) {
    tt <- t0 + step * seq_len(n)
    v <- cumsum(rnorm(n)) * 0.01 + 10 * sin(tt / 86400 * 2 * pi)
    sp <- sample.int(n, max(1L, round(50 * scale)))     # injected spikes
    v[sp] <- v[sp] + 100
    if (scn == "S2") {                                  # 3 gaps of ~2 days each
      for (g in 1:3) { a <- sample.int(n - 400, 1); v[a:(a + 287)] <- NA }
    }
    data.table(series = paste0("s", s), t = tt, value = v)[!is.na(value)]
  })
  d <- rbindlist(out); setkey(d, series, t); d
}

# ---- Option A: fixed bins, every level aggregated directly from raw ----------
agg <- function(d, w) d[, .(n = .N, s = sum(value), mn = min(value), mx = max(value)),
                        by = .(series, b = floor(t / w) * w)]
build_fixed <- function(d) {
  L <- lapply(FIXED_W, function(w) agg(d, w)); names(L) <- FIXED_W; lapply(L, setkey, series, b)
}
# ---- Option C: same fixed widths, each merged from the previous level --------
merge_up <- function(p, w) p[, .(n = sum(n), s = sum(s), mn = min(mn), mx = max(mx)),
                             by = .(series, b = floor(b / w) * w)]
build_hybrid <- function(d) {
  L <- vector("list", length(FIXED_W)); L[[1]] <- agg(d, FIXED_W[1])
  for (i in 2:length(FIXED_W)) L[[i]] <- merge_up(L[[i - 1]], FIXED_W[i])
  names(L) <- FIXED_W; lapply(L, setkey, series, b)
}
# ---- Option B: power-of-two pyramid, base = 4 x median cadence ---------------
build_pyramid <- function(d, levels = 12L) {
  cad <- d[, .(c = median(diff(t))), by = series]$c
  base <- max(1, 4 * median(cad)); L <- vector("list", levels + 1L)
  L[[1]] <- agg(d, base)
  for (i in seq_len(levels)) L[[i + 1]] <- merge_up(L[[i]], base * 2^i)
  names(L) <- base * 2^(0:levels); lapply(L, setkey, series, b)
}

# viewport query: choose the finest level whose bin count fits the plot width
query_levels <- function(L, a, z, px = 1000) {
  w <- as.numeric(names(L)); i <- which((z - a) / w <= px)[1]; if (is.na(i)) i <- length(L)
  x <- L[[i]]; x[b >= a - w[i] & b <= z]
}
query_raw <- function(d, a, z) d[t >= a & t <= z]

stage <- if (length(args) && args[1] == "stage") args[2] else "orchestrate"

if (stage == "orchestrate") {
  scn <- args[1]; scale <- if (length(args) > 1) as.numeric(args[2]) else 1
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  cat("R:", R.version.string, " data.table:", as.character(packageVersion("data.table")),
      " cores:", parallel::detectCores(), " threads used: 1\n")
  dir <- tempfile("bench"); dir.create(dir)
  run <- function(st, ...) system2("Rscript", c(shQuote(self), "stage", st, scn, scale, dir, ...), stdout = "", stderr = "")
  cat("=== scenario", scn, "scale", scale, "===\n")
  run("generate"); run("designs"); run("storage_build"); run("storage_cold_P"); run("storage_cold_H")
  run("storage_cold_M"); run("accuracy")
  unlink(dir, recursive = TRUE)
  quit(save = "no")
}

scn <- args[3]; scale <- as.numeric(args[4]); dir <- args[5]
raw_csv <- file.path(dir, "raw.csv"); line <- function(...) cat(sprintf("[%s] ", stage), ..., "\n", sep = "")
read_raw <- function() { d <- fread(raw_csv); setkey(d, series, t); d }

if (stage == "generate") {
  d <- make_data(scn, scale)
  fwrite(d, raw_csv)
  line("rows=", nrow(d), " series=", uniqueN(d$series), " csv_MB=", round(file.size(raw_csv) / 1e6, 1),
       " raw_object_MB=", round(as.numeric(object.size(d)) / 1e6, 1),
       " span_days=", round((max(d$t) - min(d$t)) / 86400))
}

if (stage == "designs") {  # 3.2: preprocessing time, size, query latency, structure
  d <- read_raw(); r <- range(d$t)
  vp <- list(full = r, `1y` = c(r[2] - 365 * 86400, r[2]), `1mo` = c(r[2] - 30 * 86400, r[2]),
             `1wk` = c(r[2] - 7 * 86400, r[2]), `1d` = c(r[2] - 86400, r[2]))
  for (nm in c("A_fixed", "B_pyramid", "C_hybrid")) {
    f <- switch(nm, A_fixed = build_fixed, B_pyramid = build_pyramid, C_hybrid = build_hybrid)
    tb <- med(function() f(d), 3); L <- f(d)
    line(nm, " build_s=", round(tb, 2), " levels=", length(L), " bins_total=", sum(vapply(L, nrow, 0L)),
         " summary_MB=", round(as.numeric(object.size(L)) / 1e6, 1),
         " bins_finest=", nrow(L[[1]]))
    q <- vapply(vp, function(v) med(function() query_levels(L, v[1], v[2]), 20) * 1000, 0)
    nb <- vapply(vp, function(v) nrow(query_levels(L, v[1], v[2])), 0L)
    line(nm, " query_ms ", paste(names(q), round(q, 2), collapse = " "),
         " | bins_returned ", paste(names(nb), nb, collapse = " "))
  }
  q <- vapply(vp, function(v) med(function() query_raw(d, v[1], v[2]), 20) * 1000, 0)
  nr <- vapply(vp, function(v) nrow(query_raw(d, v[1], v[2])), 0L)
  line("raw_subset query_ms ", paste(names(q), round(q, 2), collapse = " "),
       " | rows_returned ", paste(names(nr), nr, collapse = " "))
}

if (stage == "storage_build") {  # 3.1: first load for memory-only vs persistent (cold, this process)
  t_read <- tm(d <- read_raw()); t_build <- tm(L <- build_hybrid(d))
  line("M_memory first_load_s=", round(t_read + t_build, 2), " (read_csv=", round(t_read, 2),
       " build=", round(t_build, 2), ")  peak_RSS_MB=", round(rss_peak_mb()))
  for (cmp in c(TRUE, FALSE)) {
    tag <- if (cmp) "gz" else "nocompress"
    ts <- tm(saveRDS(L, file.path(dir, paste0("sum_", tag, ".rds")), compress = cmp))
    tr <- tm(saveRDS(d, file.path(dir, paste0("raw_", tag, ".rds")), compress = cmp))
    line("persist_write ", tag, " summaries_s=", round(ts, 2), " raw_s=", round(tr, 2),
         " summaries_MB=", round(file.size(file.path(dir, paste0("sum_", tag, ".rds"))) / 1e6, 1),
         " raw_MB=", round(file.size(file.path(dir, paste0("raw_", tag, ".rds"))) / 1e6, 1),
         " source_csv_MB=", round(file.size(raw_csv) / 1e6, 1))
  }
}
cold <- function(tag, load_raw) {
  for (cmp in c("gz", "nocompress")) {
    t1 <- tm(L <- readRDS(file.path(dir, paste0("sum_", cmp, ".rds"))))
    t2 <- if (load_raw) tm(d <- readRDS(file.path(dir, paste0("raw_", cmp, ".rds")))) else 0
    t3 <- tm(for (i in 1:20) query_levels(L, 1.4e9, 1.5e9))
    line(tag, " ", cmp, " cold_load_s=", round(t1 + t2, 2), " (summaries=", round(t1, 2), " raw=", round(t2, 2),
         ") first_query_ms=", round(t3 / 20 * 1000, 2), " peak_RSS_MB=", round(rss_peak_mb()))
    rm(L); gc(FALSE)
  }
}
if (stage == "storage_cold_P") cold("P_persist_all (summaries+raw loaded)", TRUE)
if (stage == "storage_cold_H") cold("H_hybrid (summaries only; raw lazily)", FALSE)
if (stage == "storage_cold_M") {  # memory-only, new process: no cache -> must redo everything
  line("M_memory new_process_first_load_s=", round(tm({d <- read_raw(); L <- build_hybrid(d)}), 2),
       " peak_RSS_MB=", round(rss_peak_mb()), " (repeat within same process = in-memory lookup)")
}

if (stage == "accuracy") {  # 3.2: exactness, spikes, local-time (DST) alignment
  d <- read_raw(); A <- build_fixed(d); C <- build_hybrid(d); B <- build_pyramid(d)
  same <- all(vapply(names(A), function(k) isTRUE(all.equal(A[[k]], C[[k]])), TRUE))
  line("A vs C identical at all ", length(A), " levels: ", same)
  # exactness of merged statistics vs direct raw aggregation at the coarsest B level
  k <- names(B)[length(B)]; direct <- agg(d, as.numeric(k)); setkey(direct, series, b)
  line("B coarsest level width_s=", k, " merged==direct(raw): ", isTRUE(all.equal(B[[k]], direct)))
  # spike preservation at ~1000-px resolution: injected +100 spikes vs 1000-bin view
  w <- 86400; L <- A[["86400"]]
  top <- d[value > 50]  # injected spikes dominate all other values (|signal|<~15)
  bn <- top[, .(series, b = floor(t / w) * w)]; j <- L[bn, on = .(series, b)]
  line("daily bins: spikes=", nrow(top), " visible_in_max=", round(mean(j$mx > 50), 3),
       " visible_in_mean=", round(mean(j$s / j$n > 50), 3), " (bins with >1 obs, mean of bin)")
  # local-time (Europe/Vienna) days: derive from hour bins (exact) vs raw
  h <- C[["3600"]]; h[, day := as.IDate(as.POSIXct(b, origin = "1970-01-01", tz = "UTC"), tz = "Europe/Vienna")]
  loc <- h[, .(n = sum(n), s = sum(s), mn = min(mn), mx = max(mx)), by = .(series, day)]
  rawd <- d[, .(n = .N, s = sum(value), mn = min(value), mx = max(value)),
            by = .(series, day = as.IDate(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), tz = "Europe/Vienna"))]
  setkey(loc, series, day); setkey(rawd, series, day)
  line("local (Europe/Vienna) days from hour bins == from raw: ", isTRUE(all.equal(loc, rawd)),
       " (", nrow(loc), " days); UTC 86400s epoch bins cannot be local-day aligned (offset +1h/+2h)")
}
