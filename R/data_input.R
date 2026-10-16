# R/data_input.R
#
# Data input helpers: read an uploaded .csv/.rds, validate it against the
# schema app.R's load_dataset() needs, and store it as .rds in the data folder
# the explorer lists. Pure functions (no Shiny), so they can be tested.

SUPPORTED_EXT <- c("csv", "rds")
MAX_UPLOAD_MB <- 200

# Alternative column names accepted for the required columns.
COLUMN_ALIASES <- list(
  datetime = c("timestamp", "date_time", "datetime_utc", "time", "posixct"),
  date     = c("day"),
  variable = c("var", "parameter", "measurand", "name"),
  value    = c("val", "measurement", "reading")
)

safe_dataset_name <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  base <- gsub("[^A-Za-z0-9_-]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) base <- "dataset"
  base
}

unique_dest_path <- function(dir, name) {
  path <- file.path(dir, paste0(name, ".rds"))
  i <- 1
  while (file.exists(path)) {
    path <- file.path(dir, sprintf("%s_%d.rds", name, i))
    i <- i + 1
  }
  path
}

# Rename alias columns to the canonical names (only if canonical is absent).
apply_column_aliases <- function(d) {
  nm <- names(d)
  low <- tolower(trimws(nm))
  for (canon in names(COLUMN_ALIASES)) {
    if (canon %in% low) {
      nm[low == canon] <- canon
    } else {
      hit <- which(low %in% COLUMN_ALIASES[[canon]])
      if (length(hit)) nm[hit[1]] <- canon
    }
  }
  names(d) <- nm
  d
}

read_dataset_file <- function(path, filename = basename(path)) {
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% SUPPORTED_EXT)
    stop(sprintf("Unsupported file type '.%s'. Upload a .csv or .rds file.", ext),
         call. = FALSE)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path))
    stop(sprintf(
      "Uploaded file '%s' is no longer available. Please upload it again.",
      basename(filename)
    ), call. = FALSE)
  if (dir.exists(path))
    stop(sprintf("Uploaded path for '%s' is a directory, not a file.", basename(filename)),
         call. = FALSE)
  if (file.access(path, mode = 4L) != 0L)
    stop(sprintf("Uploaded file '%s' cannot be read. Check its permissions and upload it again.",
                 basename(filename)), call. = FALSE)
  if (file.size(path) > MAX_UPLOAD_MB * 1024^2)
    stop(sprintf("File is larger than %d MB.", MAX_UPLOAD_MB), call. = FALSE)
  d <- if (ext == "rds") {
    tryCatch(
      readRDS(path),
      error = function(e) stop(sprintf(
        "Could not open RDS file '%s': %s. Check that the upload completed and the file is a valid RDS.",
        basename(filename), conditionMessage(e)
      ), call. = FALSE)
    )
  } else {
    first <- readLines(path, n = 1, warn = FALSE)
    delim <- if (length(first) && lengths(regmatches(first, gregexpr(";", first))) >
                 lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
    header <- trimws(gsub("^\"|\"$", "", strsplit(first, delim, fixed = TRUE)[[1]]))
    var_cols <- header[tolower(header) %in% c("variable", COLUMN_ALIASES$variable)]
    col_spec <- do.call(readr::cols, c(
      stats::setNames(rep(list(readr::col_character()), length(var_cols)), var_cols),
      list(.default = readr::col_guess())))
    readr::read_delim(path, delim = delim, show_col_types = FALSE,
                      progress = FALSE, guess_max = 10000,
                      col_types = col_spec,
                      locale = readr::locale(decimal_mark = if (delim == ";") "," else ".",
                                             grouping_mark = if (delim == ";") "." else ","))
  }
  if (!is.data.frame(d))
    stop("The file does not contain a table (data frame).", call. = FALSE)
  as.data.frame(d, stringsAsFactors = FALSE)
}

# Returns list(ok, data, errors, warnings, n_dropped). Never modifies source file.
validate_dataset <- function(d, tz = "Europe/Vienna") {
  errors <- character(); warnings <- character()
  fail <- function() list(ok = FALSE, data = NULL, errors = errors,
                          warnings = warnings, n_dropped = 0L)
  if (nrow(d) == 0) { errors <- "The table has no rows."; return(fail()) }
  d <- apply_column_aliases(d)

  missing_measurements <- setdiff(c("variable", "value"), names(d))
  if (length(missing_measurements)) {
    columns <- paste(names(d), collapse = ", ")
    if (length(missing_measurements) == 2L) {
      errors <- c(errors, sprintf(
        paste0(
          "Missing measurement columns 'variable' and 'value'. Found columns: %s. ",
          "Expected long format (one row per measurement, with datetime, variable, and value). ",
          "This may be a wide-format file or a non-measurement log; columns are not guessed automatically."
        ),
        columns
      ))
    } else {
      errors <- c(errors, sprintf(
        "Missing required column '%s'. Found columns: %s. Data must be in long format (one row per measurement).",
        missing_measurements, columns
      ))
    }
  }
  if (!any(c("datetime", "date") %in% names(d)))
    errors <- c(errors, "Missing a time column: provide 'datetime' or 'date'.")
  if (length(errors)) {
    num <- setdiff(names(d)[vapply(d, is.numeric, NA)], c("variable", "value"))
    if (length(missing_measurements) == 2L &&
        any(c("datetime", "date") %in% names(d)) && length(num))
      errors <- c(errors, sprintf(
        "Numeric columns found: %s. Wide files are not imported automatically because variable names, units and metadata would have to be guessed; reshape to one row per measurement (columns datetime, variable, value), e.g. with tidyr::pivot_longer().",
        paste(head(num, 10), collapse = ", ")))
    return(fail())
  }

  if (!is.numeric(d$value)) {
    v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(d$value), fixed = TRUE)))
    bad <- sum(is.na(v) & !is.na(d$value) & nzchar(as.character(d$value)))
    if (bad > 0.5 * nrow(d)) {
      errors <- c(errors, "Column 'value' is not numeric (more than half of the entries cannot be converted).")
      return(fail())
    }
    if (bad > 0) warnings <- c(warnings, sprintf("%d non-numeric 'value' entries set to NA.", bad))
    d$value <- v
  }
  d$variable <- as.character(d$variable)

  parse_time <- function(x) {
    if (inherits(x, "POSIXct")) return(x)
    if (inherits(x, "Date")) return(as.POSIXct(paste(x, "12:00:00"), tz = tz))
    x <- as.character(x)
    as.POSIXct(x, tz = tz, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%d %H:%M", "%Y-%m-%d", "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M",
      "%d.%m.%Y", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d"), optional = TRUE)
  }
  tcol <- if ("datetime" %in% names(d)) "datetime" else "date"
  t <- tryCatch(suppressWarnings(parse_time(d[[tcol]])), error = function(e) NULL)
  if (is.null(t) || all(is.na(t))) {
    errors <- c(errors, sprintf(
      "Column '%s' could not be read as dates. Use ISO format, e.g. 2023-05-31 14:00:00.", tcol))
    return(fail())
  }
  unparsed <- sum(is.na(t) & !is.na(d[[tcol]]))
  if (unparsed > 0)
    warnings <- c(warnings, sprintf("%d rows with unreadable '%s' were dropped.", unparsed, tcol))
  if (tcol == "datetime") d$datetime <- t else d$date <- as.Date(t, tz = tz)
  if (tcol == "datetime") d <- d[!is.na(d$datetime), , drop = FALSE]
  else d <- d[!is.na(d$date), , drop = FALSE]
  if (!nrow(d)) { errors <- "No rows remain after removing unreadable dates."; return(fail()) }
  list(ok = TRUE, data = d, errors = errors, warnings = warnings,
       n_dropped = as.integer(unparsed))
}

# Read + validate + store. Returns list(ok, path, message, warnings).
ingest_upload <- function(path, filename, dest_dir, tz = "Europe/Vienna") {
  res <- tryCatch({
    d <- read_dataset_file(path, filename)
    v <- validate_dataset(d, tz)
    if (!v$ok) {
      # Not the documented long format: keep the raw table if it has at least
      # one numeric column, so it can still be explored with a basic plot.
      explorable <- tryCatch(
        normalize_exploratory_dataset(d, filename, timezone = tz),
        error = function(e) NULL
      )
      if (is.null(explorable)) {
        return(list(ok = FALSE, path = NULL,
                    message = paste(c(v$errors, "No numeric columns to plot."),
                                    collapse = " "),
                    warnings = character()))
      }
      if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
      dest <- unique_dest_path(dest_dir, safe_dataset_name(filename))
      saveRDS(d, dest)
      return(list(
        ok = TRUE,
        path = dest,
        message = sprintf(
          "Loaded %s as an exploratory table (%d rows, %d numeric columns) with a basic plot.",
          basename(dest), nrow(d), length(unique(explorable$variable))
        ),
        warnings = paste("Not in long format:", paste(v$errors, collapse = " "))
      ))
    }
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    dest <- unique_dest_path(dest_dir, safe_dataset_name(filename))
    saveRDS(v$data, dest)
    list(ok = TRUE, path = dest,
         message = sprintf("Loaded %s: %d rows, %d variables.", basename(dest),
                           nrow(v$data), length(unique(v$data$variable))),
         warnings = v$warnings)
  }, error = function(e) list(ok = FALSE, path = NULL,
         message = paste("Could not read file:", conditionMessage(e)), warnings = character()))
  res
}
