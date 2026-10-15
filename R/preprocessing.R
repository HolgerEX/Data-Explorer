PREPROCESSING_VERSION <- "1.0.0"

# Default zone for naive timestamps; same convention as R/data_input.R.
PREPROCESSING_DEFAULT_TZ <- "Europe/Vienna"

PREPROCESSING_ID_COLUMNS <- c(
  "logger", "position", "source", "trm", "tree", "core_id", "probe_set",
  "depth_bottom", "depth_top", "station", "sensor", "channel"
)

PREPROCESSING_MEMORY_CACHE <- new.env(parent = emptyenv())
PREPROCESSING_MEMORY_CACHE_ORDER <- character()

preprocessing_abort <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("preprocessing_error", "error", "condition")
  ))
}

discover_data_files <- function(directories = c("data_input", "data")) {
  existing <- directories[dir.exists(directories)]
  if (!length(existing)) {
    preprocessing_abort(paste0(
      "No input directories were found. Create one of: ",
      paste(directories, collapse = ", ")
    ))
  }

  paths <- unlist(lapply(existing, function(directory) {
    list.files(
      directory,
      pattern = "\\.(csv|rds)$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE
    )
  }), use.names = FALSE)
  paths <- paths[!startsWith(basename(paths), ".")]
  if (!length(paths)) {
    return(data.frame(
      path = character(),
      dataset = character(),
      format = character(),
      size_bytes = numeric(),
      modified = as.POSIXct(character(), tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(paths)
  keep <- !is.na(info$isdir) & !info$isdir & !is.na(info$size)
  paths <- paths[keep]
  info <- info[keep, , drop = FALSE]
  if (!length(paths)) {
    return(data.frame(
      path = character(),
      dataset = character(),
      format = character(),
      size_bytes = numeric(),
      modified = as.POSIXct(character(), tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    path = normalizePath(paths, winslash = "/", mustWork = TRUE),
    dataset = tools::file_path_sans_ext(basename(paths)),
    format = tolower(tools::file_ext(paths)),
    size_bytes = as.numeric(info$size),
    modified = as.POSIXct(info$mtime, tz = "UTC"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

read_timeseries_table <- function(path, header_only = FALSE) {
  extension <- tolower(tools::file_ext(path))
  if (identical(extension, "csv")) {
    # Same convention as R/data_input.R: ';' separated files use decimal commas.
    first <- readLines(path, n = 1L, warn = FALSE)
    n_semicolon <- if (length(first)) lengths(regmatches(first, gregexpr(";", first))) else 0L
    n_comma <- if (length(first)) lengths(regmatches(first, gregexpr(",", first))) else 0L
    semicolon <- n_semicolon > n_comma
    table <- data.table::fread(
      path,
      sep = if (semicolon) ";" else ",",
      dec = if (semicolon) "," else ".",
      nrows = if (header_only) 0L else -1L,
      check.names = FALSE,
      data.table = FALSE,
      showProgress = FALSE,
      nThread = 1L,
      na.strings = c("", "NA", "NaN")
    )
    if (header_only) return(names(table))
    # fread turns ISO timestamps into UTC POSIXct; restore the wall-clock text
    # so the schema's timezone is applied to what the file actually contains.
    for (column in names(table)) {
      if (inherits(table[[column]], "POSIXct")) {
        table[[column]] <- format(table[[column]], "%Y-%m-%d %H:%M:%OS", tz = "UTC")
      }
    }
    return(table)
  }
  if (!identical(extension, "rds")) {
    preprocessing_abort(paste0(
      "Unsupported file type: ", extension, ". Choose a CSV or RDS file."
    ))
  }

  object <- tryCatch(
    readRDS(path),
    error = function(error) {
      preprocessing_abort(paste0(
        "Could not read ", basename(path), ": ", conditionMessage(error)
      ))
    }
  )
  if (!is.data.frame(object)) {
    preprocessing_abort(paste0(
      basename(path), " contains a ", paste(class(object), collapse = "/"),
      " object; expected a data frame or tibble."
    ))
  }
  if (header_only) names(object) else as.data.frame(object, stringsAsFactors = FALSE)
}

infer_timeseries_schema <- function(data, path, mapping = list()) {
  columns <- names(data)
  if (is.null(columns) || !length(columns)) {
    preprocessing_abort(paste0(basename(path), " has no columns."))
  }
  normalized_names <- tolower(gsub("[^a-z0-9]+", "_", columns))

  pick <- function(field, candidates, preferred = character()) {
    explicit <- mapping[[field]]
    if (!is.null(explicit) && length(explicit) && nzchar(explicit)) {
      if (!explicit %in% columns) {
        preprocessing_abort(paste0(
          "Mapped ", field, " column '", explicit,
          "' does not exist in ", basename(path), "."
        ))
      }
      return(explicit)
    }

    matched <- columns[normalized_names %in% candidates]
    if (length(preferred)) {
      preferred_matches <- intersect(preferred, matched)
      if (length(preferred_matches) == 1L) return(preferred_matches)
    }
    if (length(matched) == 1L) return(matched)
    if (length(matched) > 1L) {
      preprocessing_abort(paste0(
        "Ambiguous ", field, " columns in ", basename(path), ": ",
        paste(matched, collapse = ", "),
        ". Specify the column explicitly in the schema mapping."
      ))
    }
    NULL
  }

  datetime <- pick(
    "datetime",
    c("datetime", "timestamp", "time", "date_time", "date", "sample_time"),
    preferred = "datetime"
  )
  if (is.null(datetime)) {
    preprocessing_abort(paste0(
      "No timestamp column was identified in ", basename(path),
      ". Map a column named datetime, timestamp, time, or date."
    ))
  }

  value <- pick(
    "value",
    c("value", "measurement", "reading", "observation", "result"),
    preferred = "value"
  )
  if (is.null(value)) {
    if (ncol(data) > 0L && !is.null(dim(data))) {
      numeric_columns <- columns[vapply(data, function(column) {
        is.numeric(column) || is.integer(column)
      }, logical(1))]
      numeric_columns <- setdiff(numeric_columns, datetime)
      if (length(numeric_columns) == 1L) value <- numeric_columns
      if (length(numeric_columns) > 1L) {
        preprocessing_abort(paste0(
          "Ambiguous measurement columns in ", basename(path), ": ",
          paste(numeric_columns, collapse = ", "),
          ". Specify the value column in the schema mapping."
        ))
      }
    }
  }
  if (is.null(value)) {
    preprocessing_abort(paste0(
      "No numeric measurement column was identified in ", basename(path),
      ". Map the value column explicitly."
    ))
  }

  variable <- pick(
    "variable",
    c("variable", "parameter", "measurement_type", "measurand"),
    preferred = "variable"
  )
  unit <- pick("unit", c("unit", "units"), preferred = "unit")
  group_columns <- mapping$group_cols
  if (is.null(group_columns)) {
    group_columns <- intersect(PREPROCESSING_ID_COLUMNS, columns)
  }
  unknown_groups <- setdiff(group_columns, columns)
  if (length(unknown_groups)) {
    preprocessing_abort(paste0(
      "Unknown grouping columns: ", paste(unknown_groups, collapse = ", ")
    ))
  }
  group_columns <- setdiff(group_columns, c(datetime, value, variable, unit))

  list(
    datetime = datetime,
    value = value,
    variable = variable,
    unit = unit,
    group_cols = group_columns,
    timezone = mapping$timezone %||% PREPROCESSING_DEFAULT_TZ
  )
}

`%||%` <- function(value, default) {
  if (is.null(value)) default else value
}

parse_timeseries_datetime <- function(value, timezone, path) {
  if (length(timezone) != 1L || is.na(timezone) ||
      !timezone %in% c("UTC", "GMT", OlsonNames())) {
    preprocessing_abort(paste0("Invalid timezone: ", timezone))
  }
  if (inherits(value, "POSIXct")) {
    parsed <- as.POSIXct(value, tz = timezone)
  } else if (inherits(value, "POSIXlt")) {
    parsed <- as.POSIXct(value, tz = timezone)
  } else if (inherits(value, "Date")) {
    parsed <- as.POSIXct(value, tz = timezone)
  } else if (is.numeric(value)) {
    parsed <- as.POSIXct(value, origin = "1970-01-01", tz = timezone)
  } else {
    text <- trimws(as.character(value))
    text[!nzchar(text)] <- NA_character_
    formats <- c(
      "%Y-%m-%dT%H:%M:%OS%z",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%d %H:%M:%OS%z",
      "%Y-%m-%d %H:%M:%OS",
      "%Y-%m-%d",
      "%d.%m.%Y %H:%M:%OS",
      "%d.%m.%Y",
      "%m/%d/%Y %H:%M:%OS",
      "%m/%d/%Y"
    )
    parsed <- as.POSIXct(rep(NA_real_, length(text)),
                         origin = "1970-01-01", tz = timezone)
    for (format in formats) {
      missing <- is.na(parsed) & !is.na(text)
      if (!any(missing)) break
      candidate <- as.POSIXct(
        text[missing],
        format = format,
        tz = timezone
      )
      parsed[missing] <- candidate
    }
  }
  if (!any(!is.na(parsed))) {
    preprocessing_abort(paste0(
      "No timestamps in ", basename(path),
      " could be parsed in timezone ", timezone, "."
    ))
  }
  parsed
}

make_timeseries_series_id <- function(data, columns) {
  if (!length(columns)) {
    return(rep("all", nrow(data)))
  }
  encoded <- lapply(columns, function(column) {
    value <- as.character(data[[column]])
    missing <- is.na(value)
    encoded_value <- paste0(nchar(value, type = "bytes"), ":", value)
    encoded_value[missing] <- "-1:"
    encoded_value
  })
  do.call(paste, c(encoded, sep = "|"))
}

normalize_timeseries <- function(data, path, schema, source_id = path,
                                 gap_factor = 5) {
  required <- c(schema$datetime, schema$value, schema$group_cols)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    preprocessing_abort(paste0(
      "Required columns are missing from ", basename(path), ": ",
      paste(missing, collapse = ", ")
    ))
  }
  timezone <- schema$timezone %||% PREPROCESSING_DEFAULT_TZ
  datetime <- parse_timeseries_datetime(data[[schema$datetime]], timezone, path)
  value_text <- data[[schema$value]]
  value <- if (is.numeric(value_text)) {
    as.numeric(value_text)
  } else {
    suppressWarnings(as.numeric(gsub(",", ".", as.character(value_text), fixed = TRUE)))
  }
  invalid_value <- !is.na(value_text) & is.na(value)
  infinite_value <- !is.na(value) & !is.finite(value)
  value[infinite_value] <- NA_real_
  value[invalid_value] <- NA_real_

  dataset <- tools::file_path_sans_ext(basename(path))
  variable <- if (!is.null(schema$variable) && nzchar(schema$variable)) {
    as.character(data[[schema$variable]])
  } else {
    rep(dataset, nrow(data))
  }
  unit <- if (!is.null(schema$unit) && nzchar(schema$unit)) {
    as.character(data[[schema$unit]])
  } else {
    rep(NA_character_, nrow(data))
  }
  out <- data.frame(
    dataset = rep(dataset, nrow(data)),
    source_file = rep(normalizePath(path, winslash = "/", mustWork = FALSE), nrow(data)),
    datetime = datetime,
    date = as.Date(datetime, tz = timezone),
    time_zone = rep(timezone, nrow(data)),
    variable = variable,
    value = value,
    unit = unit,
    stringsAsFactors = FALSE
  )
  for (column in schema$group_cols) {
    if (!column %in% names(out)) out[[column]] <- data[[column]]
  }

  series_columns <- c("source_file", "dataset", "variable", "unit", schema$group_cols)
  out$.series_id <- make_timeseries_series_id(out, series_columns)
  valid_time <- !is.na(out$datetime)
  dropped_time <- sum(!valid_time)
  out <- out[valid_time, , drop = FALSE]
  if (!nrow(out)) {
    preprocessing_abort(paste0(
      "No rows with valid timestamps remain in ", basename(path), "."
    ))
  }
  data.table::setorderv(out, c(".series_id", "datetime"), na.last = TRUE)
  row.names(out) <- NULL

  duplicate_key <- paste(out$.series_id, as.numeric(out$datetime), sep = "\r")
  duplicate_count <- sum(duplicated(duplicate_key))
  gaps <- detect_timeseries_gaps(out, gap_factor = gap_factor)
  out <- gaps$data
  valid_value_count <- sum(is.finite(out$value), na.rm = TRUE)
  if (!valid_value_count) {
    preprocessing_abort(paste0(
      "No finite numeric measurements remain in ", basename(path),
      ". Check the mapped value column."
    ))
  }

  diagnostics <- gaps$diagnostics
  diagnostics$input_rows <- nrow(data)
  diagnostics$valid_timestamp_rows <- nrow(out)
  diagnostics$dropped_timestamp_rows <- dropped_time
  diagnostics$valid_value_rows <- valid_value_count
  diagnostics$invalid_value_rows <- sum(invalid_value)
  diagnostics$non_finite_value_rows <- sum(infinite_value)
  diagnostics$duplicate_timestamp_rows <- duplicate_count
  attr(out, "diagnostics") <- diagnostics
  list(
    data = out,
    schema = schema,
    diagnostics = diagnostics,
    source_id = source_id
  )
}

detect_timeseries_gaps <- function(data, gap_factor = 5) {
  if (!is.finite(gap_factor) || gap_factor <= 1) {
    preprocessing_abort("The gap factor must be a finite number greater than 1.")
  }
  if (!all(c(".series_id", "datetime", "value") %in% names(data))) {
    preprocessing_abort(
      "Gap detection requires .series_id, datetime, and value columns."
    )
  }
  out <- data.table::as.data.table(data)
  data.table::setorderv(out, c(".series_id", "datetime"))
  out[, .time_seconds := as.numeric(datetime)]
  out[, .delta_seconds := .time_seconds - data.table::shift(.time_seconds),
      by = .series_id]
  cadence <- out[.delta_seconds > 0,
                 .(cadence_seconds = stats::median(.delta_seconds)),
                 by = .series_id]
  cadence[, .gap_threshold_seconds := cadence_seconds * gap_factor]
  out <- merge(out, cadence, by = ".series_id", all.x = TRUE, sort = FALSE)
  data.table::setorderv(out, c(".series_id", "datetime"))
  out[, .gap_before := !is.na(.delta_seconds) &
        !is.na(.gap_threshold_seconds) &
        .delta_seconds > .gap_threshold_seconds]
  out[, .segment_break := .gap_before |
        !is.finite(value) |
        data.table::shift(!is.finite(value), fill = FALSE),
      by = .series_id]
  out[, .gap_id := cumsum(.segment_break), by = .series_id]
  out[, .segment_break := NULL]
  out[, .time_seconds := NULL]
  diagnostics <- out[, .(
    row_count = .N,
    distinct_timestamp_count = data.table::uniqueN(datetime),
    first_timestamp = min(datetime),
    last_timestamp = max(datetime),
    cadence_seconds = if (all(is.na(cadence_seconds))) NA_real_
    else stats::median(cadence_seconds, na.rm = TRUE),
    gap_threshold_seconds = if (all(is.na(.gap_threshold_seconds))) NA_real_
    else stats::median(.gap_threshold_seconds, na.rm = TRUE),
    gap_count = sum(.gap_before, na.rm = TRUE)
  ), by = .series_id]
  out[, .delta_seconds := NULL]
  out[, .gap_threshold_seconds := NULL]
  list(data = as.data.frame(out), diagnostics = as.data.frame(diagnostics))
}

read_timeseries_file <- function(path, mapping = list(), gap_factor = 5) {
  if (!file.exists(path)) {
    preprocessing_abort(paste0("Input file does not exist: ", path))
  }
  table <- read_timeseries_table(path)
  if (!nrow(table)) {
    preprocessing_abort(paste0("Input file is empty: ", basename(path)))
  }
  schema <- infer_timeseries_schema(table, path, mapping)
  normalize_timeseries(
    table,
    path,
    schema,
    source_id = path,
    gap_factor = gap_factor
  )
}

# Explorer-format table (dataset, datetime, date, variable, value, unit, ids)
# from any CSV/RDS the schema inference understands. Used by app.R for files
# found in data_input/ and data/ that are not already in explorer format.
load_timeseries_for_explorer <- function(path, mapping = list()) {
  result <- read_timeseries_file(path, mapping)
  d <- result$data
  d <- d[!is.na(d$datetime), , drop = FALSE]
  drop <- c("source_file", "time_zone", "cadence_seconds")
  d[, !(names(d) %in% drop | startsWith(names(d), ".")), drop = FALSE]
}
