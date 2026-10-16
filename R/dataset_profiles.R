# Saved dataset profiles: column mapping + plot settings, stored as .rds files
# in the local Env-Plot checkout and re-applied to datasets with similar columns.

PROFILE_DIR <- file.path("Env-Plot", "profiles")
PROFILE_MATCH_THRESHOLD <- 0.8
PROFILE_SETTING_IDS <- c(
  "plotmode", "geom", "alpha", "lwd", "psize", "scheme", "interactive",
  "basesize", "title", "xlab", "ylab", "vars", "ids"
)

normalize_column_names <- function(columns) {
  tolower(gsub("[^[:alnum:]]+", "_", columns))
}

read_dataset_profiles <- function(profile_dir = PROFILE_DIR) {
  if (!dir.exists(profile_dir)) return(list())
  paths <- list.files(profile_dir, pattern = "\\.rds$", full.names = TRUE,
                      ignore.case = TRUE)
  profiles <- lapply(paths, function(path) {
    tryCatch(
      readRDS(path),
      error = function(error) stop(
        sprintf("Could not read dataset profile '%s': %s", basename(path),
                conditionMessage(error)),
        call. = FALSE
      )
    )
  })
  names(profiles) <- tools::file_path_sans_ext(basename(paths))
  profiles
}

# Jaccard overlap of normalised column names.
column_overlap <- function(a, b) {
  a <- unique(normalize_column_names(a))
  b <- unique(normalize_column_names(b))
  union_size <- length(union(a, b))
  if (!union_size) return(0)
  length(intersect(a, b)) / union_size
}

# Best profile whose columns overlap the dataset's by at least `threshold`.
match_dataset_profile <- function(columns, profiles,
                                  threshold = PROFILE_MATCH_THRESHOLD) {
  if (!length(profiles)) return(NULL)
  scores <- vapply(profiles, function(profile)
    column_overlap(columns, profile$columns %||% character()), numeric(1))
  best <- which.max(scores)
  if (scores[best] < threshold) return(NULL)
  profile <- profiles[[best]]
  profile$match_score <- scores[best]
  profile
}

guess_dataset_mapping <- function(data) {
  normalized <- normalize_column_names(names(data))
  time_candidates <- c(
    "datetime", "timestamp", "date_time", "datetime_utc", "time", "date", "day"
  )
  time_index <- match(time_candidates, normalized, nomatch = 0L)
  time_index <- time_index[time_index > 0L]
  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
  list(
    time_column = if (length(time_index)) names(data)[time_index[1]] else "",
    value_columns = numeric_columns
  )
}

parse_exploratory_time <- function(x, timezone) {
  if (inherits(x, "POSIXt")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(paste(x, "12:00:00"), tz = timezone))
  as.POSIXct(
    as.character(x), tz = timezone, optional = TRUE,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M",
      "%Y-%m-%d", "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d.%m.%Y"
    )
  )
}

# Turns any table into the long format the renderer expects. Without a usable
# time column the row number is used as x so the data can still be plotted.
normalize_exploratory_dataset <- function(data, path, mapping = NULL,
                                          timezone = "Europe/Vienna") {
  if (!is.data.frame(data) || !nrow(data)) {
    stop(sprintf("'%s' must contain a non-empty table.", basename(path)),
         call. = FALSE)
  }
  mapping <- mapping %||% guess_dataset_mapping(data)
  time_column <- mapping$time_column %||% ""
  if (!time_column %in% names(data)) time_column <- ""
  value_columns <- setdiff(
    intersect(mapping$value_columns %||% character(), names(data)), time_column
  )
  value_columns <- value_columns[vapply(data[value_columns], function(column)
    is.numeric(column) ||
      any(!is.na(suppressWarnings(as.numeric(as.character(column))))),
    logical(1))]
  if (!length(value_columns)) {
    stop(sprintf("'%s' has no numeric columns to plot.", basename(path)),
         call. = FALSE)
  }

  parsed <- if (nzchar(time_column)) {
    suppressWarnings(parse_exploratory_time(data[[time_column]], timezone))
  }
  row_index <- is.null(parsed) || all(is.na(parsed))
  datetime <- if (row_index) {
    as.POSIXct(seq_len(nrow(data)) - 1, origin = "1970-01-01", tz = "UTC")
  } else parsed

  identifiers <- setdiff(names(data), c(time_column, value_columns))
  data$.datetime <- datetime
  long <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(value_columns),
    names_to = "variable",
    values_to = "value",
    values_transform = list(value = function(x)
      suppressWarnings(as.numeric(as.character(x))))
  )
  long$datetime <- long$.datetime
  long$date <- as.Date(long$datetime, tz = timezone)
  long$unit <- NA_character_
  long$dataset <- tools::file_path_sans_ext(basename(path))
  long <- long[!is.na(long$datetime), , drop = FALSE]
  long <- long[, c("dataset", "datetime", "date", "variable", "value", "unit",
                   identifiers), drop = FALSE]
  attr(long, "time_is_index") <- row_index
  attr(long, "mapping") <- list(time_column = time_column,
                                value_columns = value_columns)
  long
}

read_raw_table <- function(path) {
  if (tolower(tools::file_ext(path)) == "rds") {
    readRDS(path)
  } else {
    read_dataset_file(path, basename(path))
  }
}

# Exploratory loader: uses the mapping of a matching saved profile, otherwise
# guesses one, so any table can be plotted in a basic way.
load_exploratory_dataset <- function(path, profile_dir = PROFILE_DIR,
                                     mapping = NULL) {
  raw <- read_raw_table(path)
  if (!is.data.frame(raw)) return(NULL)
  profile <- tryCatch(
    match_dataset_profile(names(raw), read_dataset_profiles(profile_dir)),
    error = function(e) NULL
  )
  long <- normalize_exploratory_dataset(raw, path, mapping %||% profile$mapping)
  attr(long, "source_columns") <- names(raw)
  attr(long, "profile") <- profile
  long
}

build_dataset_profile <- function(name, columns, mapping, settings) {
  list(
    name = name,
    columns = columns,
    mapping = mapping,
    settings = settings,
    saved_at = Sys.time()
  )
}

# Writes the profile into the Env-Plot checkout; committing is left to the user.
save_dataset_profile <- function(profile, profile_dir = PROFILE_DIR) {
  name <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9_-]+", "_", profile$name))
  if (!nzchar(name)) {
    stop("Enter a profile name using letters or numbers.", call. = FALSE)
  }
  if (!dir.exists(profile_dir) &&
      !dir.create(profile_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("Could not create profile directory '%s'.", profile_dir),
         call. = FALSE)
  }
  path <- file.path(profile_dir, paste0(name, ".rds"))
  saveRDS(profile, path)
  path
}
