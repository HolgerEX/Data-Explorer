# CANDIDATE (not approved, not sourced by app.R): cache + multi-resolution
# prototype from Sins-boku/Data-Explorer feature/preprocessing @ f3e6e2f.
# Requires R/preprocessing.R. See docu/prototypes/README.md.


source_cache_identity <- function(path, mapping, configuration = list()) {
  info <- file.info(path)
  if (is.na(info$size) || is.na(info$mtime)) {
    preprocessing_abort(paste0("Could not inspect source file: ", path))
  }
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    size = as.numeric(info$size),
    modified = as.numeric(info$mtime),
    version = PREPROCESSING_VERSION,
    mapping = mapping,
    configuration = configuration
  )
}

preprocessing_cache_path <- function(path, mapping, cache_dir,
                                     configuration = list()) {
  identity <- source_cache_identity(path, mapping, configuration)
  payload <- serialize(identity, NULL, version = 2)
  temporary <- tempfile("preprocess-id-")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(payload, temporary)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    preprocessing_abort(paste0("Cannot create cache directory: ", cache_dir))
  }
  key <- unname(tools::md5sum(temporary))
  list(path = file.path(cache_dir, paste0(key, ".rds")), identity = identity)
}

aggregate_summary_level <- function(data, width_by_series, level) {
  dt <- data.table::copy(data.table::as.data.table(data))
  dt[, .base_seconds := unname(width_by_series[.series_id])]
  dt[, .width_seconds := .base_seconds * (2 ^ level)]
  dt[, .bin_start_seconds := floor(as.numeric(datetime) / .width_seconds) *
        .width_seconds]
  dt[, .bin_start := as.POSIXct(
    .bin_start_seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )]
  dt <- dt[is.finite(value) & !is.na(datetime)]
  if (!nrow(dt)) return(data.frame())

  grouped <- dt[, .(
    .count = .N,
    .sum = sum(value),
    .mean = mean(value),
    .min = min(value),
    .min_time = datetime[which.min(value)],
    .max = max(value),
    .max_time = datetime[which.max(value)]
  ), by = c(".series_id", ".gap_id", ".base_seconds", ".width_seconds",
            ".bin_start")]
  grouped[, .bin_end := .bin_start + .width_seconds]
  grouped[, .level := as.integer(level)]
  as.data.frame(grouped)
}

merge_summary_level <- function(previous, level) {
  if (!nrow(previous)) return(data.frame())
  dt <- data.table::copy(data.table::as.data.table(previous))
  dt[, .width_seconds := .base_seconds * (2 ^ level)]
  dt[, .bin_start := as.POSIXct(
    floor(as.numeric(.bin_start) / .width_seconds) * .width_seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )]
  grouped <- dt[, .(
    .count = sum(.count),
    .sum = sum(.sum),
    .mean = sum(.sum) / sum(.count),
    .min = min(.min),
    .min_time = .min_time[which.min(.min)],
    .max = max(.max),
    .max_time = .max_time[which.max(.max)]
  ), by = c(".series_id", ".gap_id", ".base_seconds", ".width_seconds",
            ".bin_start")]
  grouped[, .bin_end := .bin_start + .width_seconds]
  grouped[, .level := as.integer(level)]
  as.data.frame(grouped)
}

build_resolution_summaries <- function(normalized, max_levels = 12L,
                                       observations_per_base_bin = 4,
                                       min_bin_seconds = 1,
                                       gap_factor = 5) {
  if (!all(c(".series_id", ".gap_id", "datetime", "value") %in%
           names(normalized))) {
    preprocessing_abort(
      "Summary generation requires .series_id, .gap_id, datetime, and value."
    )
  }
  if (!is.finite(max_levels) || max_levels < 1 ||
      !is.finite(observations_per_base_bin) || observations_per_base_bin < 1 ||
      !is.finite(min_bin_seconds) || min_bin_seconds <= 0) {
    preprocessing_abort("Invalid resolution-generation configuration.")
  }
  dt <- data.table::as.data.table(normalized)
  cadence <- if ("cadence_seconds" %in% names(dt)) {
    dt[, .(
      .cadence_seconds = if (any(is.finite(cadence_seconds))) {
        stats::median(cadence_seconds[is.finite(cadence_seconds)])
      } else {
        NA_real_
      }
    ), by = .series_id]
  } else {
    dt[, .(.cadence_seconds = NA_real_), by = .series_id]
  }
  cadence[, .base_seconds := pmax(
    min_bin_seconds,
    ifelse(is.finite(.cadence_seconds),
           .cadence_seconds * observations_per_base_bin,
           min_bin_seconds)
  )]
  width_by_series <- stats::setNames(
    cadence$.base_seconds,
    cadence$.series_id
  )
  level_zero <- aggregate_summary_level(dt, width_by_series, 0L)
  if (!nrow(level_zero)) {
    preprocessing_abort("No finite observations are available to summarize.")
  }

  levels <- vector("list", max_levels + 1L)
  levels[[1L]] <- level_zero
  for (level in seq_len(max_levels)) {
    next_level <- merge_summary_level(levels[[level]], level)
    if (!nrow(next_level)) break
    levels[[level + 1L]] <- next_level
    if (nrow(next_level) <= 1L) {
      levels <- levels[seq_len(level + 1L)]
      break
    }
  }
  levels <- Filter(Negate(is.null), levels)
  names(levels) <- as.character(seq_along(levels) - 1L)

  non_series_columns <- c(
    "datetime", "date", "value", "cadence_seconds", ".gap_before",
    ".gap_id", ".delta_seconds", ".time_seconds", ".gap_threshold_seconds"
  )
  series_columns <- setdiff(names(normalized), non_series_columns)
  series_columns <- c(".series_id", setdiff(series_columns, ".series_id"))
  series_map <- unique(normalized[series_columns])
  row.names(series_map) <- NULL
  time_ranges <- data.table::as.data.table(normalized)[, .(
    .start = min(datetime),
    .end = max(datetime)
  ), by = .series_id]

  list(
    version = PREPROCESSING_VERSION,
    generated_at = as.POSIXct(Sys.time(), tz = "UTC"),
    source_identity = NULL,
    configuration = list(
      max_levels = as.integer(max_levels),
      observations_per_base_bin = observations_per_base_bin,
      min_bin_seconds = min_bin_seconds,
      alignment = "UTC Unix epoch",
      gap_factor = gap_factor
    ),
    raw_object_bytes = as.numeric(object.size(normalized)),
    series_map = series_map,
    time_ranges = as.data.frame(time_ranges),
    diagnostics = attr(normalized, "diagnostics"),
    levels = levels
  )
}

save_rds_atomic <- function(object, path) {
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    preprocessing_abort(paste0("Cannot create output directory: ", directory))
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = directory,
    fileext = ".tmp"
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = FALSE)
  if (file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(error) NULL)
    if (!is.null(existing)) {
      unlink(temporary)
      return(invisible(path))
    }
    if (!file.remove(path)) {
      preprocessing_abort(paste0(
        "Cannot replace a corrupt cache file: ", path
      ))
    }
  }
  if (!file.rename(temporary, path)) {
    if (file.exists(path)) {
      winner <- tryCatch(readRDS(path), error = function(error) NULL)
      if (!is.null(winner)) return(invisible(path))
    }
    preprocessing_abort(paste0(
      "Could not atomically write preprocessing cache: ", path
    ))
  }
  invisible(path)
}

preprocessing_memory_limit <- function() {
  value <- getOption("dataExplorer.preprocessing_cache_bytes", 512 * 1024^2)
  if (!is.numeric(value) || length(value) != 1L ||
      !is.finite(value) || value < 0) {
    preprocessing_abort(
      "Option dataExplorer.preprocessing_cache_bytes must be non-negative."
    )
  }
  as.numeric(value)
}

prune_preprocessing_cache <- function(cache_dir, current_path = NULL) {
  limit <- getOption(
    "dataExplorer.preprocessing_disk_cache_bytes",
    5 * 1024^3
  )
  if (!is.numeric(limit) || length(limit) != 1L ||
      !is.finite(limit) || limit < 0) {
    preprocessing_abort(
      "Option dataExplorer.preprocessing_disk_cache_bytes must be non-negative."
    )
  }
  files <- list.files(
    cache_dir,
    pattern = "\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(files)) return(invisible(0L))
  info <- file.info(files)
  total <- sum(info$size, na.rm = TRUE)
  candidates <- order(info$mtime)
  removed <- 0L
  for (index in candidates) {
    if (total <= limit) break
    path <- files[index]
    if (!is.null(current_path) &&
        identical(normalizePath(path, winslash = "/", mustWork = FALSE),
                  normalizePath(current_path, winslash = "/", mustWork = FALSE))) {
      next
    }
    if (file.remove(path)) {
      total <- total - info$size[index]
      removed <- removed + 1L
    } else {
      warning(
        paste0("Could not remove expired summary cache file: ", path),
        call. = FALSE
      )
    }
  }
  invisible(removed)
}

preprocessing_memory_get <- function(key) {
  if (!exists(key, envir = PREPROCESSING_MEMORY_CACHE, inherits = FALSE)) {
    return(NULL)
  }
  PREPROCESSING_MEMORY_CACHE_ORDER <<- c(
    key,
    setdiff(PREPROCESSING_MEMORY_CACHE_ORDER, key)
  )
  get(key, envir = PREPROCESSING_MEMORY_CACHE, inherits = FALSE)
}

preprocessing_memory_set <- function(key, value) {
  limit <- preprocessing_memory_limit()
  value_size <- as.numeric(object.size(value))
  if (value_size > limit) {
    value$raw <- NULL
    value_size <- as.numeric(object.size(value))
    if (value_size > limit) {
      if (exists(key, envir = PREPROCESSING_MEMORY_CACHE, inherits = FALSE)) {
        rm(list = key, envir = PREPROCESSING_MEMORY_CACHE)
        PREPROCESSING_MEMORY_CACHE_ORDER <<- setdiff(
          PREPROCESSING_MEMORY_CACHE_ORDER,
          key
        )
      }
      return(value)
    }
  }
  assign(key, value, envir = PREPROCESSING_MEMORY_CACHE)
  PREPROCESSING_MEMORY_CACHE_ORDER <<- c(
    key,
    setdiff(PREPROCESSING_MEMORY_CACHE_ORDER, key)
  )
  sizes <- vapply(
    as.list(PREPROCESSING_MEMORY_CACHE),
    function(item) as.numeric(object.size(item)),
    numeric(1)
  )
  total <- sum(sizes)
  while (total > preprocessing_memory_limit() &&
         length(PREPROCESSING_MEMORY_CACHE_ORDER) > 1L) {
    oldest <- tail(PREPROCESSING_MEMORY_CACHE_ORDER, 1L)
    if (identical(oldest, key)) break
    rm(list = oldest, envir = PREPROCESSING_MEMORY_CACHE)
    PREPROCESSING_MEMORY_CACHE_ORDER <<- head(
      PREPROCESSING_MEMORY_CACHE_ORDER,
      -1L
    )
    total <- sum(vapply(
      as.list(PREPROCESSING_MEMORY_CACHE),
      function(item) as.numeric(object.size(item)),
      numeric(1)
    ))
  }
  if (total > limit) {
    value$raw <- NULL
    assign(key, value, envir = PREPROCESSING_MEMORY_CACHE)
    return(value)
  }
  value
}

load_preprocessed_timeseries <- function(path, mapping = list(),
                                         cache_dir = file.path(
                                           "data_processed", ".cache"
                                         ),
                                         max_levels = 12L,
                                         gap_factor = 5) {
  cache <- preprocessing_cache_path(
    path,
    mapping,
    cache_dir,
    configuration = list(max_levels = max_levels, gap_factor = gap_factor)
  )
  key <- cache$path
  cached <- preprocessing_memory_get(key)
  if (!is.null(cached)) return(cached)

  summary <- NULL
  if (file.exists(cache$path)) {
    summary <- tryCatch(readRDS(cache$path), error = function(error) {
      warning(
        paste0("Ignoring corrupt preprocessing cache: ", conditionMessage(error)),
        call. = FALSE
      )
      NULL
    })
    if (!is.null(summary) &&
        !identical(summary$source_identity, cache$identity)) {
      summary <- NULL
    }
  }

  normalized <- NULL
  if (is.null(summary)) {
    normalized <- read_timeseries_file(path, mapping, gap_factor = gap_factor)
    source_after_read <- source_cache_identity(
      path,
      mapping,
      configuration = list(max_levels = max_levels, gap_factor = gap_factor)
    )
    if (!identical(cache$identity, source_after_read)) {
      preprocessing_abort(paste0(
        basename(path), " changed while it was being read. Retry after the ",
        "file has finished being written."
      ))
    }
    summary <- build_resolution_summaries(
      normalized$data,
      max_levels = max_levels,
      gap_factor = gap_factor
    )
    summary$source_identity <- cache$identity
    save_rds_atomic(summary, cache$path)
    prune_preprocessing_cache(cache_dir, cache$path)
  }
  value <- list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    mapping = mapping,
    gap_factor = gap_factor,
    cache_path = cache$path,
    summary = summary,
    raw = if (is.null(normalized)) NULL else normalized$data,
    diagnostics = if (is.null(normalized)) summary$diagnostics
    else normalized$diagnostics
  )
  value <- preprocessing_memory_set(key, value)
  preprocessing_memory_get(key) %||% value
}

load_cached_raw_timeseries <- function(preprocessed) {
  if (!is.null(preprocessed$raw)) return(preprocessed$raw)
  normalized <- read_timeseries_file(
    preprocessed$path,
    preprocessed$mapping,
    gap_factor = preprocessed$gap_factor %||% 5
  )
  key <- preprocessed$cache_path
  current <- preprocessing_memory_get(key)
  current$raw <- normalized$data
  preprocessing_memory_set(key, current)
  normalized$data
}

select_plot_resolution <- function(summary, start, end, plot_width_px,
                                   target_per_pixel = 2,
                                   selected_series = NULL,
                                   raw_row_count = NULL) {
  start_seconds <- as.numeric(start)
  end_seconds <- as.numeric(end)
  if (length(start_seconds) != 1L || length(end_seconds) != 1L ||
      !is.finite(start_seconds) || !is.finite(end_seconds) ||
      start_seconds >= end_seconds) {
    preprocessing_abort("Viewport start and end must define a finite time range.")
  }
  if (length(plot_width_px) != 1L || !is.finite(plot_width_px) ||
      plot_width_px <= 0) {
    preprocessing_abort("Plot width must be a positive number of pixels.")
  }
  if (length(target_per_pixel) != 1L || !is.finite(target_per_pixel) ||
      target_per_pixel <= 0) {
    preprocessing_abort("Target representations per pixel must be positive.")
  }
  budget <- max(1L, floor(plot_width_px * target_per_pixel))
  levels <- summary$levels
  if (!length(levels)) {
    return(list(level = "empty", budget = budget, reason = "no summary levels"))
  }
  selected <- selected_series
  candidate_rows <- lapply(levels, function(level_data) {
    keep <- level_data$.bin_end > start_seconds &
      as.numeric(level_data$.bin_start) < end_seconds
    if (!is.null(selected)) keep <- keep & level_data$.series_id %in% selected
    level_data[keep, , drop = FALSE]
  })
  level_zero_count <- if (length(candidate_rows)) {
    sum(candidate_rows[[1L]]$.count)
  } else {
    0
  }
  if (!is.null(raw_row_count)) level_zero_count <- raw_row_count
  raw_available <- summary$raw_object_bytes <= preprocessing_memory_limit()
  if (level_zero_count <= budget && raw_available) {
    return(list(
      level = "raw",
      budget = budget,
      estimated_rows = level_zero_count,
      reason = "raw observations fit the viewport point and memory budgets"
    ))
  }

  representations <- vapply(candidate_rows, nrow, integer(1)) * 3L
  selected_level <- which(representations <= budget)[1L]
  if (is.na(selected_level)) selected_level <- length(levels)
  list(
    level = as.integer(names(levels)[selected_level]),
    budget = budget,
    estimated_rows = representations[selected_level],
    reason = if (representations[selected_level] <= budget) {
      "finest summary level within the point budget"
    } else if (level_zero_count <= budget && !raw_available) {
      "raw subset exceeds the configured process memory budget"
    } else {
      "coarsest available level; series count exceeds the point budget"
    }
  )
}

query_timeseries_viewport <- function(preprocessed, start, end,
                                      plot_width_px, target_per_pixel = 2,
                                      selected_series = NULL) {
  choice <- select_plot_resolution(
    preprocessed$summary,
    start,
    end,
    plot_width_px,
    target_per_pixel,
    selected_series
  )
  if (identical(choice$level, "empty")) {
    return(list(
      raw = data.frame(),
      summary = data.frame(),
      resolution = choice,
      diagnostics = list(returned_rows = 0L, partial_edge_bins = 0L)
    ))
  }
  start_seconds <- as.numeric(start)
  end_seconds <- as.numeric(end)
  if (identical(choice$level, "raw")) {
    raw <- load_cached_raw_timeseries(preprocessed)
    dt <- data.table::as.data.table(raw)
    data.table::setkeyv(dt, c(".series_id", "datetime"))
    ids <- selected_series %||% unique(dt$.series_id)
    ranges <- data.table::data.table(
      .series_id = ids,
      start = as.POSIXct(start_seconds, origin = "1970-01-01", tz = "UTC"),
      end = as.POSIXct(end_seconds, origin = "1970-01-01", tz = "UTC")
    )
    rows <- dt[ranges, on = .(.series_id, datetime >= start, datetime < end),
               nomatch = 0L, allow.cartesian = TRUE]
    return(list(
      raw = as.data.frame(rows),
      summary = data.frame(),
      resolution = choice,
      diagnostics = list(returned_rows = nrow(rows), partial_edge_bins = 0L)
    ))
  }

  level_number <- as.character(choice$level)
  summary_rows <- preprocessed$summary$levels[[level_number]]
  keep <- as.numeric(summary_rows$.bin_start) >= start_seconds &
    summary_rows$.bin_end <= end_seconds
  if (!is.null(selected_series)) {
    keep <- keep & summary_rows$.series_id %in% selected_series
  }
  complete <- summary_rows[keep, , drop = FALSE]
  raw <- data.frame()
  edge_count <- 0L
  if (is.null(preprocessed$raw) &&
      preprocessed$summary$raw_object_bytes > preprocessing_memory_limit()) {
    return(list(
      raw = raw,
      summary = complete,
      resolution = choice,
      diagnostics = list(
        returned_rows = nrow(complete),
        partial_edge_bins = 0L,
        edge_bins_omitted = TRUE
      )
    ))
  }
  if (is.null(preprocessed$raw)) {
    raw <- load_cached_raw_timeseries(preprocessed)
    preprocessed$raw <- raw
  } else {
    raw <- preprocessed$raw
  }
  if (nrow(raw)) {
    base_width <- stats::setNames(
      summary_rows$.base_seconds,
      summary_rows$.series_id
    )
    base_width <- base_width[!duplicated(names(base_width))]
    dt <- data.table::as.data.table(raw)
    data.table::setkeyv(dt, c(".series_id", "datetime"))
    ids <- selected_series %||% unique(dt$.series_id)
    ranges <- data.table::data.table(
      .series_id = ids,
      start = as.POSIXct(start_seconds, origin = "1970-01-01", tz = "UTC"),
      end = as.POSIXct(end_seconds, origin = "1970-01-01", tz = "UTC")
    )
    visible <- dt[ranges, on = .(.series_id, datetime >= start, datetime < end),
                  nomatch = 0L, allow.cartesian = TRUE]
    widths <- unname(base_width[visible$.series_id]) * (2 ^ choice$level)
    bins <- floor(as.numeric(visible$datetime) / widths) * widths
    fully_represented <- bins >= start_seconds & bins + widths <= end_seconds
    edge_rows <- visible[!fully_represented]
    if (nrow(edge_rows)) {
      edge_bins <- bins[!fully_represented]
      edge_rows[, .plot_group := paste(
        .series_id, .gap_id, edge_bins, "edge", sep = "|"
      )]
    }
    raw <- as.data.frame(edge_rows)
    edge_count <- nrow(raw)
  }
  list(
    raw = raw,
    summary = complete,
    resolution = choice,
    diagnostics = list(
      returned_rows = nrow(raw) + nrow(complete),
      partial_edge_bins = edge_count
    )
  )
}
