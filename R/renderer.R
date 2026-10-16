# Adapted from Sins-boku/Env-Plot feature/preprocessing-renderer at dc19987.
ID_CANDIDATES <- c(
  "logger", "position", "source", "trm", "tree", "core_id", "probe_set",
  "depth_bottom", "depth_top", "station", "sensor", "channel"
)

DEPTH_PAL <- c(
  "#000000", "#3D1F00", "#5C3317", "#7B3F00", "#996633",
  "#D2B48C", "#F5DEB3", "#FFFDD0", "#D9D9D9", "#808080"
)
DEPTH_STEP <- 10
TRM_RAMP <- list(D = c("red3", "#FB6A4A"), R = c("royalblue", "#6BAED6"))

`%||%` <- function(value, default) {
  if (is.null(value)) default else value
}

`%|""|%` <- function(value, default) {
  if (is.null(value) || !nzchar(value)) default else value
}

depth_cols <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  i <- pmin(pmax(x / DEPTH_STEP, 1), length(DEPTH_PAL))
  out <- rep(NA_character_, length(x))
  valid <- !is.na(i)
  if (any(valid)) {
    ramp <- grDevices::colorRamp(DEPTH_PAL)(
      (i[valid] - 1) / (length(DEPTH_PAL) - 1)
    )
    out[valid] <- grDevices::rgb(
      ramp[, 1], ramp[, 2], ramp[, 3], maxColorValue = 255
    )
  }
  out
}

make_series <- function(data, id_columns, include_variable = TRUE) {
  if (!length(id_columns)) {
    data$series <- if (include_variable) data$variable else "all"
    return(data)
  }
  data <- tidyr::unite(
    data,
    "id_label",
    dplyr::all_of(id_columns),
    sep = " | ",
    remove = FALSE,
    na.rm = TRUE
  )
  if (!include_variable) {
    data$series <- ifelse(data$id_label == "", "all", data$id_label)
    return(data)
  }
  data$series <- ifelse(
    data$id_label == "",
    data$variable,
    paste0(data$variable, " (", data$id_label, ")")
  )
  data
}

build_colours <- function(data, scheme, manual = NULL) {
  if (!"ckey" %in% names(data)) data$ckey <- data$series
  series <- sort(unique(data$ckey))

  if (scheme == "depth" && "depth_bottom" %in% names(data)) {
    key <- dplyr::distinct(data, ckey, depth_bottom)
    colors <- depth_cols(key$depth_bottom)
    names(colors) <- key$ckey
    if (anyNA(colors)) {
      colors[is.na(colors)] <- scales::hue_pal()(sum(is.na(colors)))
    }
    colors <- colors[series]
    names(colors) <- series
    return(colors)
  }

  if (scheme == "trm" && "trm" %in% names(data)) {
    key <- dplyr::distinct(data, ckey, trm) |>
      dplyr::arrange(ckey)
    colors <- stats::setNames(rep("grey50", nrow(key)), key$ckey)
    for (treatment in names(TRM_RAMP)) {
      index <- which(key$trm == treatment)
      if (length(index)) {
        colors[index] <- grDevices::colorRampPalette(TRM_RAMP[[treatment]])(
          length(index)
        )
      }
    }
    return(colors[series])
  }

  if (scheme == "date") {
    return(stats::setNames(
      grDevices::rainbow(length(series), end = 0.85),
      series
    ))
  }
  if (scheme == "manual" && !is.null(manual)) return(manual[series])
  stats::setNames(scales::hue_pal()(length(series)), series)
}

env_plot_build_plot <- function(data, input, manual_colors = NULL, zoom = NULL) {
  shiny::validate(shiny::need(nrow(data) > 0, "No data for that selection."))
  null_coalesce <- function(value) {
    if (is.null(value) || !is.finite(value)) NA_real_ else as.numeric(value)
  }
  per_column_x <- FALSE
  colors <- build_colours(
    data,
    input$scheme,
    if (identical(input$scheme, "manual")) manual_colors else NULL
  )
  plot_group <- if (".plot_group" %in% names(data)) ".plot_group" else "series"
  add_geoms <- function(plot) {
    plot + switch(
      input$geom,
      line = ggplot2::geom_line(alpha = input$alpha, linewidth = input$lwd),
      point = ggplot2::geom_point(alpha = input$alpha, size = input$psize),
      both = list(
        ggplot2::geom_line(alpha = input$alpha, linewidth = input$lwd),
        ggplot2::geom_point(alpha = input$alpha, size = input$psize)
      ),
      col = ggplot2::geom_col(
        ggplot2::aes(fill = series),
        alpha = input$alpha,
        position = "identity",
        colour = NA
      )
    )
  }

  summary_rows <- NULL
  color_data <- data
  if (identical(input$plotmode, "ts") && ".is_summary" %in% names(data)) {
    summary_rows <- data[data$.is_summary %in% TRUE, , drop = FALSE]
    data <- data[is.na(data$.is_summary) | !data$.is_summary, , drop = FALSE]
  }

  if (identical(input$plotmode, "dual")) {
    shiny::req(input$xvar, input$yvar)
    id_columns <- c(
      "dataset", "series", "ckey", "date",
      intersect("datetime", names(data)),
      intersect(ID_CANDIDATES, names(data))
    )
    wide <- data |>
      dplyr::select(
        dplyr::all_of(id_columns),
        variable,
        value
      ) |>
      dplyr::distinct(
        dplyr::across(dplyr::all_of(c(id_columns, "variable"))),
        .keep_all = TRUE
      ) |>
      tidyr::pivot_wider(names_from = variable, values_from = value)
    shiny::validate(shiny::need(
      all(c(input$xvar, input$yvar) %in% names(wide)) &&
        sum(!is.na(wide[[input$xvar]]) & !is.na(wide[[input$yvar]])) > 0,
      "No samples have both variables. Try adding core_id under 'Split series by'."
    ))

    plot <- ggplot2::ggplot(
      wide,
      ggplot2::aes(.data[[input$xvar]], .data[[input$yvar]], colour = ckey)
    )
    if (isTRUE(input$gmwl)) {
      plot <- plot + ggplot2::geom_abline(
        slope = 8,
        intercept = 10,
        linetype = "dashed",
        colour = "grey40"
      )
    }
    if (isTRUE(input$lmwl) &&
        !is.null(input$lmwl_slope) && !is.null(input$lmwl_int)) {
      plot <- plot + ggplot2::geom_abline(
        slope = input$lmwl_slope,
        intercept = input$lmwl_int,
        linetype = "dotdash",
        colour = "steelblue"
      )
    }
    plot <- plot + ggplot2::geom_point(
      alpha = input$alpha,
      size = input$psize
    )

    fit_text <- NULL
    if (!identical(input$fitline, "none")) {
      formula <- stats::as.formula(paste0(
        "`", input$yvar, "` ~ `", input$xvar, "`"
      ))
      fit_one <- function(frame, label) {
        frame <- frame[
          !is.na(frame[[input$xvar]]) & !is.na(frame[[input$yvar]]),
        ]
        if (nrow(frame) < 3) return(NULL)
        model <- stats::lm(formula, data = frame)
        coefficients <- stats::coef(model)
        tibble::tibble(
          series = label,
          slope = coefficients[2],
          intercept = coefficients[1],
          r2 = summary(model)$r.squared,
          n = nrow(frame)
        )
      }
      fits <- if (identical(input$fitline, "all")) {
        fit_one(wide, "all points")
      } else {
        dplyr::bind_rows(lapply(
          split(wide, wide$series),
          function(frame) fit_one(frame, frame$series[1])
        ))
      }
      if (!is.null(fits) && nrow(fits) > 0) {
        if (identical(input$fitline, "all")) {
          plot <- plot + ggplot2::geom_abline(
            slope = fits$slope[1],
            intercept = fits$intercept[1],
            colour = "black",
            linewidth = 0.6
          )
        } else {
          plot <- plot + ggplot2::geom_smooth(
            method = "lm",
            se = FALSE,
            formula = y ~ x,
            linewidth = 0.6
          )
        }
        if (isTRUE(input$fit_eq)) {
          fit_text <- paste(sprintf(
            "%s: y = %.3fx %+.3f  (R2 = %.3f, n = %d)",
            fits$series, fits$slope, fits$intercept, fits$r2, fits$n
          ), collapse = "\n")
        }
      }
    }
    plot <- plot + ggplot2::labs(
      x = input$xlab %|""|% input$xvar,
      y = input$ylab %|""|% input$yvar,
      subtitle = fit_text
    )
  } else if (identical(input$plotmode, "depth")) {
    shiny::validate(shiny::need(
      "depth_bottom" %in% names(data),
      "The selected datasets have no depth_bottom column."
    ))
    data <- dplyr::filter(data, !is.na(depth_bottom))
    shiny::validate(shiny::need(nrow(data) > 0, "No rows with a depth."))
    group_columns <- intersect(
      c("dataset", "core_id", "tree", "trm", "position", "logger",
        "probe_set", "variable"),
      names(data)
    )
    data$.grp <- do.call(paste, c(
      data[c("series", "date", group_columns)],
      sep = "_"
    ))

    facet_by <- input$facet_by %||% ""
    x_left <- c(null_coalesce(input$xmin), null_coalesce(input$xmax))
    x_right <- c(null_coalesce(input$xmin_r), null_coalesce(input$xmax_r))
    if (nzchar(facet_by) && !all(is.na(x_right))) {
      per_column_x <- TRUE
      facet_levels <- if (is.factor(data[[facet_by]])) {
        levels(droplevels(data[[facet_by]]))
      } else {
        as.character(sort(unique(data[[facet_by]])))
      }
      number_of_columns <- if (identical(facet_by, "tree")) {
        ceiling(length(facet_levels) / 2)
      } else {
        ceiling(sqrt(length(facet_levels)))
      }
      column_of <- ((seq_along(facet_levels) - 1) %% number_of_columns) + 1
      lower <- ifelse(column_of == 1, x_left[1], x_right[1])
      upper <- ifelse(column_of == 1, x_left[2], x_right[2])
      index <- match(as.character(data[[facet_by]]), facet_levels)
      data <- data[
        (is.na(lower[index]) | data$value >= lower[index]) &
          (is.na(upper[index]) | data$value <= upper[index]),
      ]
      shiny::validate(shiny::need(nrow(data) > 0, "No data inside that x range."))
      pin <- tibble::tibble(
        level = rep(facet_levels, 2),
        value = c(lower, upper),
        depth_bottom = data$depth_bottom[1]
      ) |>
        dplyr::filter(!is.na(value))
      pin$level <- if (is.factor(data[[facet_by]])) {
        factor(pin$level, levels = levels(data[[facet_by]]))
      } else {
        methods::as(pin$level, class(data[[facet_by]])[1])
      }
      names(pin)[1] <- facet_by
    }

    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(value, depth_bottom, colour = ckey, group = .grp)
    ) +
      ggplot2::scale_y_reverse(breaks = sort(unique(data$depth_bottom))) +
      ggplot2::labs(
        x = input$xlab %|""|% "value",
        y = input$ylab %|""|% "Depth (cm)"
      )
    plot <- plot + switch(
      input$geom,
      line = ggplot2::geom_path(alpha = input$alpha, linewidth = input$lwd),
      point = ggplot2::geom_point(alpha = input$alpha, size = input$psize),
      both = list(
        ggplot2::geom_path(alpha = input$alpha, linewidth = input$lwd),
        ggplot2::geom_point(alpha = input$alpha, size = input$psize)
      ),
      col = ggplot2::geom_point(alpha = input$alpha, size = input$psize)
    )
    if (per_column_x) {
      plot <- plot + ggplot2::geom_blank(
        data = pin,
        ggplot2::aes(value, depth_bottom),
        inherit.aes = FALSE
      )
    }
    if (nzchar(facet_by)) {
      plot <- plot + ggplot2::facet_wrap(
        stats::as.formula(paste("~", facet_by)),
        scales = "free_x",
        nrow = if (identical(facet_by, "tree")) 2 else NULL
      )
    } else if (dplyr::n_distinct(data$variable) > 1) {
      plot <- plot + ggplot2::facet_wrap(
        ~ variable,
        nrow = 1,
        scales = "free_x"
      )
    }
  } else if (identical(input$mode, "sec") &&
             !is.null(input$sec_var) &&
             input$sec_var %in% data$variable &&
             dplyr::n_distinct(data$variable) > 1) {
    left <- dplyr::filter(data, variable != input$sec_var)
    right <- dplyr::filter(data, variable == input$sec_var)
    shiny::validate(shiny::need(
      nrow(left) > 0 && nrow(right) > 0,
      "Need at least one variable on each axis."
    ))
    left_range <- range(left$value, na.rm = TRUE)
    right_range <- range(right$value, na.rm = TRUE)
    scale_factor <- diff(left_range) / diff(right_range)
    if (!is.finite(scale_factor) || scale_factor == 0) scale_factor <- 1
    shift <- left_range[1] - right_range[1] * scale_factor
    data_scaled <- dplyr::bind_rows(
      left,
      dplyr::mutate(right, value = value * scale_factor + shift)
    )
    plot <- add_geoms(ggplot2::ggplot(
      data_scaled,
      ggplot2::aes(datetime, value, colour = ckey, group = .data[[plot_group]])
    )) +
      ggplot2::scale_y_continuous(
        name = input$ylab %|""|% paste(
          setdiff(input$vars, input$sec_var),
          collapse = ", "
        ),
        sec.axis = ggplot2::sec_axis(
          ~ (. - shift) / scale_factor,
          name = (input$ylab2 %||% "") %|""|% input$sec_var
        )
      ) +
      ggplot2::labs(x = input$xlab %|""|% NULL)
  } else {
    plot <- add_geoms(ggplot2::ggplot(
      data,
      ggplot2::aes(datetime, value, colour = ckey, group = .data[[plot_group]])
    )) +
      ggplot2::labs(
        x = input$xlab %|""|% NULL,
        y = input$ylab %|""|% {
          units <- unique(stats::na.omit(data$unit))
          if (length(units) == 1) units else "value"
        }
      )
    if (identical(input$mode, "facet")) {
      labels <- stats::setNames(
        purrr::map_chr(input$vars, function(variable) {
          value <- input[[paste0("flab_", make.names(variable))]]
          if (is.null(value) || !nzchar(value)) variable else value
        }),
        input$vars
      )
      if (identical(input$strippos, "left")) {
        plot <- plot +
          ggplot2::facet_wrap(
            ~ variable,
            ncol = 1,
            scales = "free_y",
            strip.position = "left",
            labeller = ggplot2::as_labeller(labels)
          ) +
          ggplot2::labs(y = NULL) +
          ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.placement = "outside"
          )
      } else {
        plot <- plot + ggplot2::facet_wrap(
          ~ variable,
          ncol = 1,
          scales = "free_y",
          labeller = ggplot2::as_labeller(labels)
        )
      }
    }
  }

  if (!is.null(summary_rows)) {
    if (nrow(summary_rows)) {
      plot <- plot + ggplot2::geom_rect(
        data = summary_rows,
        ggplot2::aes(
          xmin = .bin_start,
          xmax = .bin_end,
          ymin = .min,
          ymax = .max,
          fill = ckey,
          group = .data[[plot_group]]
        ),
        inherit.aes = FALSE,
        alpha = input$alpha * 0.15,
        colour = NA
      )
    }
  }

  legend_name <- if (identical(input$scheme, "date")) {
    "Sampling date"
  } else if (identical(input$scheme, "trm") &&
             identical(input$plotmode, "depth")) {
    "Treatment | sampling date"
  } else {
    NULL
  }
  plot <- plot + ggplot2::scale_colour_manual(values = colors, name = legend_name)
  if ((identical(input$geom, "col") &&
       identical(input$plotmode, "ts")) ||
      (!is.null(summary_rows) && nrow(summary_rows) > 0)) {
    plot <- plot + ggplot2::scale_fill_manual(
      values = colors,
      name = NULL,
      guide = "none"
    )
  }

  if (!is.null(zoom) && identical(input$plotmode, "ts")) {
    plot <- plot + ggplot2::coord_cartesian(
      xlim = as.POSIXct(
        zoom$x,
        origin = "1970-01-01",
        tz = attr(data$datetime, "tzone") %||% "UTC"
      ),
      ylim = zoom$y,
      expand = FALSE
    )
  } else if (!is.null(zoom)) {
    plot <- plot + ggplot2::coord_cartesian(xlim = zoom$x, ylim = zoom$y)
  }

  x_limits <- if (per_column_x) {
    c(NA_real_, NA_real_)
  } else {
    c(null_coalesce(input$xmin), null_coalesce(input$xmax))
  }
  y_limits <- c(null_coalesce(input$ymin), null_coalesce(input$ymax))
  if (is.null(zoom) && !all(is.na(c(x_limits, y_limits)))) {
    plot <- plot + ggplot2::coord_cartesian(
      xlim = if (all(is.na(x_limits))) NULL else x_limits,
      ylim = if (all(is.na(y_limits))) NULL else y_limits
    )
  }

  base_size <- input$basesize %||% 12
  key_count <- dplyr::n_distinct(color_data$ckey)
  legend_columns <- input$legcols %||% 0
  if (!is.finite(legend_columns) || legend_columns < 1) {
    label_width <- max(nchar(as.character(unique(color_data$ckey)))) *
      0.020 * base_size + 1
    legend_columns <- max(
      1,
      min(key_count, floor((input$w %||% 25) / label_width))
    )
  }
  plot <- plot + ggplot2::guides(
    colour = ggplot2::guide_legend(
      ncol = legend_columns,
      byrow = TRUE,
      title.position = "top"
    ),
    fill = ggplot2::guide_legend(
      ncol = legend_columns,
      byrow = TRUE,
      title.position = "top"
    )
  )
  plot +
    ggplot2::labs(title = if (nzchar(input$title)) input$title else NULL) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(size = base_size),
      strip.text = ggplot2::element_text(size = base_size)
    )
}
