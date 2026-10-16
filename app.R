# =============================================================================
# app.R  -  Rosalia plotting app
#
# Reads the tidy .rds files in data_processed/ (and data/); data can be added
# in the browser (CSV or RDS upload, see R/data_input.R) and builds every dropdown from
# the data itself, so a new dataset saved there appears without editing this
# file. A dataset only needs: a `variable` column, a `value` column, and
# either `datetime` or `date`.
#
# RUN:  open app.R, click "Run App"   (or  shiny::runApp("app.R"))
#
# SAVE YOUR DATA FIRST (re-run whenever it changes):
#   saveRDS(raw_soilcore, "data_processed/soilcore.rds")
#   saveRDS(met_d,        "data_processed/met_d.rds")
#   saveRDS(sm,           "data_processed/soil_moisture.rds")
#  a test comment
# =============================================================================

#library(shiny)
#library(tidyverse)
#library(plotly)

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(scales)
library(plotly)

# Uploads are stored in DATA_DIR (created on demand); .csv/.rds files placed in
# data/ and data_input/ are listed as well.
DATA_DIR  <- "data_processed"
DATA_DIRS <- c(DATA_DIR, "data", "data_input")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

# Shiny auto-loads R/*.R; source explicitly only when that did not happen.
if (!exists("env_plot_build_plot", mode = "function")) source("R/renderer.R", local = TRUE)
if (!exists("load_exploratory_dataset", mode = "function")) source("R/dataset_profiles.R", local = TRUE)
if (!exists("ingest_upload", mode = "function")) source("R/data_input.R", local = TRUE)
if (!exists("load_timeseries_for_explorer", mode = "function")) source("R/preprocessing.R", local = TRUE)
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

load_dataset <- function(path, mapping = NULL) {
  d <- if (tolower(tools::file_ext(path)) == "rds") {
    tryCatch(readRDS(path), error = function(e) NULL)
  }
  explorer_ready <- is.data.frame(d) &&
    all(c("variable", "value") %in% names(d)) &&
    any(c("datetime", "date") %in% names(d))
  if (!explorer_ready) {
    d <- tryCatch(load_timeseries_for_explorer(path), error = function(e) NULL)
  }
  source_columns <- if (is.data.frame(d)) names(d)
  if (is.null(d) || !all(c("variable", "value") %in% names(d))) {
    d <- tryCatch(load_exploratory_dataset(path, mapping = mapping), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    source_columns <- attr(d, "source_columns")
  }
  mapping_used <- attr(d, "mapping")
  exploratory_note <- attr(d, "time_is_index")
  if (!"datetime" %in% names(d)) {
    if (!"date" %in% names(d)) return(NULL)
    d$datetime <- as.POSIXct(paste(d$date, "12:00:00"), tz = "Europe/Vienna")
  }
  profile_columns <- source_columns
  if (!"date" %in% names(d)) d$date <- as.Date(d$datetime)
  if (!"unit" %in% names(d)) d$unit <- NA_character_
  drop <- c("dataset", "datetime", "date", "variable", "value", "unit",
            "source_file", "timestep_min")
  extra <- setdiff(names(d), c(ID_CANDIDATES, drop))
  extra <- extra[!stringr::str_starts(extra, "flag_")]
  extra <- extra[purrr::map_lgl(extra, function(v) {
    x <- d[[v]]
    n <- dplyr::n_distinct(x, na.rm = TRUE)
    if (is.character(x) || is.factor(x) || is.logical(x)) return(n > 0 && n <= 100)
    is.numeric(x) && n > 0 && n <= 50 && all(x[!is.na(x)] %% 1 == 0)
  })]
  ids <- c(intersect(ID_CANDIDATES, names(d)), extra)
  out <- d |>
    dplyr::mutate(dataset = tools::file_path_sans_ext(basename(path))) |>
    dplyr::select(dataset, datetime, date, variable, value, unit, dplyr::all_of(ids)) |>
    dplyr::filter(!is.na(datetime))
  attr(out, "source_columns") <- profile_columns
  attr(out, "time_is_index") <- isTRUE(exploratory_note)
  attr(out, "mapping") <- mapping_used
  out
}

# -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Rosalia data explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # options scroll on their own, the plot stays put
      style = "height: calc(100vh - 90px); overflow-y: auto; position: sticky; top: 10px;",
      fileInput("upload", "Add data (.csv or .rds; any table with numeric columns)",
                accept = c(".csv", ".rds"), multiple = TRUE),
      uiOutput("upload_msg"),
      selectInput("datasets", "Datasets", choices = NULL, multiple = TRUE),
      uiOutput("profile_ui"),
      
      radioButtons("plotmode", "Plot type",
                   c("Time series"   = "ts",
                     "Depth profile" = "depth",
                     "Dual isotope"  = "dual"), selected = "ts"),
      
      uiOutput("var_ui"),
      uiOutput("dual_ui"),
      uiOutput("id_ui"),
      uiOutput("id_keep_ui"),
      uiOutput("facet_by_ui"),
      uiOutput("depth_ui"),
      uiOutput("date_ui"),
      uiOutput("dates_pick_ui"),
      
      hr(),
      uiOutput("axis_mode_ui"),
      uiOutput("sec_ui"),
      
      hr(),
      checkboxInput("interactive", "Interactive time series (hover for values)", FALSE),
      radioButtons("geom", "Draw as",
                   c("Line" = "line", "Points" = "point",
                     "Line + points" = "both", "Bars" = "col"),
                   selected = "line"),
      sliderInput("alpha", "Opacity", 0.1, 1, 0.9, step = 0.05),
      sliderInput("lwd", "Line width", 0.1, 2, 0.5, step = 0.1),
      sliderInput("psize", "Point size", 0.5, 6, 2, step = 0.5),
      
      hr(),
      selectInput("scheme", "Colour by",
                  c("Automatic" = "auto", "Treatment (D red / R blue)" = "trm",
                    "Depth (soil ramp)" = "depth", "Date (rainbow)" = "date",
                    "Pick by hand" = "manual"), selected = "auto"),
      uiOutput("colour_ui"),
      
      hr(),
      textInput("title", "Title", ""),
      textInput("xlab", "X label (blank = auto)", ""),
      textInput("ylab", "Y label / left axis", ""),
      uiOutput("ylab2_ui"),
      uiOutput("facet_lab_ui"),
      
      hr(),
      uiOutput("xlim_ui"),
      fluidRow(
        column(6, numericInput("ymin", "Y min", NA)),
        column(6, numericInput("ymax", "Y max", NA))
      ),
      helpText("Axis range: blank = automatic. Applies to every panel when facetted."),
      actionLink("lim_reset", "reset axis range"),
      
      hr(),
      numericInput("w", "Width (cm)", 25, 4, 60, 0.5),
      numericInput("h", "Height (cm)", 15, 4, 60, 0.5),
      numericInput("dpi", "DPI", 300, 72, 900, 10),
      numericInput("basesize", "Base font size (pt)", 12, 6, 24, 1),
      numericInput("legcols", "Legend columns (0 = auto)", 0, 0, 12, 1),
      downloadButton("dl_png", "PNG"),
      downloadButton("dl_pdf", "PDF")
    ),
    
    mainPanel(
      width = 9,
      style = "position: sticky; top: 10px;",
      uiOutput("plot_area"),
      helpText("Drag a box then double-click to zoom. Double-click on empty space to reset."),
      hr(),
      verbatimTextOutput("info")
    )
  )
)

# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  uploaded <- reactiveVal(0)
  upload_results <- reactiveVal(list())

  files <- reactive({
    invalidateLater(30000, session)
    uploaded()
    list.files(DATA_DIRS[dir.exists(DATA_DIRS)], pattern = "\\.(rds|csv)$",
               full.names = TRUE, ignore.case = TRUE)
  })

  # Accepted uploads are written to DATA_DIR, so they enter the same
  # files() -> input$datasets -> dat() path as any other dataset.
  observeEvent(input$upload, {
    up <- input$upload
    res <- lapply(seq_len(nrow(up)), function(i)
      c(list(name = up$name[i]), ingest_upload(up$datapath[i], up$name[i], DATA_DIR)))
    upload_results(res)
    uploaded(uploaded() + 1)
    ok <- Filter(function(r) r$ok, res)
    if (length(ok)) {
      new <- vapply(ok, function(r) r$path, character(1))
      ch <- setNames(files(), tools::file_path_sans_ext(basename(files())))
      updateSelectInput(session, "datasets", choices = ch,
                        selected = union(isolate(input$datasets), new))
    }
  })

  output$upload_msg <- renderUI({
    lapply(upload_results(), function(r) {
      tagList(
        div(style = paste0("color:", if (r$ok) "#2e7d32" else "#c62828", ";"),
            if (r$ok) r$message else paste0(r$name, ": ", r$message)),
        lapply(r$warnings, function(w) div(style = "color:#ef6c00;", w)))
    })
  })
  
  observe({
    ch <- setNames(files(), tools::file_path_sans_ext(basename(files())))
    updateSelectInput(session, "datasets", choices = ch,
                      selected = isolate(input$datasets))
  })
  
  # A depth profile is almost always read as "how did the profile change
  # between campaigns", so colouring by sampling date is the sensible default
  # there. Switching plot type flips it; the user can still override.
  observeEvent(input$plotmode, {
    saved <- isolate(active_profile()$settings$scheme)
    updateSelectInput(session, "scheme",
                      selected = saved %||%
                        if (identical(input$plotmode, "depth")) "date" else "auto")
  }, ignoreInit = TRUE)
  
  observeEvent(input$facet_by, {                                 
    if (isTRUE(input$facet_by %in% c("trm", "tree")))
      updateSelectInput(session, "scheme", selected = "trm")
  }, ignoreInit = TRUE)
  # Adding a dataset from another period used to leave the old, narrower date
  # range in place; the new variable then had zero rows and facet_wrap dropped
  # its panel without a word - "the second figure never shows up".
  observeEvent(input$datasets, {
    r <- range(dat()$date, na.rm = TRUE)
    if (all(is.finite(r)))
      updateDateRangeInput(session, "dates", start = r[1], end = r[2],
                           min = r[1], max = r[2])
  })
  
  # The widget can be momentarily absent (it is rendered by renderUI), so the
  # date window falls back to everything that is loaded.
  date_window <- reactive({
    rng <- suppressWarnings(range(dat()$date, na.rm = TRUE))
    if (is.null(input$dates) || !all(is.finite(rng))) rng else as.Date(input$dates)
  })
  
  dat <- reactive({
    req(input$datasets)
    override <- mapping_override()
    out <- compact(map(input$datasets, function(path) {
      mapping <- if (identical(override$path, path)) override$mapping
      possibly(load_dataset, NULL)(path, mapping)
    }))
    validate(need(length(out) > 0,
                  "None of those files could be read as a table with numeric columns."))
    res <- bind_rows(out)
    if (length(out) == 1L) {
      attr(res, "source_columns") <- attr(out[[1]], "source_columns")
      attr(res, "mapping") <- attr(out[[1]], "mapping")
      attr(res, "time_is_index") <- attr(out[[1]], "time_is_index")
    }
    res
  })

  # --- saved profiles (mapping + plot settings, stored in Env-Plot) ---------
  mapping_override <- reactiveVal(NULL)
  profile_version <- reactiveVal(0)
  active_profile <- reactiveVal(NULL)

  observeEvent(list(dat(), profile_version()), {
    cols <- attr(dat(), "source_columns")
    profile <- if (length(cols)) {
      tryCatch(match_dataset_profile(cols, read_dataset_profiles()),
               error = function(e) NULL)
    }
    active_profile(profile)
    s <- profile$settings
    if (is.null(s)) return()
    if (!is.null(s$scheme)) updateSelectInput(session, "scheme", selected = s$scheme)
    if (!is.null(s$plotmode)) updateRadioButtons(session, "plotmode", selected = s$plotmode)
    if (!is.null(s$geom)) updateRadioButtons(session, "geom", selected = s$geom)
    for (id in c("alpha", "lwd", "psize"))
      if (!is.null(s[[id]])) updateSliderInput(session, id, value = s[[id]])
    if (!is.null(s$basesize)) updateNumericInput(session, "basesize", value = s$basesize)
    if (!is.null(s$interactive)) updateCheckboxInput(session, "interactive", value = s$interactive)
    for (id in c("title", "xlab", "ylab"))
      if (!is.null(s[[id]])) updateTextInput(session, id, value = s[[id]])
  })

  output$profile_ui <- renderUI({
    req(input$datasets)
    d <- dat()
    profile <- active_profile()
    exploratory <- !is.null(attr(d, "mapping"))
    mapping_ui <- NULL
    if (exploratory && length(input$datasets) == 1L) {
      raw <- tryCatch(read_raw_table(input$datasets), error = function(e) NULL)
      if (is.data.frame(raw)) {
        mapping <- attr(d, "mapping")
        numeric_cols <- names(raw)[vapply(raw, is.numeric, logical(1))]
        mapping_ui <- tagList(
          helpText("Basic plot of a table outside the documented format. Adjust the columns if needed:"),
          selectInput("map_time", "Time column",
                      c("(row number)" = "", names(raw)),
                      selected = mapping$time_column),
          selectInput("map_values", "Value columns", names(raw), multiple = TRUE,
                      selected = mapping$value_columns),
          actionButton("map_apply", "Apply columns")
        )
      }
    }
    save_ui <- if (length(input$datasets) == 1L && length(attr(d, "source_columns"))) {
      tagList(
        textInput("profile_name", "Profile name",
                  value = profile$name %||% tools::file_path_sans_ext(basename(input$datasets))),
        actionButton("profile_save", "Save plot config to Env-Plot")
      )
    }
    tagList(
      if (!is.null(profile))
        div(style = "color:#2e7d32;",
            sprintf("Saved profile '%s' applied (%.0f%% column match).",
                    profile$name, 100 * profile$match_score)),
      mapping_ui,
      save_ui,
      hr()
    )
  })

  observeEvent(input$map_apply, {
    req(length(input$datasets) == 1L)
    mapping_override(list(
      path = input$datasets,
      mapping = list(time_column = input$map_time %||% "",
                     value_columns = input$map_values %||% character())
    ))
  })

  observeEvent(input$profile_save, {
    d <- dat()
    settings <- setNames(lapply(PROFILE_SETTING_IDS, function(id) input[[id]]),
                         PROFILE_SETTING_IDS)
    profile <- build_dataset_profile(
      name = input$profile_name,
      columns = attr(d, "source_columns"),
      mapping = attr(d, "mapping"),
      settings = settings
    )
    path <- tryCatch(save_dataset_profile(profile),
                     error = function(e) {
                       showNotification(conditionMessage(e), type = "error")
                       NULL
                     })
    if (!is.null(path)) {
      showNotification(
        sprintf("Saved to %s. Commit and push the Env-Plot checkout to share it.", path),
        type = "message", duration = 10)
      profile_version(profile_version() + 1)
    }
  })
  
  # --- variable pickers ------------------------------------------------------
  output$var_ui <- renderUI({
    if (identical(input$plotmode, "dual")) return(NULL)
    v <- sort(unique(dat()$variable))
    sel <- intersect(active_profile()$settings$vars, v)
    if (!length(sel)) sel <- head(v, 1)
    selectInput("vars", "Variables", choices = v, multiple = TRUE,
                selected = sel)
  })
  
  output$dual_ui <- renderUI({
    if (!identical(input$plotmode, "dual")) return(NULL)
    v <- sort(unique(dat()$variable))
    tagList(
      selectInput("xvar", "X variable", choices = v,
                  selected = if ("d18O" %in% v) "d18O" else v[1]),
      selectInput("yvar", "Y variable", choices = v,
                  selected = if ("d2H" %in% v) "d2H" else tail(v, 1)),
      checkboxInput("gmwl", "Show GMWL (d2H = 8*d18O + 10)", TRUE),
      checkboxInput("lmwl", "Show LMWL", TRUE),
      fluidRow(
        column(6, numericInput("lmwl_slope", "LMWL slope", 7.6603, step = 0.001)),
        column(6, numericInput("lmwl_int", "LMWL intercept", 4.4649, step = 0.1))
      ),
      hr(),
      radioButtons("fitline", "Regression line",
                   c("None" = "none",
                     "One line for all points" = "all",
                     "One line per series" = "series"),
                   selected = "none"),
      checkboxInput("fit_eq", "Show slope / intercept / R2", TRUE)
    )
  })
  
  output$id_ui <- renderUI({
    ids <- intersect(ID_CANDIDATES, names(dat()))
    ids <- ids[map_lgl(ids, ~ n_distinct(dat()[[.x]], na.rm = TRUE) > 1)]
    selectInput("ids", "Split series by", choices = ids, multiple = TRUE,
                selected = if (length(saved <- intersect(active_profile()$settings$ids, ids)))
                  saved else intersect(c("logger", "trm"), ids))
  })
  
  # one tick box per level of every "Split series by" column, so single trees,
  # treatments or loggers can be dropped from the plot without re-filtering
  output$id_keep_ui <- renderUI({
    d <- dat(); ids <- intersect(input$ids %||% character(), names(d))
    if (!length(ids)) return(NULL)
    tagList(lapply(ids, function(v) {
      lv <- as.character(sort(unique(d[[v]])))
      if (identical(v, "tree")) lv <- c(intersect(c("6", "4", "7", "10"), lv),
                                        setdiff(lv, c("6", "4", "7", "10")))
      keep <- isolate(input[[paste0("keep_", v)]])
      checkboxGroupInput(paste0("keep_", v), paste("Show", v), inline = TRUE,
                         choices = lv,
                         selected = if (is.null(keep)) lv else intersect(lv, keep))
    }))
  })
  
  output$facet_by_ui <- renderUI({
    if (identical(input$plotmode, "ts")) return(NULL)
    ids <- intersect(ID_CANDIDATES, names(dat()))
    ids <- ids[map_lgl(ids, ~ n_distinct(dat()[[.x]], na.rm = TRUE) > 1)]
    selectInput("facet_by", "Facet panels by",
                choices = c("None" = "", "variable", ids), selected = "")
  })
  
  # --- show / hide individual depths ----------------------------------------
  output$depth_ui <- renderUI({
    d <- dat()
    if (!"depth_bottom" %in% names(d)) return(NULL)
    lev <- sort(unique(d$depth_bottom[!is.na(d$depth_bottom)]))
    if (!length(lev)) return(NULL)
    checkboxGroupInput("depths", "Depths to show (depth_bottom)",
                       choices = lev, selected = lev, inline = TRUE)
  })
  
  output$date_ui <- renderUI({
    r <- range(dat()$date, na.rm = TRUE)
    dateRangeInput("dates", "Date range", start = r[1], end = r[2],
                   min = r[1], max = r[2])
  })
  
  # Tick individual sampling dates on and off, the same way depths work.
  # Only offered when there is a manageable number of distinct dates, so it
  # does not try to list every timestamp of a 10-minute logger series.
  output$dates_pick_ui <- renderUI({
    req(input$dates)
    dd <- dat() %>%
      filter(date >= date_window()[1], date <= date_window()[2])
    lev <- sort(unique(dd$date))
    if (!length(lev) || length(lev) > 60) return(NULL)
    lev_chr <- format(lev, "%Y-%m-%d")
    tagList(
      checkboxGroupInput("pick_dates", "Sampling dates to show",
                         choices = lev_chr, selected = lev_chr, inline = TRUE),
      actionLink("dates_all", "all"), " / ", actionLink("dates_none", "none")
    )
  })
  
  observeEvent(input$dates_all, {
    dd <- dat() %>% filter(date >= date_window()[1], date <= date_window()[2])
    lev <- format(sort(unique(dd$date)), "%Y-%m-%d")
    updateCheckboxGroupInput(session, "pick_dates", selected = lev)
  })
  observeEvent(input$dates_none, {
    updateCheckboxGroupInput(session, "pick_dates", selected = character(0))
  })
  
  # x is a datetime in a time series, where the existing "Date range" already
  # sets the span, so numeric x limits are only offered for the other modes
  output$xlim_ui <- renderUI({
    if (identical(input$plotmode, "ts")) return(NULL)
    tagList(
      fluidRow(
        column(6, numericInput("xmin", "X min", NA)),
        column(6, numericInput("xmax", "X max", NA))
      ),
      # A facetted depth profile can need a different x range on each side of
      # the grid (e.g. the two reference trees vs the two drought trees), so
      # the right-hand panel column gets its own pair. Blank = same as left.
      if (identical(input$plotmode, "depth"))
        fluidRow(
          column(6, numericInput("xmin_r", "X min (right col.)", NA)),
          column(6, numericInput("xmax_r", "X max (right col.)", NA))
        )
    )
  })
  
  observeEvent(input$lim_reset, {
    for (i in c("xmin", "xmax", "xmin_r", "xmax_r", "ymin", "ymax"))
      updateNumericInput(session, i, value = NA)
  })
  
  output$axis_mode_ui <- renderUI({
    if (!identical(input$plotmode, "ts")) return(NULL)
    radioButtons("mode", "Y axes",
                 c("One axis" = "single", "Facet per variable" = "facet",
                   "Secondary axis" = "sec"), selected = "facet")
  })
  
  output$sec_ui <- renderUI({
    if (!identical(input$plotmode, "ts") || !identical(input$mode, "sec")) return(NULL)
    req(input$vars)
    selectInput("sec_var", "Variable on the RIGHT axis",
                choices = input$vars, selected = tail(input$vars, 1))
  })
  
  output$ylab2_ui <- renderUI({
    if (!identical(input$plotmode, "ts") || !identical(input$mode, "sec")) return(NULL)
    textInput("ylab2", "Right axis label", "")
  })
  
  # per-variable labels shown as facet strip text
  output$facet_lab_ui <- renderUI({
    if (!identical(input$plotmode, "ts") || !identical(input$mode, "facet")) return(NULL)
    req(input$vars)
    tagList(
      radioButtons("strippos", "Facet label position",
                   c("Above each panel" = "top",
                     "Left, as per-panel y-axis label" = "left"),
                   selected = "left"),
      helpText("Choose 'Left' to give each variable its own y-axis label (e.g. permille on top, % below):"),
      lapply(input$vars, function(v)
        textInput(paste0("flab_", make.names(v)), v, value = v))
    )
  })
  
  # --- filtered data ---------------------------------------------------------
  plot_dat <- reactive({
    d <- dat() %>%
      filter(date >= date_window()[1], date <= date_window()[2], !is.na(value))
    
    # panel order: 6 | 4 top, 7 | 10 bottom
    if ("tree" %in% names(d)) {
      lv <- c(6, 4, 7, 10)
      d <- d %>% mutate(tree = factor(tree, levels = c(
        lv, setdiff(sort(unique(tree)), lv))))
    }
    
    # a NULL box means the UI has not been drawn yet -> keep everything;
    # an empty box means the user unticked every level
    for (v in intersect(input$ids %||% character(), names(d))) {
      keep <- input[[paste0("keep_", v)]]
      if (!is.null(keep)) d <- d[as.character(d[[v]]) %in% keep, ]
    }
    d <- droplevels(d)
    
    if (!is.null(input$pick_dates)) {
      d <- d %>% filter(format(date, "%Y-%m-%d") %in% input$pick_dates)
    }
    
    if ("depth_bottom" %in% names(d) && !is.null(input$depths)) {
      d <- d %>% filter(is.na(depth_bottom) |
                          as.character(depth_bottom) %in% as.character(input$depths))
    }
    
    if (identical(input$plotmode, "dual")) {
      req(input$xvar, input$yvar)
      d <- d %>% filter(variable %in% c(input$xvar, input$yvar))
    } else {
      req(input$vars)
      d <- d %>% filter(variable %in% input$vars)
    }
    d <- make_series(d, input$ids %||% character(0),
                     include_variable = !identical(input$plotmode, "dual"))
    # `series` groups the lines; `ckey` drives the colour. Keeping them apart
    # is what lets one line per core be coloured by sampling date.
    if (identical(input$scheme, "date")) {
      d <- d %>% mutate(ckey = format(date, "%Y-%m-%d"))
    } else if (identical(input$scheme, "trm") &&
               identical(input$plotmode, "depth")) {
      # one legend entry per treatment AND sampling date, so each campaign
      # gets its own step of the D-red / R-blue ramp and is named in the key
      d <- d %>% mutate(ckey = paste0(series, " | ", format(date, "%Y-%m-%d")))
    } else {
      d <- d %>% mutate(ckey = series)
    }
    d
  })
  
  output$colour_ui <- renderUI({
    if (!identical(input$scheme, "manual")) return(NULL)
    s <- sort(unique(plot_dat()$series))
    validate(need(length(s) > 0, "No data for that selection."))
    if (length(s) > 15)
      return(helpText(paste(length(s), "series - too many to pick by hand.")))
    pal <- scales::hue_pal()(length(s))
    tagList(lapply(seq_along(s), function(i)
      textInput(paste0("col_", i), s[i], value = pal[i])))
  })
  
  manual_cols <- reactive({
    s <- sort(unique(plot_dat()$series))
    pal <- scales::hue_pal()(length(s))
    out <- map_chr(seq_along(s), function(i) {
      v <- input[[paste0("col_", i)]]
      if (is.null(v) || !nzchar(v)) pal[i] else v
    })
    ok <- map_lgl(out, ~ tryCatch({ grDevices::col2rgb(.x); TRUE },
                                  error = function(e) FALSE))
    out[!ok] <- pal[!ok]
    setNames(out, s)
  })
  
  zoom <- reactiveVal(NULL)
  observeEvent(input$dbl, {
    b <- input$brush
    zoom(if (!is.null(b)) list(x = c(b$xmin, b$xmax), y = c(b$ymin, b$ymax)) else NULL)
  })
  
  # --- the plot --------------------------------------------------------------
  build_plot <- reactive({
    d <- plot_dat()
    validate(need(nrow(d) > 0, "No data for that selection."))

    miss <- setdiff(input$vars %||% character(), unique(d$variable))
    if (length(miss) && !identical(input$plotmode, "dual"))
      showNotification(
        paste0("No data for ", paste(miss, collapse = ", "),
               " with the current filters (date range, depths, dates ticked)",
               " - that panel is missing."),
        type = "warning", duration = 10)

    env_plot_build_plot(
      d,
      input,
      manual_colors = if (identical(input$scheme, "manual")) manual_cols() else NULL,
      zoom = zoom()
    )
  })
  # Static ggplot by default (brush zoom + exact export). With the box ticked a
  # time series goes to plotly instead: hovermode "closest" reports the nearest
  # POINT only, rather than a running readout like dygraphs.
  output$plot_area <- renderUI({
    if (isTRUE(input$interactive) && identical(input$plotmode, "ts")) {
      if (!requireNamespace("plotly", quietly = TRUE))
        return(helpText("Interactive mode needs plotly: install.packages('plotly')"))
      plotly::plotlyOutput("iplot", height = "640px")
    } else {
      plotOutput("plot", height = "640px", dblclick = "dbl",
                 brush = brushOpts("brush", resetOnNew = TRUE))
    }
  })
  
  output$plot <- renderPlot(build_plot())
  
  output$iplot <- plotly::renderPlotly({
    p <- plotly::ggplotly(build_plot(), tooltip = c("x", "y", "colour"))
    p <- plotly::plotly_build(p)
    # the colour aesthetic is called ckey internally; the hover box should not
    # say so
    p$x$data <- lapply(p$x$data, function(tr) {
      if (!is.null(tr$text))
        tr$text <- gsub("ckey:", "series:", tr$text, fixed = TRUE)
      tr
    })
    plotly::layout(p, hovermode = "closest",
                   legend = list(orientation = "h", y = -0.15))
  })
  
  output$info <- renderText({
    d <- plot_dat()
    paste0(nrow(d), " points | ", n_distinct(d$series), " series | ",
           paste(sort(unique(d$dataset)), collapse = ", "))
  })
  
  output$dl_png <- downloadHandler(
    filename = function() paste0("rosalia_", Sys.Date(), ".png"),
    content  = function(file) ggsave(file, build_plot(), width = input$w,
                                     height = input$h, units = "cm",
                                     dpi = input$dpi)
  )
  output$dl_pdf <- downloadHandler(
    filename = function() paste0("rosalia_", Sys.Date(), ".pdf"),
    content  = function(file) ggsave(file, build_plot(), width = input$w,
                                     height = input$h, units = "cm",
                                     device = cairo_pdf)
  )
}

shinyApp(ui, server)
