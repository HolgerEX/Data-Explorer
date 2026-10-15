Below is a modular Shiny app that currently implements only CSV dataset upload and server-side storage.

It provides:

- CSV upload through the browser
- Server-side persistence in a `data/` directory
- Upload validation
- Safe filenames
- Duplicate filename handling
- Dataset listing with file size and upload time
- Download of all stored datasets as a ZIP archive
- Modularized UI and server logic
- Minimal `app.R` entry point for the person extending the explorer

## Project structure

```text
environmental-data-explorer/
├── app.R
├── DESCRIPTION
├── README.md
├── R/
│   ├── mod_data_input.R
│   └── utils.R
└── data/
    └── .gitkeep
```

---

## `app.R`

```r
# app.R
#
# Main application entry point.
# Explorer capabilities can be added as additional modules under R/.

library(shiny)

source("R/utils.R")
source("R/mod_data_input.R")

ui <- fluidPage(
  titlePanel("Environmental Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      mod_data_input_ui("data_input")
    ),

    mainPanel(
      h3("Stored datasets"),
      mod_data_input_table_ui("data_input")
    )
  )
)

server <- function(input, output, session) {
  mod_data_input_server(
    id = "data_input",
    storage_dir = "data"
  )
}

shinyApp(ui, server)
```

---

## `R/utils.R`

```r
# R/utils.R

# Create a directory if it does not exist.
ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(path)) {
    stop("Could not create directory: ", path)
  }

  invisible(path)
}


# Return a safe filename based on a user-provided filename.
#
# This prevents path traversal and removes characters that are problematic
# on common operating systems.
sanitize_filename <- function(filename) {
  filename <- basename(filename)

  filename <- iconv(
    filename,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = "_"
  )

  filename <- gsub("[^A-Za-z0-9._-]", "_", filename)
  filename <- gsub("_+", "_", filename)

  if (!nzchar(filename) || filename %in% c(".", "..")) {
    filename <- "dataset.csv"
  }

  filename
}


# Ensure that a filename ends in .csv.
ensure_csv_extension <- function(filename) {
  if (!grepl("\\.csv$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".csv")
  }

  filename
}


# Create a unique destination path for an uploaded file.
unique_file_path <- function(directory, filename) {
  filename <- sanitize_filename(filename)
  filename <- ensure_csv_extension(filename)

  candidate <- file.path(directory, filename)

  if (!file.exists(candidate)) {
    return(candidate)
  }

  stem <- tools::file_path_sans_ext(filename)
  extension <- tools::file_ext(filename)

  counter <- 1L

  repeat {
    candidate_name <- paste0(stem, "_", counter, ".", extension)
    candidate <- file.path(directory, candidate_name)

    if (!file.exists(candidate)) {
      return(candidate)
    }

    counter <- counter + 1L
  }
}


# Return information about all CSV files in a storage directory.
list_csv_datasets <- function(directory) {
  ensure_directory(directory)

  files <- list.files(
    directory,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0L) {
    return(
      data.frame(
        filename = character(),
        size_kb = numeric(),
        modified = as.POSIXct(character()),
        stringsAsFactors = FALSE
      )
    )
  }

  file_info <- file.info(files)

  data.frame(
    filename = basename(files),
    size_kb = round(file_info$size / 1024, 2),
    modified = file_info$mtime,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# Create a ZIP archive containing all CSV files in a directory.
create_dataset_archive <- function(directory, archive_path) {
  ensure_directory(directory)

  files <- list.files(
    directory,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0L) {
    stop("There are no CSV datasets to archive.")
  }

  old_working_directory <- getwd()
  on.exit(setwd(old_working_directory), add = TRUE)

  setwd(directory)

  relative_files <- basename(files)

  # utils::zip() uses the system zip executable where available.
  utils::zip(
    zipfile = archive_path,
    files = relative_files,
    flags = "-j"
  )

  archive_path
}
```

---

## `R/mod_data_input.R`

```r
# R/mod_data_input.R

#' Data input module UI
#'
#' @param id Shiny module namespace ID.
#'
#' @return A Shiny UI object.
mod_data_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Upload dataset"),

    shiny::fileInput(
      inputId = ns("file"),
      label = "Choose a CSV file",
      accept = c(
        ".csv",
        "text/csv",
        "text/comma-separated-values"
      ),
      multiple = FALSE
    ),

    shiny::helpText(
      "Uploaded files are stored on the server."
    ),

    shiny::actionButton(
      inputId = ns("upload"),
      label = "Save dataset",
      class = "btn-primary"
    ),

    shiny::br(),
    shiny::br(),

    shiny::downloadButton(
      outputId = ns("download_archive"),
      label = "Download all datasets"
    ),

    shiny::br(),
    shiny::br(),

    shiny::uiOutput(ns("status"))
  )
}


#' Stored dataset table UI
#'
#' This is separate from the upload controls so it can be placed elsewhere
#' in the application layout.
#'
#' @param id Shiny module namespace ID.
#'
#' @return A Shiny UI object.
mod_data_input_table_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tableOutput(ns("dataset_table"))
}


#' Data input module server
#'
#' @param id Shiny module namespace ID.
#' @param storage_dir Directory in which uploaded datasets are stored.
#'
#' @return A Shiny module server function.
mod_data_input_server <- function(id, storage_dir = "data") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      storage_dir <- normalizePath(
        storage_dir,
        winslash = "/",
        mustWork = FALSE
      )

      ensure_directory(storage_dir)

      status_message <- shiny::reactiveVal(NULL)

      datasets <- shiny::reactiveVal(
        list_csv_datasets(storage_dir)
      )

      output$status <- shiny::renderUI({
        message <- status_message()

        if (is.null(message)) {
          return(NULL)
        }

        shiny::div(
          class = paste0(
            "alert alert-",
            message$type
          ),
          message$text
        )
      })

      shiny::observeEvent(input$upload, {
        status_message(NULL)

        uploaded_file <- input$file

        if (is.null(uploaded_file)) {
          status_message(
            list(
              type = "warning",
              text = "Please choose a CSV file first."
            )
          )

          return()
        }

        original_name <- uploaded_file$name

        if (!grepl("\\.csv$", original_name, ignore.case = TRUE)) {
          status_message(
            list(
              type = "danger",
              text = "Only files with a .csv extension are accepted."
            )
          )

          return()
        }

        # Read the file before storing it so malformed CSV files can be
        # rejected early. This is intentionally conservative for now.
        csv_check <- tryCatch(
          {
            utils::read.csv(
              uploaded_file$datapath,
              stringsAsFactors = FALSE,
              check.names = FALSE,
              nrows = 100
            )

            TRUE
          },
          error = function(error) {
            FALSE
          }
        )

        if (!csv_check) {
          status_message(
            list(
              type = "danger",
              text = "The uploaded file could not be read as a CSV file."
            )
          )

          return()
        }

        destination <- unique_file_path(
          directory = storage_dir,
          filename = original_name
        )

        copied <- file.copy(
          from = uploaded_file$datapath,
          to = destination,
          overwrite = FALSE
        )

        if (!copied) {
          status_message(
            list(
              type = "danger",
              text = "The dataset could not be saved on the server."
            )
          )

          return()
        }

        datasets(list_csv_datasets(storage_dir))

        status_message(
          list(
            type = "success",
            text = paste(
              "Dataset saved as",
              basename(destination)
            )
          )
        )
      })

      output$dataset_table <- shiny::renderTable({
        dataset_info <- datasets()

        if (nrow(dataset_info) == 0L) {
          return(
            data.frame(
              Message = "No datasets have been uploaded yet.",
              stringsAsFactors = FALSE
            )
          )
        }

        dataset_info
      })

      output$download_archive <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "environmental_datasets_",
            format(Sys.Date(), "%Y-%m-%d"),
            ".zip"
          )
        },

        content = function(file) {
          tryCatch(
            {
              create_dataset_archive(
                directory = storage_dir,
                archive_path = file
              )
            },
            error = function(error) {
              stop(
                "Could not create dataset archive: ",
                conditionMessage(error)
              )
            }
          )
        }
      )
    }
  )
}
```

---

## `DESCRIPTION`

```text
Package: environmentalDataExplorer
Title: Environmental Data Explorer
Version: 0.1.0
Description: A modular Shiny application for uploading and managing environmental datasets.
Authors@R: person(
    "Application", 
    "Author",
    email = "example@example.com",
    role = c("aut", "cre")
)
License: MIT
Encoding: UTF-8
Depends:
    R (>= 4.1.0)
Imports:
    shiny
```

---

## `README.md`

```markdown
# Environmental Data Explorer

A modular Shiny application for uploading environmental datasets.

At this stage, the application supports:

- Uploading CSV files through a browser
- Saving uploaded datasets on the server
- Listing stored datasets
- Downloading all datasets as a ZIP archive

## Requirements

- R >= 4.1
- The `shiny` package
- A system ZIP utility available on the server

Install Shiny with:

```r
install.packages("shiny")
```

## Running the application

From the project directory, run:

```r
shiny::runApp()
```

Alternatively:

```r
shiny::runApp("path/to/environmental-data-explorer")
```

## Storage

Uploaded files are stored in:

```text
data/
```

The `data/` directory is created automatically if it does not exist.

The directory is intended to persist between application sessions. When deploying the application, make sure the process running Shiny has write permission for this directory.

## File handling

Only files ending in `.csv` are accepted.

Uploaded filenames are sanitized before storage. If a file with the same name already exists, a numeric suffix is added:

```text
measurements.csv
measurements_1.csv
measurements_2.csv
```

## Extending the explorer

Additional functionality can be implemented as new modules under `R/`.

For example:

```text
R/
├── mod_data_input.R
├── mod_data_preview.R
├── mod_data_filter.R
└── mod_data_plot.R
```

The main `app.R` file should remain a small composition layer that assembles the modules.
```

---

## `data/.gitkeep`

Create an empty file named:

```text
data/.gitkeep
```

This keeps the storage directory in source-control systems such as Git. Uploaded CSV files should generally not be committed.

---

# Optional: create the project archive automatically

If you want to create a ZIP archive containing all of the files above, place the files in the stated directory structure and run this from the parent directory:

```r
zip(
  zipfile = "environmental-data-explorer.zip",
  files = list.files(
    "environmental-data-explorer",
    recursive = TRUE,
    full.names = TRUE
  ),
  flags = "-r9X"
)
```

On Linux or macOS, the equivalent shell command is:

```bash
zip -r environmental-data-explorer.zip environmental-data-explorer/
```

On Windows PowerShell:

```powershell
Compress-Archive `
  -Path environmental-data-explorer `
  -DestinationPath environmental-data-explorer.zip
```

## Important deployment note

The uploaded files are stored on the server filesystem, not in the browser. The deployment user must therefore have write permission for the `data/` directory.

For example, on Linux:

```bash
mkdir -p data
chmod u+rwX data
```

The current implementation validates that the file can be read as CSV by reading the first 100 rows before saving it. For very large datasets, this can later be replaced with a streaming or schema-validation approach.
