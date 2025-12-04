#' Assessment UI
#'
#' @description User interface for the Assessment module, which focuses on
#' data quality checks, sleep/non-wear diary validation, and summarizing steps
#' data from Matrix and StepWatch devices. The UI is structured into two steps
#' to guide the researcher through the validation process.
#'
#' @param id The module namespace ID.
#'
#' @section UI Structure:
#' The UI is divided into two primary sections:
#' \itemize{
#'   \item \strong{1. Diary Validation (Always Visible):} Displays minute-level time series data from Matrix and StepWatch, and provides tools for manual or Excel-based input of sleep/non-wear diary segments. Diary segments are visualized as interactive overlays on the plots (editable regions).
#'   \item \strong{2. Daily Step Summary (Conditionally Visible):} Displays daily aggregated bar plots for Matrix and StepWatch, allowing filtering by cadence threshold. This section is designed for review after diary validation.
#' }
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom shinyjs hidden
#' @importFrom shinyWidgets airDatepickerInput timeInput
#' @importFrom plotly plotlyOutput
#' @import shinycssloaders
#' @noRd
mod_assessment_ui <- function(id) {
  ns <- NS(id)
  
  spacer <- function(px = 8) tags$div(style = paste0("height:", px, "px;"))
  
  tags$div(
    id = ns("wrap"),
    # vpn status (conditional, only if not connected)
    uiOutput(ns("vpn_status_ui")),
    
    # Tutorial
    # shiny::br(),
    # shiny::actionButton(
    #   inputId = ns("start_tutorial_btn"),
    #   label = "Start Tutorial",
    #   icon = shiny::icon("circle-question"),
    #   class = "btn-info btn-sm mb-3"
    # ),
    
    tagList(
      
      fluidRow(
        shiny::column(
          width = 12,
          # === START MAIN ROW ===
          fluidRow(
            class = "g-compact",
            shiny::column(
              width = 4,
              shiny::actionButton(
                ns("preprocess_assessment"), 
                "Click to Add New Participants!",
                icon = icon("play"),
                class = "btn btn-success")
            ),
            # INNER ROW 1: ID Input + Refresh Button
            class = "g-compact align-items-end",
            shiny::column(
              width = 3, # ID Input
              tags$div(
                id = ns("matrix_file_container"),
                selectizeInput(
                  ns("matrix_file"), "Matrix", choices = NULL,
                  options = list(create = FALSE, persist = TRUE,
                                 placeholder = "Pick an Matrix file"
                  )
                )
              )
            ),
            shiny::column(
              width = 3, # ID Input
              tags$div(
                id = ns("stepwatch_file_container"),
                selectizeInput(
                  ns("stepwatch_file"), "Stepwatch", choices = NULL,
                  options = list(create = FALSE, persist = TRUE,
                                 placeholder = "Pick a Stepwatch file"
                  )
                )
              )
            ),
            shiny::column(
              width = 2,
              shiny::actionButton(
                ns("load_ts"), "",
                icon = icon("play"),
                class = "btn btn-success")
            )
          ) 
        )
      ),
      
      bslib::card(
        class = "compact-card",
        bslib::card_header("Participant & File"),
        bslib::card_body(
          fluidRow(
            column(width = 4,
                   h4("Input Diary Segments"),
                   fluidRow(
                     column(6, shinyWidgets::airDatepickerInput(ns("start_date"), "Start Date")),
                     column(6, shinyWidgets::timeInput(ns("start_time"), "Start Time", value = strptime("07:00:00", "%T")))
                   ),
                   fluidRow(
                     column(6, shinyWidgets::airDatepickerInput(ns("end_date"), "End Date")),
                     column(6, shinyWidgets::timeInput(ns("end_time"), "End Time", value = strptime("22:00:00", "%T")))
                   ),
                   actionButton(ns("add_diary_segment"), "Add Diary Segment"),
                   actionButton(ns("update_diary_segment"), "Update Selected Segment", class = "btn-warning"),
                   DT::DTOutput(ns("diary_table")),
                   actionButton(ns("save_diary"), "Save Diary", class = "btn-primary"),
                   br(), br(),
                   h4("Load Diary From Excel"),
                   fileInput(ns("diary_excel"), "Select Excel file (.xlsx)", accept = c(".xlsx",".xls")),
                   checkboxInput(ns("replace_diary"), "Replace existing diary segments", value = TRUE),
                   actionButton(ns("load_diary_excel"), "Load Excel Diary"),
                   downloadButton(ns("download_diary_template"), "Download Template"),
                   br(),
                   tags$small(
                     "Template columns: start_date (YYYY-MM-DD), start_time (HH:MM:SS), ",
                     "end_date (YYYY-MM-DD), end_time (HH:MM:SS)."
                   )
                   # actionButton(ns("add_to_dataset"), "Add to Dataset", class = "btn-success")
            ),
            column(width = 8,
                   h4("Time Series"),
                   checkboxGroupInput(ns("metrics"), "Metrics to plot",
                                      choices = NULL, selected = NULL, inline = TRUE),
                   shinycssloaders::withSpinner(plotlyOutput(ns("time_series_minute")), 
                                                type = 3, color = "#21604C", color.background = "#F8F8F8")
            )
          )
        )
      )
    )
  )
}



#' Assessment Visualizer Server
#'
#' @description Server logic for the Assessment module. This module manages the
#' loading and preprocessing of Matrix and StepWatch data, maintains the reactive
#' list of sleep/non-wear diary segments (editable manually or via Excel/plot drag),
#' and triggers the final processing pipeline (GGIR/data build).
#'
#' @param id Module ID
#' @param matrix_file Reactive path to matrix .bin file (or equivalent).
#' @param stepwatch_file Reactive path to stepwatch .csv file.
#' @param diary_file Currently unused (placeholder for future direct diary input).
#' @param participant_id Reactive participant ID.
#' @param outdirs Reactive list of output directories needed for file persistence (e.g., sleep diaries).
#'
#' @section Core Logic and Features:
#' \itemize{
#'   \item \strong{Diary Management:} Manages the reactive \code{diary_segments} list, allowing addition, deletion, update via DT selection, and editing directly by dragging the overlay shapes on the minute-level plots (\code{plotly_relayout} event).
#'   \item \strong{Visualization:} Renders minute-level time series for Matrix and StepWatch with interactive diary overlays. Renders daily bar plots filtered by cadence.
#'   \item \strong{Data Preprocessing:} Calls internal functions like \code{preprocess_matrix} and \code{preprocess_stepwatch} to prepare device data.
#'   \item \strong{Final Pipeline:} The \code{add_to_dataset} button triggers the multi-step process using \code{withProgress} to combine all sleep logs, run GGIR parts 3-5 (via \code{process_matrix}), and build the final dataset (\code{build_final_dataset}).
#' }
#'
#' @import shiny
#' @import shinybusy
#' @importFrom DT renderDT datatable
#' @importFrom plotly renderPlotly layout event_data
#' @importFrom shinyWidgets updateAirDateInput updateTimeInput
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom GGIR iso8601chartime2POSIX
#' @importFrom plyr rbind.fill
#' @importFrom stats as.formula
#' @importFrom RColorBrewer brewer.pal
#' @noRd
mod_assessment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    section <- reactiveVal(0)
    
    matrix_data <- reactiveVal(NULL)
    stepwatch_data <- reactiveVal(NULL)
    ts_data <- reactiveVal(NULL)
    diary_segments <- reactiveVal(data.frame(start = as.POSIXct(character()), end = as.POSIXct(character())))
    selected_row <- reactiveVal(NULL)
    
    COLOR_MAP <- list(
      "steps_matrix"    = "rgb(31, 119, 180)",  # Blue
      "steps_stepwatch" = "rgb(255, 127, 14)",  # Orange
      "anglez"          = "rgb(44, 162, 95)",   # Green
      "ENMO"            = "rgb(214, 39, 40)"    # Red
    )
    
    # ---- File Choice Population ----
    observe({
      # Populate Matrix files
      if (dir.exists(MATRIX_META_DIR)) {
        # Fix: This lists only the file names (e.g., "P001.RData")
        matrix_files <- list.files(MATRIX_META_DIR, pattern = "\\.RData$", full.names = FALSE)
        matrix_clean = gsub(".RData", "", gsub("meta_", "", matrix_files))
        # Use names for the choices list to ensure the display text (name) matches the return value (name)
        updateSelectizeInput(session, "matrix_file",
                             choices = matrix_clean,
                             selected = "")
      } else {
        showNotification(paste("Matrix meta directory not found:", MATRIX_META_DIR), type = "error")
      }
      
      # 2. Populate StepWatch files (assuming STEPWATCH_META_DIR is also a global constant)
      if (dir.exists(STEPWATCH_META_DIR)) {
        # Fix: Use full.names = FALSE
        stepwatch_files <- list.files(STEPWATCH_META_DIR, pattern = "\\.RData$", full.names = FALSE)
        names(stepwatch_files) <- stepwatch_files
        updateSelectizeInput(session, "stepwatch_file", 
                             choices = c("Pick a Stepwatch file" = "", stepwatch_files), 
                             selected = "")
      } else {
        showNotification(paste("StepWatch meta directory not found:", STEPWATCH_META_DIR), type = "error")
      }
    }, priority = 100)
    
    # ---- Matrix Data Loading ----
    observeEvent(input$load_ts, {
      req(input$matrix_file, input$stepwatch_file)
      
      if (!is.null(input$matrix_file)) {
        matrix_path <- file.path(MATRIX_META_DIR, paste0("meta_", input$matrix_file, ".RData"))
        if (!file.exists(matrix_path)) {
          showNotification(paste("Matrix file not found:", matrix_path), type = "error")
          matrix_data(NULL) # Clear data
          return()
        }
      }
      
      if (!is.null(input$stepwatch_file)) {
        stepwatch_path <- file.path(STEPWATCH_META_DIR, input$stepwatch_file)
        if (!file.exists(stepwatch_path)) {
          showNotification(paste("stepwatch file not found:", stepwatch_path), type = "error")
          stepwatch_data(NULL) # Clear data
          return()
        }
      }
      
      env <- new.env()
      shinybusy::show_modal_spinner(text = "Processing data...",
                                    spin = "atom", color = "#21604C")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      tryCatch({
        # Load the RData content into the 'env' environment
        load(matrix_path, envir = env)

        # 1. Check if 'M$metashort' is present in the loaded environment
        if ("M" %in% ls(env) && !is.null(env$M$metashort)) {
          colnames(env$M$metashort) = gsub("step_count", "steps", colnames(env$M$metashort))
          matrix_df = env$M$metashort
          matrix_df$timestamp = GGIR::iso8601chartime2POSIX(matrix_df$timestamp, tz = "Europe/Madrid")
          steps = resample_epoch(matrix_df[, c("timestamp", "steps")])
          angle = resample_epoch(matrix_df[, c("timestamp", "anglez")], FUN = mean)
          enmo = resample_epoch(matrix_df[, c("timestamp", "ENMO")], FUN = mean)
          matrix_df_min = merge(steps[, c("timestamp", "steps")],
                                angle[, c("timestamp", "anglez")], by = "timestamp")
          matrix_df_min = merge(matrix_df_min, enmo[, c("timestamp", "ENMO")], by = "timestamp")
          matrix_data(matrix_df_min) 
          showNotification("Matrix data loaded successfully.", type = "message", duration = 3)
        } else {
          # This handles if the file is loaded but the internal structure is wrong
          showNotification("Could not find expected data structure (M$metashort) in RData file.",type = "warning")
          matrix_data(NULL)
        }
      }, error = function(e) {
        showNotification(paste("Error loading/processing Matrix data:", e$message), type = "error", duration = NULL)
        matrix_data(NULL)
      })
      
      # stepwatch_data()
      tryCatch({
        # Load the RData content into the 'env' environment
        load(stepwatch_path, envir = env)
        
        # 1. Check if the expected object (e.g., 'M' containing 'metashort') is present in the loaded environment
        if ("stepwatch" %in% ls(env) && !is.null(env$stepwatch)) {
          stepwatch_data(env$stepwatch) 
          showNotification("Stepwatch data loaded successfully.", type = "message", duration = 3)
        } else {
          # This handles if the file is loaded but the internal structure is wrong
          showNotification("Could not find expected data structure in file.",type = "warning")
          stepwatch_data(NULL)
        }
      }, error = function(e) {
        showNotification(paste("Error loading/processing stepwatch data:", e$message), type = "error", duration = NULL)
        stepwatch_data(NULL) # CRITICAL: Reset data on failure to clear plot
      })
      
      # now merge
      if (!is.null(matrix_data()) & !is.null(stepwatch_data())) {
        merged <- merge(matrix_data(), stepwatch_data()[, c("timestamp", "steps")], 
                       by = "timestamp",
                       suffixes = c("_matrix", "_stepwatch"), all = T)
        merged <- merged[order(merged$timestamp), c("timestamp",
                                                   "steps_matrix", "steps_stepwatch",
                                                   "anglez", "ENMO")]
        ts_data(merged)
      } else if (!is.null(matrix_data()) & is.null(stepwatch_data())) {
        mtx = matrix_data()
        colnames(mtx) <- gsub("steps", "steps_matrix", colnames(mtx))
        ts_data(matrix_data())
      } else if (is.null(matrix_data()) & !is.null(stepwatch_data())) {
        stw = stepwatch_data()
        colnames(stw) <- gsub("steps", "steps_stepwatch", colnames(stw))
        ts_data(stepwatch_data())
      } else {
        ts_data(NULL)
      }
    })
    
    # ---- load diary from Excel ----
    observeEvent(input$load_diary_excel, {
      req(input$diary_excel)
      
      shinybusy::show_modal_spinner(text = "Reading diary from Excel...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      tryCatch({
        raw <- readxl::read_excel(input$diary_excel$datapath)
        parsed <- parse_diary_df(raw)
        
        if (nrow(parsed) == 0) {
          showNotification("No valid rows found in the Excel file.", type = "warning")
          return()
        }
        
        if (isTRUE(input$replace_diary)) {
          diary_segments(parsed)
        } else {
          diary_segments(rbind(diary_segments(), parsed))
        }
        
        showNotification(sprintf("Loaded %d diary segment(s) from Excel.", nrow(parsed)),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Failed to load Excel diary:", e$message),
                         type = "error", duration = NULL)
      })
    })
    
    # ---- download: blank template ----
    output$download_diary_template <- downloadHandler(
      filename = function() sprintf("diary_template_%s.xlsx", input$matrix_file),
      content = function(file) {
        # empty rows as an example
        tpl <- data.frame(
          start_date = format(as.Date("2025-01-01")),
          start_time = format("22:00:00"),
          end_date   = format(as.Date("2025-01-02")),
          end_time   = format("06:00:00"),
          stringsAsFactors = FALSE
        )
        message(file)
        writexl::write_xlsx(tpl, path = file)
      }
    )
    
    # Update date pickers to only allow dates present in the loaded data
    observeEvent(list(matrix_data(), stepwatch_data()), {
      # pick the data source you prefer; here we use whichever is available
      df_list <- list(matrix = matrix_data(), stepwatch = stepwatch_data())
      # combine timestamps from available sources
      timestamps <- do.call(c, lapply(df_list, function(df) {
        if (is.null(df)) return(NULL)
        if (!"timestamp" %in% names(df)) return(NULL)
        df$timestamp
      }))
      
      if (length(timestamps) == 0) {
        # No data loaded -> reset picker constraints (optional)
        updateAirDateInput(session, "start_date", value = NULL, 
                           options = list(minDate = NULL, maxDate = NULL))
        updateAirDateInput(session, "end_date",   value = NULL, 
                           options = list(minDate = NULL, maxDate = NULL))
        return()
      }
      
      # Convert to Date and compute min/max
      dates <- as.Date(timestamps)
      minD <- min(dates, na.rm = TRUE)
      maxD <- max(dates, na.rm = TRUE)
      
      # Set defaults: start -> min, end -> max (or other sensible defaults)
      updateAirDateInput(session, "start_date",
                         value = minD,
                         options = list(
                           minDate = minD,
                           maxDate = maxD 
                         ))
      updateAirDateInput(session, "end_date",
                         value = maxD,
                         options = list(
                           minDate = minD,
                           maxDate = maxD 
                         ))
    })
    
    observeEvent(input$add_diary_segment, {
      req(input$start_date, input$end_date, input$start_time, input$end_time)
      start <- as.POSIXct(paste(input$start_date, as.character(input$start_time)), format = "%Y-%m-%d %H:%M", tz = "Europe/Madrid")
      end <- as.POSIXct(paste(input$end_date, as.character(input$end_time)), format = "%Y-%m-%d %H:%M", tz = "Europe/Madrid")
      if (is.na(start) || is.na(end) || start >= end) {
        showNotification("Invalid diary segment: check date/time fields.", type = "error")
        return()
      }
      new_seg <- data.frame(start = start, end = end)
      diary_segments(rbind(diary_segments(), new_seg))
    })
    
    observeEvent(input$update_diary_segment, {
      req(selected_row(), input$start_date, input$start_time, input$end_date, input$end_time)
      start <- as.POSIXct(paste(input$start_date, as.character(input$start_time)), format = "%Y-%m-%d %H:%M")
      end <- as.POSIXct(paste(input$end_date, as.character(input$end_time)), format = "%Y-%m-%d %H:%M")
      if (is.na(start) || is.na(end) || start >= end) {
        showNotification("Invalid diary segment update.", type = "error")
        return()
      }
      segs <- diary_segments()
      segs[selected_row(), ] <- data.frame(start = start, end = end)
      diary_segments(segs)
    })
    
    output$diary_table <- DT::renderDT({
      diary_data <- diary_segments()
      if (nrow(diary_data) > 0 && inherits(diary_data$start, "POSIXct")) {
        diary_data$start <- format(diary_data$start, 
                                   format = "%Y-%m-%d %H:%M:%S", 
                                   tz = "Europe/Madrid")
        diary_data$end <- format(diary_data$end, 
                                 format = "%Y-%m-%d %H:%M:%S", 
                                 tz = "Europe/Madrid")
      }
      DT::datatable(diary_data, 
                    selection = "single", 
                    options = list(dom = 't', paging = FALSE))
    })
    
    observeEvent(input$diary_table_rows_selected, {
      idx <- input$diary_table_rows_selected
      if (length(idx) == 1) {
        selected_row(idx)
        seg <- diary_segments()[idx, ]
        updateAirDateInput(session, "start_date", value = as.Date(seg$start))
        updateTimeInput(session, "start_time", value = format(seg$start, "%H:%M:%S"))
        updateAirDateInput(session, "end_date", value = as.Date(seg$end))
        updateTimeInput(session, "end_time", value = format(seg$end, "%H:%M:%S"))
      }
    })
    
    observeEvent(input$preprocess_assessment, {
      # first: preprocess matrix
      preprocess_matrix()
      
      # second: preprocess stepwatch
      preprocess_stepwatch()
    })
    
    plot_with_diary_overlay <- function(df, diary) {
      p <- timeseries_plots(df, x = "timestamp", y = "steps", interactive = TRUE)
      if (nrow(diary) > 0) {
        shapes <- lapply(1:nrow(diary), function(i) {
          list(
            type = "rect", x0 = diary$start[i], x1 = diary$end[i],
            y0 = 0, y1 = 1, xref = "x", yref = "paper",
            fillcolor = "rgba(255, 0, 0, 0.2)", line = list(width = 0),
            editable = TRUE
          )
        })
        p <- p %>% layout(shapes = shapes)
      }
      p
    }
    
    # ----- plot ------
    # update metrics choices from the main dataframe (e.g. matrix_data)
    observe({
      df <- ts_data()
      if (is.null(df)) return()
      avail <- setdiff(names(df), "timestamp")
      # optionally keep only numeric-ish columns:
      is_num <- sapply(df[avail], function(col) {
        # treat as numeric if coercion yields some non-NA numeric values
        suppressWarnings(!all(is.na(as.numeric(col))))
      })
      avail <- avail[is_num]
      updateCheckboxGroupInput(session, "metrics", choices = avail, selected = avail[1])
    })
    
    # helper: create plotly y-axis specs for multiple axes
    create_yaxes <- function(selected_metrics, df) {
      # 1. Calculate the overall maximum range across all selected metrics
      combined_rng <- c(0, 0)
      
      for (name in selected_metrics) {
        rng <- range(as.numeric(df[[name]]), na.rm = TRUE, finite = TRUE)
        if (length(rng) == 2) {
          combined_rng[1] <- min(combined_rng[1], rng[1])
          combined_rng[2] <- max(combined_rng[2], rng[2])
        }
      }
      
      # Ensure minimum of 0 is included (rangemode="tozero" style)
      combined_rng[1] <- min(0, combined_rng[1])
      
      # 2. Add padding to the combined max range
      pad <- diff(combined_rng) * 0.06
      if (!is.finite(pad) || pad == 0) pad <- 1
      
      # 3. Define only the primary left axis ('y')
      axes <- list()
      axes[["y"]] <- list(
        title = paste(selected_metrics, collapse = " | "), # Display all metric names
        rangemode = "tozero",
        range = c(combined_rng[1] - pad, combined_rng[2] + pad),
        titlefont = list(color = "black"),
        tickfont = list(color = "black")
      )
      return(axes)
    }
    
    # color palette
    palette_colors <- function(n) {
      if (n < 3) n = 3
      cols <- RColorBrewer::brewer.pal(n, "Set1") # fallback helper
      # robust color generation:
      if (n <= 9) RColorBrewer::brewer.pal(n, "Set1")
      else grDevices::rainbow(n)
    }
    
    # render plotly with multiple y axes
    output$time_series_minute <- renderPlotly({
      req(ts_data(), input$metrics)
      df <- ts_data()
      metrics <- input$metrics
      if (length(metrics) == 0) return(NULL)
      
      # handle na_action
      # na_action <- input$na_action %||% "gap"  # default 'gap'
      na_action <- "gap"
      plot_df <- df
      plot_df$timestamp <- plot_df$timestamp  # assume already POSIXct

      # Prepare numeric columns and NA handling per chosen metric
      for (m in metrics) {
        vec <- as.numeric(plot_df[[m]])
        if (m == "ENMO") vec <- vec * 1000
        if (na_action == "zero") vec[is.na(vec)] <- 0
        if (na_action == "interpolate") {
          if (!all(is.na(vec))) vec <- zoo::na.approx(vec, x = as.numeric(plot_df$timestamp), na.rm = FALSE)
        }
        plot_df[[m]] <- vec
      }
      
      # Build plotly traces
      n <- length(metrics)
      plt <- plot_ly()
      
      # Add traces, first metric uses default y-axis (y), others use y2,y3...
      for (i in seq_along(metrics)) {
        m <- metrics[i]
        color <- COLOR_MAP[[m]]
        if (is.null(color)) {
          color <- "grey50" 
        }
        plt <- add_trace(plt,
                         data = plot_df,
                         x = ~timestamp,
                         y = stats::as.formula(paste0("~`", m, "`")),
                         type = "scatter",
                         mode = "lines",
                         name = m,
                         line = list(color = color),
                         yaxis = "y",
                         hoverinfo = "x+y",
                         connectgaps = (na_action != "gap"))
      }
      
      # diary overlay as shapes (if you have diary data with start_time/end_time)
      if (!is.null(diary_segments()) && nrow(diary_segments()) > 0) {
        # compute a ymax for the rectangles: use maximum of all plotted metrics
        combined_max <- max(sapply(metrics, function(m) max(plot_df[[m]], na.rm = TRUE)), na.rm = TRUE)
        if (!is.finite(combined_max)) combined_max <- 1
        shapes <- list()
        for (i in seq_len(nrow(diary_segments()))) {
          s <- diary_segments()$start[i]
          e <- diary_segments()$end[i]
          shapes[[length(shapes) + 1]] <- list(type = "rect",
                                               x0 = s, x1 = e,
                                               y0 = 0, y1 = combined_max * 1.02,
                                               xref = "x", yref = "y",
                                               fillcolor = "rgba(255, 0, 0, 0.12)",
                                               line = list(width = 0),
                                               layer = "below")
        }
        # we'll add shapes to layout below
      } else shapes <- NULL
      
      # create y-axes spec
      axes_spec <- create_yaxes(metrics, plot_df)
      
      # assemble layout axes arguments dynamically
      layout_args <- list(
        xaxis = list(title = "Time", type = "date", rangeslider = list(visible = TRUE)),
        yaxis = axes_spec[["y"]]
      )
      
      layout_args$shapes <- shapes
      layout_args$legend <- list(orientation = "h")
      layout_args$margin <- list(r = 80 + 60 * max(0, length(metrics) - 1), t = 50) # make space for right axes
      
      plt <- do.call(plotly::layout, c(list(p = plt), layout_args))
      
      style_plotly(plt)
    })
    
    # helper - overwrite diary entry
    showOverwriteModal <- function(ns) {
      modalDialog(
        title = icon("triangle-exclamation", class = "text-warning"), "Overwrite Existing Diary?",
        p(
          "A sleep diary log already exists for this participant ID in the final dataset file.",
          "Do you want to overwrite the existing entry with the current diary segments?"
        ),
        footer = tagList(
          modalButton("No, Cancel Save"),
          actionButton(ns("confirm_overwrite"), "Yes, Overwrite Entry", class = "btn-warning")
        ),
        easyClose = FALSE
      )
    }
    
    observeEvent(input$save_diary, {
      req(nrow(diary_segments()) > 0, input$matrix_file)
      
      # Where to save the log
      log_dir  <- file.path(WEARABLE_OUTPUT_DIR, "output", "sleeplog")
      log_path_i <- file.path(log_dir, "individual", sprintf("sleep_log_%s.csv", input$matrix_file))
      log_path <- file.path(log_dir, "ggir_sleep_log.csv")
      
      # 1) Write GGIR sleep log
      try({
        # build formatted individual log
        log_i = data.frame(
          start_date = format(diary_segments()$start, format = "%Y-%m-%d"),
          start_time = format(diary_segments()$start, format = "%H:%M:%S"),
          end_date   = format(diary_segments()$end, format = "%Y-%m-%d"),
          end_time   = format(diary_segments()$end, format = "%H:%M:%S"),
          stringsAsFactors = FALSE
        )
        
        # build 1-row ggir sleep log
        ggir_log = ggir_sleep_log(diary_segments(), input$matrix_file)
        
        # need overwriting?
        overwrite_needed = FALSE
        log_available = FALSE
        if (file.exists(log_path)) {
          old_log = data.table::fread(log_path, data.table = FALSE)
          log_available = TRUE
          if (participant_id %in% old_log$ID) {
            overwrite_needed = TRUE
          }
        }
        
        # if need overwriting, ask for confirmation, else combine and save
        if (log_available) {
          if (overwrite_needed) {
            showModal(showOverwriteModal(ns))
          } else {
            # save individual file
            utils::write.csv(log_i, file = log_path_i, 
                             row.names = FALSE, na = "")
            # combine with previous
            ggir_log = plyr::rbind.fill(old_log, ggir_log)
            utils::write.csv(ggir_log, file = log_path, 
                             row.names = FALSE, na = "")
          }
        } else {
          # if log is not available at all, then save existing files
          utils::write.csv(log_i, file = log_path_i, 
                           row.names = FALSE, na = "")
          utils::write.csv(ggir_log, file = log_path, row.names = FALSE, na = "")
        }
      }, silent = TRUE)
      showNotification(paste("Diary saved at", dirname(log_path)), 
                       type = "message", duration = 6)
    })
    
    # Handle the action after the user confirms overwriting
    observeEvent(input$confirm_overwrite, {
      # Remove the modal
      removeModal()
      
      # Recalculate IDs and paths (since we're in a separate observer)
      req(nrow(diary_segments()) > 0, input$matrix_file)
      participant_id <- gsub("\\.RData$", "", gsub("^meta_", "", input$matrix_file))
      log_dir  <- file.path(WEARABLE_OUTPUT_DIR, "output", "sleeplog")
      log_path_i <- file.path(log_dir, "individual", sprintf("sleep_log_%s.csv", input$matrix_file))
      log_path <- file.path(log_dir, "ggir_sleep_log.csv")
      
      # Derive diaries
      log_i = data.frame(
        start_date = format(diary_segments()$start, format = "%Y-%m-%d"),
        start_time = format(diary_segments()$start, format = "%H:%M:%S"),
        end_date   = format(diary_segments()$end, format = "%Y-%m-%d"),
        end_time   = format(diary_segments()$end, format = "%H:%M:%S"),
        stringsAsFactors = FALSE
      )
      ggir_log = ggir_sleep_log(diary_segments(), participant_id)
      
      # save individual file
      utils::write.csv(log_i, file = log_path_i, 
                       row.names = FALSE, na = "")
      # combine with previous
      ggir_log = plyr::rbind.fill(old_log, ggir_log)
      utils::write.csv(ggir_log, file = log_path, 
                       row.names = FALSE, na = "")
    })
    
    observeEvent(plotly::event_data("plotly_relayout"), {
      ed <- plotly::event_data("plotly_relayout")
      if (length(ed) == 0) return()
      diary <- diary_segments()
      updated <- FALSE
      for (i in seq_len(nrow(diary))) {
        x0key <- sprintf("shapes[%d].x0", i - 1)
        x1key <- sprintf("shapes[%d].x1", i - 1)
        if (x0key %in% names(ed)) diary$start[i] <- as.POSIXct(ed[[x0key]], tz = "UTC")
        if (x1key %in% names(ed)) diary$end[i] <- as.POSIXct(ed[[x1key]], tz = "UTC")
        if (x0key %in% names(ed) || x1key %in% names(ed)) updated <- TRUE
      }
      if (updated) diary_segments(diary)
    })
    
    # observeEvent(input$add_to_dataset, {
    #   req(outdirs(), participant_id())
    #   
    #   showModal(modalDialog("Adding participant to dataset...",
    #                         footer = NULL, easyClose = FALSE))
    #   on.exit(removeModal(), add = TRUE)
    #   
    #   withProgress(message = "Building dataset", value = 0, {
    #     incProgress(0.1, detail = "Collecting all sleep logs...")
    #     
    #     # 1. Combine sleep logs
    #     log_dir <- file.path(outdirs()$sleep_diaries_dir(), "individual")
    #     ggir_sleep_diary <- file.path(outdirs()$sleep_diaries_dir(), "GGIR_sleep_diary.csv")
    #     combine_ggir_sleep_logs(log_dir, ggir_sleep_diary)
    #     
    #     incProgress(0.4, detail = "Running GGIR parts 3-5...")
    #     
    #     # 2. Run GGIR parts 3-5 with combined log
    #     ggir_output = process_matrix(outdirs()$matrix_timeseries_dir(),
    #                                  ggir_sleep_diary) 
    #     incProgress(0.7, detail = "Extracting variables of interest...")
    #     
    #     # 3. Calculate Step Metrics
    #     
    #     
    #     # 3. Extract variables
    #     # final_dataset = build_final_dataset(ggir_output)
    #     
    #     incProgress(1, detail = "Completed")
    #   })
    #   
    #   # 4. Notify user
    #   showNotification(
    #     sprintf("Participant %s has been correctly added to dataset", participant_id()),
    #     type = "message",
    #     duration = 8
    #   )
    # })
  })
}