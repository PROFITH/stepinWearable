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
  tagList(
    h3("1. Diary Validation"),
    
    fluidRow(
      column(width = 8,
             h4("MATRIX: Minute-level Step Time Series"),
             shinycssloaders::withSpinner(plotlyOutput(ns("steps_matrix_minute")), type = 4),
             h4("STEPWATCH: Minute-level Step Time Series"),
             shinycssloaders::withSpinner(plotlyOutput(ns("steps_stepwatch_minute")), type = 4)
      ),
      column(width = 4,
             h4("Load Diary From Excel"),
             fileInput(ns("diary_excel"), "Select Excel file (.xlsx)", accept = c(".xlsx",".xls")),
             checkboxInput(ns("replace_diary"), "Replace existing diary segments", value = TRUE),
             actionButton(ns("load_diary_excel"), "Load Excel Diary"),
             downloadButton(ns("download_diary_template"), "Download Excel Template"),
             tags$small(
               "Template columns: start_date (YYYY-MM-DD), start_time (HH:MM:SS), ",
               "end_date (YYYY-MM-DD), end_time (HH:MM:SS). ",
               "Alternatively: start and end as full timestamps."
             ),
             br(), br(),
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
             br(),
             actionButton(ns("save_diary"), "Save Diary", class = "btn-primary"),
             actionButton(ns("add_to_dataset"), "Add to Dataset", class = "btn-success")
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 1", ns("section")),
      h3("2. Daily Step Summary"),
      fluidRow(
        column(width = 6,
               h4("MATRIX: Daily Step Totals"),
               selectInput(ns("cadence_filter"), "Select cadence:",
                           choices = c("Total steps", "Steps \u2265 80", 
                                       "Steps \u2265 90", "Steps \u2265 100")),
               plotlyOutput(ns("steps_matrix_day"))
        ),
        column(width = 6,
               h4("STEPWATCH: Daily Step Totals"),
               selectInput(ns("cadence_filter_stepwatch"), "Select cadence:",
                           choices = c("Total steps", "Steps \u2265 80", "Steps \u2265 90", "Steps \u2265 100")),
               plotlyOutput(ns("steps_stepwatch_day"))
        )
      )
    ),
    
    hidden(textInput(ns("section"), "", value = 0))  # toggles section 1 vs 2
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
#' @noRd
mod_assessment_server <- function(id, matrix_file, stepwatch_file, diary_file, participant_id, outdirs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    section <- reactiveVal(0)
    # observeEvent(input$continue_to_daily, {
    #   section(1)
    #   updateTextInput(session, "section", value = "1")
    # })
    
    diary_segments <- reactiveVal(data.frame(start = as.POSIXct(character()), end = as.POSIXct(character())))
    selected_row <- reactiveVal(NULL)
    
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
      filename = function() sprintf("diary_template_%s.xlsx", participant_id()),
      content = function(file) {
        # empty rows as an example
        tpl <- data.frame(
          start_date = as.Date(character()),
          start_time = character(),
          end_date   = as.Date(character()),
          end_time   = character(),
          stringsAsFactors = FALSE
        )
        writexl::write_xlsx(list("diary" = tpl), path = file)
      }
    )
    
    observeEvent(input$add_diary_segment, {
      req(input$start_date, input$end_date, input$start_time, input$end_time)
      start <- as.POSIXct(paste(input$start_date, as.character(input$start_time)), format = "%Y-%m-%d %H:%M")
      end <- as.POSIXct(paste(input$end_date, as.character(input$end_time)), format = "%Y-%m-%d %H:%M")
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
      DT::datatable(diary_segments(), selection = "single", options = list(dom = 't', paging = FALSE))
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
    
    matrix_data <- reactive({
      req(matrix_file())
      preprocess_matrix(matrix_file(), participant_id(), outdirs()$processed_matrix_dir())
    })
    
    stepwatch_data <- reactive({
      req(stepwatch_file())
      preprocess_stepwatch(stepwatch_file(), participant_id(), outdirs()$processed_stepwatch_dir())
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
    
    output$steps_matrix_minute <- renderPlotly({
      req(matrix_data())
      plot_with_diary_overlay(matrix_data(), diary_segments())
    })
    
    output$steps_stepwatch_minute <- renderPlotly({
      req(stepwatch_data())
      plot_with_diary_overlay(stepwatch_data(), diary_segments())
    })
    
    observeEvent(input$save_diary, {
      req(nrow(diary_segments()) > 0, matrix_file(), outdirs())
      
      # Where to save the log
      log_dir  <- file.path(outdirs()$sleep_diaries_dir(), "individual")
      log_path <- file.path(log_dir, sprintf("sleep_log_%s.csv", participant_id()))
      
      # 1) Write GGIR sleep log
      try({
        ggir_sleep_log(diary_segments(), participant_id(), log_path)
      }, silent = TRUE)
      showNotification(paste("Diary saved at", log_path), type = "message", duration = 6)
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
    
    daily_matrix_plot_result <- reactive({
      req(matrix_data(), input$cadence_filter, section() == 1)
      cutoff <- switch(input$cadence_filter,
                       "Steps \u2265 80" = 80,
                       "Steps \u2265 90" = 90,
                       "Steps \u2265 100" = 100,
                       NULL)
      daily_plots(matrix_data(), x = "timestamp", y = "steps", interactive = TRUE, cadence_cutoff = cutoff)
    })
    
    daily_stepwatch_plot_result <- reactive({
      req(stepwatch_data(), input$cadence_filter_stepwatch, section() == 1)
      cutoff <- switch(input$cadence_filter_stepwatch,
                       "Steps \u2265 80" = 80,
                       "Steps \u2265 90" = 90,
                       "Steps \u2265 100" = 100,
                       NULL)
      daily_plots(stepwatch_data(), x = "timestamp", y = "steps", interactive = TRUE, cadence_cutoff = cutoff)
    })
    
    output$steps_matrix_day <- renderPlotly({
      req(daily_matrix_plot_result())
      daily_matrix_plot_result()$plot
    })
    
    output$steps_stepwatch_day <- renderPlotly({
      req(daily_stepwatch_plot_result())
      daily_stepwatch_plot_result()$plot
    })
    
    observeEvent(input$add_to_dataset, {
      req(outdirs(), participant_id())
      
      showModal(modalDialog("Adding participant to dataset...",
                            footer = NULL, easyClose = FALSE))
      on.exit(removeModal(), add = TRUE)
      
      withProgress(message = "Building dataset", value = 0, {
        incProgress(0.1, detail = "Collecting all sleep logs...")
        
        # 1. Combine sleep logs
        log_dir <- file.path(outdirs()$sleep_diaries_dir(), "individual")
        ggir_sleep_diary <- file.path(outdirs()$sleep_diaries_dir(), "GGIR_sleep_diary.csv")
        combine_ggir_sleep_logs(log_dir, ggir_sleep_diary)
        
        incProgress(0.4, detail = "Running GGIR parts 3-5...")
        
        # 2. Run GGIR parts 3-5 with combined log
        ggir_output = process_matrix(outdirs()$matrix_timeseries_dir(),
                                     ggir_sleep_diary) 
        incProgress(0.7, detail = "Extracting variables of interest...")
        
        # 3. Calculate Step Metrics
        
        
        # 3. Extract variables
        # final_dataset = build_final_dataset(ggir_output)
        
        incProgress(1, detail = "Completed")
      })
      
      # 4. Notify user
      showNotification(
        sprintf("Participant %s has been correctly added to dataset", participant_id()),
        type = "message",
        duration = 8
      )
    })
  })
}