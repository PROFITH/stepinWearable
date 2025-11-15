#' Intervention Server (Shiny module)
#'
#' Server-side module that drives the STEP-IN intervention workflow:
#' Fitbit file selection and preprocessing, 14-day window selection,
#' KPI calculation, automatic message/target generation (with manual
#' override), and persistence of windowed data plus audit state.
#'
#' @param id Module namespace ID.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Polls VPN/server availability via \code{check_vpn_and_server_access()} and
#'         surfaces a banner when access is not possible.
#'   \item Manages a light participant registry (ID \eqn{\leftrightarrow} name) so
#'         analysts can reuse IDs and keep names consistent.
#'   \item Loads minute-level Fitbit CSV using \pkg{shinyFiles}, preprocesses with
#'         \code{preprocess_fitbit()}, and derives a daily summary via \code{daily_summary()}.
#'   \item Lets the analyst pick or default a 14-day analysis window and computes KPIs
#'         (\code{kpis()}) on current and previous windows.
#'   \item Calls the decision engine \code{decide_message()} to obtain the next targets
#'         (X, Y, Z) and an automatic message; allows overriding the template while
#'         keeping targets consistent.
#'   \item Saves the windowed time series and an audit entry to participant state with
#'         \code{save_participant_state()}.
#' }
#'
#' The module renders Plotly visualizations (minute and daily views), exposes a
#' tutorial tour (via \pkg{rintrojs}), and writes state files per participant and
#' t-index under a package-configured output root.
#'
#' @section State file contents (per participant):
#' The saved state contains the latest targets (`last_X`, `last_Y`, `last_Z`),
#' the `consecutive_fails` counter, and a `history` list with one entry per
#' processed \eqn{t}-index. Each history entry stores:
#' \describe{
#'   \item{t_index}{Integer. Cycle index \eqn{t} (0 = post-basal, 1..7 = months 1–3, 8+ = months 4–9).}
#'   \item{date_str}{Character. Start date (YYYYMMDD) of the corresponding minute series.}
#'   \item{start_date}{Date. First day of the 14-day analysis window used for this entry.}
#'   \item{end_date}{Date. Last day of the 14-day analysis window used for this entry.}
#'   \item{kpis}{Named list of window medians used by the decision engine, typically:
#'     `med_steps_day`, `med_steps_80plus`, `med_steps_90plus`, `med_steps_100plus`.}
#'   \item{steps_factor}{Numeric. Multiplication factor applied when computing the next X.}
#'   \item{minutes_inc}{Integer. Minute increment applied when computing/adjusting Y.}
#'   \item{target_steps}{Numeric. Next \strong{X} target (steps/day).}
#'   \item{target_minutes}{Integer. Next \strong{Y} target (minutes/day at cadence Z).}
#'   \item{target_cadence}{Integer. Next \strong{Z} target (one of 80, 90, 100 steps/min).}
#'   \item{auto_message_key}{Character. Template key chosen by the engine
#'     (e.g., `"msg0"`, `"pasos1"`, ..., `"ambos8"`, `"nodata3"`).}
#'   \item{final_message_key}{Character. Template key finally used/shown (may differ if overridden).}
#'   \item{manual_override}{Logical. `TRUE` iff `final_message_key` differs from `auto_message_key`.}
#'   \item{message}{Character. Fully rendered WhatsApp message text stored for audit.}
#' }
#'
#' @section Participant info file contents (per participant):
#' Participant ID, participant name, laste update timestamp.
#' 
#' @return Invisibly returns \code{NULL}. Outputs are registered in the UI using
#'         the provided \code{id} namespace.
#'
#' @import dplyr
#' @importFrom plotly renderPlotly
#' @importFrom shiny showModal modalDialog modalButton removeModal showNotification outputOptions
#' @importFrom lubridate with_tz as_date force_tz
#' @import rintrojs
#' @importFrom htmltools HTML htmlEscape
#' @noRd
mod_intervention_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Small helpers
    fmt_int <- function(x) {
      if (is.null(x) || is.na(x)) return(0L)
      as.integer(x)
    }
    fmt_num <- function(x) {
      if (is.null(x) || is.na(x)) return(0)
      as.numeric(x)
    }
    fmt_big <- function(x) format(x, big.mark = ".", decimal.mark = ",")
    
    # ---- Interactive tutorial (rintrojs) ----
    observeEvent(input$start_tutorial_btn, {
      
      # Tutorial steps
      steps <- data.frame(
        element = c(
          paste0("#", ns("id_container")),                 # Step 1: select/add ID
          paste0("#", ns("name")),                         # Step 2: confirm/update name
          paste0("#", ns("fitbit_file")),                  # Step 3: select file
          paste0("#", ns("load_ts")),                      # Step 4: process file
          paste0("#", ns("cadence_container")),            # Step 5: variable to visualize
          paste0("#", ns("window_container")),             # Step 6: date range
          paste0("#", ns("generate_prompt")),              # Step 7: generate challenge
          paste0("#", ns("step_prompt")),                  # Step 8: copy message
          paste0("#", ns("mess_override_container")),      # Step 9: message override
          paste0("#", ns("steps_factor_container")),       # Step 10: steps factor
          paste0("#", ns("minutes_increment_container")),  # Step 11: minutes increment
          paste0("#", ns("generate_prompt")),              # Step 12: regenerate
          paste0("#", ns("save_state"))                    # Step 13: save state
        ),
        intro = c(
          "<strong>1:</strong> Selecciona un <strong>ID</strong> ya generado o introduce un ID nuevo aqu\u00ED.",
          "<strong>2:</strong> Introduce el <strong>nombre</strong> del participante que aparecer\u00E1 en el mensaje de Whatsapp.",
          "<strong>3:</strong> Selecciona el archivo de Fitbit.",
          "<strong>4:</strong> Haz clic aqu\u00ED, confirma el momento de medici\u00F3n (t), y procesa el archivo. Una vez procesado, ver\u00E1s la gr\u00E1fica debajo.",
          "<strong>5:</strong> Aqu\u00ED podr\u00E1s elegir qu\u00E9 variable visualizar y las fechas a incluir en el an\u00E1lisis.",
          "<strong>6:</strong> Una vez veas el gr\u00E1fico, aqu\u00ED podr\u00E1s seleccionar el rango de fechas (14 d\u00EDas) que se debe incluir en el an\u00E1lisis.",
          "<strong>7:</strong> Pulsa para generar el mensaje de reto y el nuevo objetivo a partir de la gr\u00E1fica.",
          "<strong>8:</strong> Aqu\u00ED ver\u00E1s el mensaje, copia y pega en Whatsapp para enviar.",
          "<strong>9:</strong> Si el mensaje que ha aparecido no es el correcto, podr\u00E1s elegir otro aqu\u00ED.",
          "<strong>10:</strong> Reajusta (si lo necesitas) el factor de multiplicaci\u00F3n para el reto de pasos.",
          "<strong>11:</strong> Reajusta (si lo necesitas) los minutos que se debe incrementar el reto de cadencia.",
          "<strong>12:</strong> Si has hecho alg\u00FAn ajuste, vuelve a pulsar aqu\u00ED para regenerar el mensaje.",
          "<strong>13: \u00A1\u00A1\u00A1IMPORTANTE!!!</strong> no olvides guardar el estado de intervenci\u00F3n (datos a utilizar en el siguiente procesamiento)."
        ),
        position = c(
          "right", "right", "right", "top", # 1:4 
          "right", "left","right","left",   # 5:8 
          "right", "right", "right","right", "top"  # 9:13
        )
      )
      
      # 2. Launch tour
      rintrojs::introjs(
        session, 
        options = list(
          steps = steps,
          'skipLabel' = 'Saltar', 
          'doneLabel' = 'Finalizar', 
          'nextLabel' = 'Siguiente',
          'prevLabel' = 'Anterior',
          'scrollToElement' = TRUE
        )
      )
    })
    
    # open info about decision logic
    observeEvent(input$open_vignette, {
      show_vignette_modal(session,
                          pkg  = "stepinWearable",
                          name = "intervention-decision-logic",
                          title = "Intervention Decision Logic"
      )
    })
    
    
    # ---- Set up directories ----
    
    # Root for all wearable outputs
    get_output_root <- function() WEARABLE_OUTPUT_DIR
    
    # Subfolders
    path_fitbit_root     <- function() file.path(get_output_root(), "output", "fitbit")
    path_stepwatch_root  <- function() file.path(get_output_root(), "output", "stepwatch")
    path_matrix_root     <- function() file.path(get_output_root(), "output", "matrix")
    path_summary_root    <- function() file.path(get_output_root(), "output", "summary")
    
    # Ensure per-ID dir exists (under FITBIT)
    ensure_dirs_for_id <- function(id) {
      dir.create(get_output_root(), recursive = TRUE, showWarnings = FALSE)
      dir.create(path_fitbit_root(), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(path_fitbit_root(), id), recursive = TRUE, showWarnings = FALSE)
      invisible(TRUE)
    }
    
    # Where the intervention .RData/state go for the current ID
    get_processed_dir <- function() {
      req(nzchar(input$participant_id))
      file.path(path_fitbit_root(), input$participant_id)
    }
    
    # ---- VPN status banner if disconnected ----
    access_result <- reactivePoll(
      intervalMillis = 5000,
      session = session,
      checkFunc = function() Sys.time(),
      valueFunc = function() check_vpn_and_server_access()
    )
    
    output$vpn_status_ui <- renderUI({
      result <- access_result()
      if (!result$vpn_connected || !result$server_accessible) {
        div(class = "alert alert-danger", style = "margin-bottom: 10px;",
            icon("times-circle"), result$message)
      } else NULL
    })
    
    # Always use the same local timezone for all date/window operations
    local_tz <- "Europe/Madrid"
    
    # helper to lock/unlock sliders based on t
    lock_sliders_for_t <- function(t) {
      if (is.null(t) || is.na(t)) {
        shinyjs::disable("steps_factor")
        shinyjs::disable("minutes_increment")
        return(invisible())
      }
      if (t == 0) {
        shinyjs::disable("steps_factor")
        shinyjs::disable("minutes_increment")
      } else if (t > 0 && t <= 7) {
        shinyjs::enable("steps_factor")
        shinyjs::disable("minutes_increment")
      } else {
        shinyjs::enable("steps_factor")
        shinyjs::enable("minutes_increment")
      }
    }
    
    # ---- ID registry based on participant_info.rds ----
    participants_df <- reactiveVal(data.frame(id = character(), name = character(), stringsAsFactors = FALSE))
    
    # Ensure per-ID directories exist, persist name if provided, and refresh the in-app ID registry.
    proceed_after_name_check <- function(id, name) {
      dirs <- ensure_dirs_for_id(id)
      if (nzchar(name)) write_participant_name(id, name)
      participants_df(refresh_registry())
    }
    
    # Helper to refresh the in-app ID registry.
    refresh_registry <- function() {
      ids <- scan_existing_ids()
      nms <- vapply(ids, read_participant_name, FUN.VALUE = character(1))
      data.frame(id = ids, name = nms, stringsAsFactors = FALSE)
    }
    
    # Update ID selector if needed
    fill_id_choices <- function(keep_selected = TRUE) {
      df <- participants_df()
      ids <- df$id
      selected <- if (keep_selected) isolate(input$id) else NULL
      updateSelectizeInput(
        session, "participant_id",
        choices = ids, selected = selected, server = TRUE,
        options = list(create = TRUE, persist = TRUE,
                       placeholder = "Pick an existing ID or type a new one")
      )
    }
    
    # Initialize
    observeEvent(TRUE, {
      participants_df(refresh_registry())
      fill_id_choices(keep_selected = FALSE)
    }, once = TRUE)
    
    # Refresh IDs button
    observeEvent(input$refresh_ids, {
      participants_df(refresh_registry())
      fill_id_choices(keep_selected = TRUE)
      showNotification("Participant IDs refreshed.", type = "message")
    })
    
    # Auto-fill name when ID changes
    observeEvent(input$participant_id, {
      req(nzchar(input$participant_id))
      df <- participants_df()
      nm <- df$name[df$id == input$participant_id]
      nm <- if (length(nm) && nzchar(nm[1])) nm[1] else ""
      if (!identical(isolate(input$name), nm)) {
        updateTextInput(session, "name", value = nm)
      }
    }, ignoreInit = TRUE)
    
    # Track pending overwrite confirmation (ID name change)
    rv_overwrite <- reactiveValues(pending = NULL)
    
    # Defer actual write; name is persisted when processing starts or when confirmed
    observeEvent(input$name, {
      # no-op; validation happens before processing
    }, ignoreInit = TRUE)
    
    # Confirm name overwrite prior to processing
    check_name_overwrite <- function() {
      req(nzchar(input$participant_id))
      df <- participants_df()
      old_name <- df$name[df$id == input$participant_id]
      old_name <- if (length(old_name)) old_name[1] else NA_character_
      new_name <- input$name
      if (!is.na(old_name) && nzchar(old_name) && nzchar(new_name) && !identical(old_name, new_name)) {
        rv_overwrite$pending <- list(id = input$participant_id, old = old_name, new = new_name)
        showModal(modalDialog(
          title = "Overwrite participant name?",
          easyClose = FALSE,
          htmltools::HTML(sprintf(
            "ID: <b>%s</b><br>Current name on file: <b>%s</b><br>New name entered: <b>%s</b><br><br>Do you want to overwrite it?",
            htmltools::htmlEscape(input$participant_id),
            htmltools::htmlEscape(old_name),
            htmltools::htmlEscape(new_name)
          )),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_overwrite_name"), "Yes, overwrite", class = "btn btn-danger")
          )
        ))
        return(FALSE)
      }
      TRUE
    }
    
    # Triggers overwrite confirmation
    observeEvent(input$confirm_overwrite_name, {
      req(rv_overwrite$pending)
      removeModal()
      write_participant_name(rv_overwrite$pending$id, rv_overwrite$pending$new)
      showNotification(
        sprintf("Name for %s updated: %s \u2192 %s",
                rv_overwrite$pending$id, rv_overwrite$pending$old, rv_overwrite$pending$new),
        type = "warning"
      )
      proceed_after_name_check(rv_overwrite$pending$id, rv_overwrite$pending$new)
      rv_overwrite$pending <- NULL
    })
    
    # ---- Internal state buckets ----
    rv <- reactiveValues(
      df = NULL,       # confirmed minute-level dataframe (only after saving/overwrite confirm)
      pending = NULL   # temporary info for t-index confirmation modal flow
    )
    
    # Holds the last automatic decision and all data needed to save later
    rv_ctx <- reactiveValues(
      available = FALSE,          # TRUE after Generate Challenge computes a message
      auto_key = NULL,            # template picked by the decision engine (or "nodata3")
      final_key = NULL,           # template actually shown (auto by default, or override)
      glue_env = NULL,            # glue environment to render any template consistently
      add_supportive = FALSE,     # whether supportive paragraph applies (2+ fails)
      add_congrats = FALSE,       # whether congrats paragraph applies (2+ success)
      curk = NULL,                # current 14-day KPIs (or NULL if nodata3)
      start_date = NULL,          # KPI window start (used for display/audit)
      end_date = NULL,            # KPI window end (used for display/audit)
      next_X = NA_real_,          # numeric targets (not formatted)
      next_Y = NA_integer_,
      next_Z = NA_integer_,
      consecutive_fails = 0L,     # streak computed by engine for this cycle
      consecutive_success = 0L,   # streak computed by engine for this cycle
      processed_dir = NULL,       # resolved processed dir for state persistence
      message = NULL              # message sent to participant
    )
    
    # We also track which t was saved for the currently loaded measurement
    rv$current_t <- NULL          # set when the time-series file is saved/overwritten
    rv$current_date_str <- NULL   # e.g., "20250724" (used for audit)
    rv$current_id <- NULL         # participant id for audit
    
    # Daily summary of the *confirmed* minute-series
    dsum_all <- reactive({
      req(steps_data())
      daily_summary(steps_data(), tz = local_tz)
    })
    
    # Render the date-range picker once data is available; default per your rules
    output$window_picker <- renderUI({
      dsum <- dsum_all()
      days <- sort(unique(dsum$date))
      n    <- length(days)
      min_d <- min(days); max_d <- max(days)
      
      # Defaults:
      # - exactly 14 days: use all days
      # - otherwise: default to the *last 14* days (bounded by available range)
      default_start <- if (n == 14) min_d else max(min_d, max_d - 13)
      default_end   <- if (n == 14) max_d else max_d
      
      dateRangeInput(
        ns("window_range"),
        label = "Analysis window (inclusive)",
        start = default_start,
        end   = default_end,
        min   = min_d,
        max   = max_d,
        format = "yyyy-mm-dd",
        separator = " to "
      )
    })
    outputOptions(output, "window_picker", suspendWhenHidden = FALSE)
    
    # Resolve the active window (uses user selection if present, else defaults above)
    active_window <- reactive({
      dsum <- dsum_all()
      dr <- input$window_range
      if (is.null(dr) || any(is.na(dr))) {
        days <- sort(unique(dsum$date))
        n    <- length(days)
        start <- if (n == 14) min(days) else max(min(days), max(days) - 13)
        end   <- if (n == 14) max(days) else max(days)
      } else {
        start <- as.Date(dr[1]); end <- as.Date(dr[2])
      }
      select_windows(dsum, start, end)
    })
    
    # Minute-level series filtered to the selected window (for plots)
    steps_data_window <- reactive({
      req(steps_data())
      d <- steps_data()
      
      # Determine the window dates to use (either user-selected or sensible default)
      dr <- input$window_range
      if (is.null(dr) || any(is.na(dr))) {
        # Default: if exactly 14 days, use them; otherwise last 14 days (in local tz)
        dsum <- daily_summary(d, tz = local_tz)
        days <- sort(unique(dsum$date)); n <- length(days)
        win_start <- if (n == 14) min(days) else max(min(days), max(days) - 13)
        win_end   <- if (n == 14) max(days) else max(days)
      } else {
        win_start <- as.Date(dr[1])
        win_end   <- as.Date(dr[2])
      }
      
      # Build local day bounds (inclusive)
      start_dt <- as.POSIXct(paste0(win_start, " 00:00:00"), tz = local_tz)
      end_dt   <- as.POSIXct(paste0(win_end,   " 23:59:59"), tz = local_tz)
      
      # Compare using the local view of timestamps (instant preserved)
      d %>%
        dplyr::mutate(timestamp = lubridate::with_tz(timestamp, local_tz)) %>%
        dplyr::filter(timestamp >= start_dt & timestamp <= end_dt)
    })
    
    # ---- Button-driven processing: NO auto-processing on upload ----
    
    # Starting folder for file chooser
    start_root <- if (nzchar(FITBIT_DOWNLOAD_DIR)) c("Fitbit Downloaded Files" = FITBIT_DOWNLOAD_DIR) else character(0)
    
    # Set fallack roots
    fallback_roots <- c(
      "Shared Drive (Win)" = DEFAULT_PATH_WINDOWS %||% "",
      "Shared Drive (Mac)" = DEFAULT_PATH_UNIX    %||% "",
      "Wearable Output"    = WEARABLE_OUTPUT_DIR
    )
    
    # Pick the existing root to ensure that the app works
    all_roots <- c(start_root, fallback_roots)
    roots <- all_roots[nzchar(all_roots) & dir.exists(all_roots)]
    
    if (length(roots) == 0) {
      showNotification("No accessible roots. Connect VPN and mount the share.", type = "error")
    }
    
    # Register chooser for the *same id* used in the UI
    shinyFiles::shinyFileChoose(
      input, id = "fitbit_file", session = session,
      roots = roots, filetypes = c("csv", "xlsx")
    )
    
    # Keep the selected path in a reactiveVal
    fitbit_path <- reactiveVal(NULL)
    
    observeEvent(input$fitbit_file, {
      sel <- shinyFiles::parseFilePaths(roots, input$fitbit_file)
      path <- if (nrow(sel)) normalizePath(sel$datapath, winslash = "/", mustWork = FALSE) else NA_character_
      fitbit_path(path)
      output$chosen_path <- renderText(path %||% "")
    })
    
    observeEvent(input$load_ts, {
      req(fitbit_path())
      
      res <- access_result()
      if (!isTRUE(res$vpn_connected) || !isTRUE(res$server_accessible)) {
        showNotification(paste("VPN/server not available.", res$message), type = "error")
        return(invisible())
      }
      
      
      # Before processing, validate ID is selected
      if (!nzchar(input$participant_id)) {
        showNotification("Please enter or select a Participant ID.", type = "error")
        return(invisible())
      }
      if (!check_name_overwrite()) return(invisible())
      proceed_after_name_check(input$participant_id, input$name)
      
      # Show busy spinner while preprocessing
      shinybusy::show_modal_spinner(text = "Processing Fitbit Data...",
                                    spin = "atom",
                                    color = "#21604C")
      
      # Preprocess (do NOT save yet). Any error -> notify and exit.
      ok <- FALSE
      df <- NULL
      dsum <- NULL
      medians <- NULL
      tryCatch({
        df <- preprocess_fitbit(file_path = fitbit_path(), tz = local_tz)
        
        # Build daily summary from the minute-level series
        dsum <- daily_summary(df, tz = local_tz)
        if (nrow(dsum) == 0) stop("No daily data found after preprocessing.")
        # Default window BEFORE user picker exists (14 days if available, else last 14)
        days <- sort(unique(dsum$date))
        n    <- length(days)
        if (n == 14) {
          win_start <- min(days); win_end <- max(days)
          cur <- dsum
        } else {
          win_end   <- max(days)
          win_start <- max(min(days), win_end - 13)
          cur <- dplyr::filter(dsum, date >= win_start & date <= win_end)
        }
        # Temporary medians (will be overwritten on Save State using the user-selected window)
        medians <- list(
          window_start          = win_start,
          window_end            = win_end,
          median_steps_day      = stats::median(cur$steps_day,     na.rm = TRUE),
          median_steps_80plus   = stats::median(cur$steps_80plus,  na.rm = TRUE),
          median_steps_90plus   = stats::median(cur$steps_90plus,  na.rm = TRUE),
          median_steps_100plus  = stats::median(cur$steps_100plus, na.rm = TRUE)
        )
        ok <- TRUE
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        showNotification(paste("Error processing Fitbit:", e$message), type = "error")
      })
      if (!ok) return(invisible())
      
      # 2) Resolve output directory robustly
      outdir <- get_processed_dir()
      if (is.null(outdir) || !nzchar(outdir)) {
        shinybusy::remove_modal_spinner()
        showNotification("Output directory (processed_fitbit_dir) is not available.", type = "error")
        return(invisible())
      }
      if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
      
      # Build filename: id-YYYYMMDD-t{n}
      first_day <- as.Date(min(df$timestamp, na.rm = TRUE))
      date_str  <- format(first_day, "%Y%m%d")
      idp       <- as.character(input$participant_id)
      
      existing  <- list.files(outdir,
                              pattern = paste0("^", idp, "-\\d{8}-t(\\d+)\\.RData$"),
                              full.names = FALSE)
      existing_t <- sort(unique(as.integer(gsub("^.*-t(\\d+)\\.RData$", "\\1", existing))))
      
      # load state of participant if not available
      processed_dir <- get_processed_dir()
      st <- load_participant_state(processed_dir, input$participant_id)
      existing_state_t <- vapply(st$history, function(h) h$t_index, FUN.VALUE = integer(1))
      
      # Suggested t: next after the maximum existing t in state file
      #              this to ensure that all states are saved (crucial for intervention progress)
      suggested  <- if (length(existing_state_t)) max(existing_state_t) + 1L else 0L
      
      # Store pending info for the confirmation modal
      rv$pending <- list(
        df = df, dsum = dsum, medians = medians,
        outdir = outdir, id = idp, date_str = date_str,
        existing_t = existing_t, suggested = suggested, t_sel = NULL
      )
      
      # Close spinner BEFORE showing the confirmation modal
      shinybusy::remove_modal_spinner()
      
      # Ask user to confirm t-index (and show existing indices)
      showModal(modalDialog(
        title = "Confirm t-index",
        easyClose = FALSE, size = "m",
        tagList(
          p(HTML(sprintf(
            "The file will be saved for <b>%s</b> with start date <b>%s</b>.",
            idp, date_str
          ))),
          if (length(existing_t) > 0 && length(existing_state_t) > 0) {
            HTML(sprintf(
              "<p>Existing t-indices for this participant: <b>%s</b></p><p>Existing Status Entries (t-indices): <b>%s</b></p>",
              paste(existing_t, collapse = ", "),
              paste(existing_state_t, collapse = ", ")
            ))
          } else if (length(existing_t) > 0 && length(existing_state_t) == 0) {
            HTML(sprintf(
              "<p>Existing t-indices for this participant: <b>%s</b></p><p>No previous state entries for this participant (please <strong>overwrite t-index 0</strong>)</p>",
              paste(existing_t, collapse = ", ")
            ))
          } else {
            HTML("<p>No previous files exist for this participant.</p>")
          },
          numericInput(ns("t_index_input"),
                       label = "t-index to use",
                       value = suggested, min = 0, step = 1)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_t"), "Save", class = "btn btn-primary")
        )
      ))
    }, ignoreInit = TRUE)
    
    # lock sliders based on t
    observeEvent(input$t_index_input, {
      lock_sliders_for_t(input$t_index_input)
    }, ignoreInit = FALSE)
    
    # When initializing app
    observeEvent(TRUE, {
      shinyjs::disable("steps_factor")
      shinyjs::disable("minutes_increment")
    }, once = TRUE)
    
    session$onFlushed(function() {
      lock_sliders_for_t(shiny::isolate(input$t_index_input))
    }, once = TRUE)
    
    # ---- Confirm t-index -> save or ask to overwrite if exists ----
    observeEvent(input$confirm_t, {
      req(rv$pending)
      t_sel <- as.integer(input$t_index_input)
      rv$pending$t_sel <- t_sel
      
      fname <- file.path(rv$pending$outdir,
                         sprintf("%s-%s-t%d.RData",
                                 rv$pending$id, rv$pending$date_str, t_sel))
      
      if (file.exists(fname)) {
        removeModal()
        showModal(modalDialog(
          title = "t-index exists",
          easyClose = FALSE,
          p(HTML(sprintf(
            "A file with the following name already exists:<br><code>%s</code><br><br>Do you want to <b>overwrite</b> it?",
            basename(fname)
          ))),
          footer = tagList(
            actionButton(ns("cancel_overwrite"), "No, go back", class = "btn btn-default"),
            actionButton(ns("confirm_overwrite"), "Yes, overwrite", class = "btn btn-danger")
          )
        ))
      } else {
        steps_ts <- rv$pending$df
        dsum     <- rv$pending$dsum
        medians  <- rv$pending$medians
        save(steps_ts, dsum, medians, file = fname)
        removeModal()
        showNotification(sprintf("Time series saved: %s", basename(fname)), type = "message")
        # Only after saving do we expose the data to the rest of the module.
        rv$df <- rv$pending$df
        rv$current_t <- rv$pending$t_sel
        rv$current_date_str <- rv$pending$date_str
        rv$current_id <- rv$pending$id
      }
      # Notification about generating targets
      showNotification(
        ui = tagList(
          shiny::icon("bullseye"),
          " Time to set the next target!"
        ),
        type = "warning",
        duration = NULL,
        id = ns("set_target")
      )
    })
    
    # ---- Overwrite confirmation ----
    observeEvent(input$confirm_overwrite, {
      req(rv$pending$t_sel)
      fname <- file.path(rv$pending$outdir,
                         sprintf("%s-%s-t%d.RData",
                                 rv$pending$id, rv$pending$date_str, rv$pending$t_sel))
      steps_ts <- rv$pending$df
      dsum     <- rv$pending$dsum
      medians  <- rv$pending$medians
      save(steps_ts, dsum, medians, file = fname)
      removeModal()
      showNotification(sprintf("Time series overwritten: %s", basename(fname)), type = "warning")
      rv$df <- rv$pending$df
      rv$current_t <- rv$pending$t_sel
      rv$current_date_str <- rv$pending$date_str
      rv$current_id <- rv$pending$id
    })
    
    observeEvent(input$cancel_overwrite, {
      removeModal()
      removeNotification(id = ns("set_target"))  # safe even if not present
    }, ignoreInit = TRUE)
    
    # ----Main reactive source for plotting and messaging ----
    steps_data <- reactive({
      # If user hasn't confirmed/saved, this will block downstream plots/messages.
      req(rv$df)
      rv$df
    })
    
    # ---- Plot (daily totals with cadence filter) ----
    daily_plot_result <- reactive({
      req(steps_data_window(), input$cadence_filter)
      
      # Build daily summary over the ALREADY windowed minute series
      #    Note: daily_summary() should return `steps_day` and the minute counts
      #    `steps_80plus`, `steps_90plus`, `steps_100plus`.
      dsum <- daily_summary(steps_data_window())
      
      # Select the series to plot and the y-axis label
      if (identical(input$cadence_filter, "Total steps")) {
        df_bar <- dplyr::transmute(dsum, date, value = steps_day, valid_day = valid_day)
        ylab <- "Steps per day"
        colorbar_title <- "Steps"
      } else {
        thr <- switch(
          input$cadence_filter,
          "Steps \u2265 80"  = 80L,
          "Steps \u2265 90"  = 90L,
          "Steps \u2265 100" = 100L
        )
        col <- paste0("steps_", thr, "plus")
        df_bar <- tibble::tibble(date = dsum$date, value = dsum[[col]], valid_day = dsum$valid_day)
        ylab <- paste0("Minutes/day at cadence \u2265 ", thr)
        colorbar_title <- "Minutes"
      }
      
      valid_df   <- dplyr::filter(df_bar,  valid_day %in% TRUE)
      invalid_df <- dplyr::filter(df_bar, !valid_day %in% TRUE)
      
      # Continuous gradient: low values red, mid amber, high green
      #    Plotly treats numeric `color` as continuous and builds a colorscale.
      p <- plotly::plot_ly()
      
      # valid days: keep your gradient
      if (nrow(valid_df)) {
        p <- p |>
          plotly::add_bars(
            data = valid_df,
            x = ~date, y = ~value,
            color = ~value,
            colors = c("#ef4444", "#f59e0b", "#16a34a"),
            hovertemplate = paste0("%{x}<br>", ylab, ": %{y}<extra></extra>"),
            showlegend = FALSE
          )
      }
      
      # invalid days: fixed gray
      if (nrow(invalid_df)) {
        p <- p |>
          plotly::add_bars(
            data = invalid_df,
            x = ~date, y = ~value,
            marker = list(color = "#9CA3AF"),
            hovertemplate = paste0("%{x}<br>", ylab, ": %{y}<br><b>Invalid day</b><extra></extra>"),
            showlegend = FALSE
          )
      }
      
      p <- p |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = ylab),
          bargap = 0.2,
          coloraxis = list(colorbar = list(title = colorbar_title))
        )
      
      list(plot = p, data = df_bar)  
    })
    
    output$steps_day <- renderPlotly({
      req(daily_plot_result())
      
      # plot and data
      p <- daily_plot_result()$plot
      df_bar <- daily_plot_result()$data
      df_bar$date <- as.Date(df_bar$date)
      
      xr <- range(df_bar$date, na.rm = TRUE)
      x_min <- as.POSIXct(xr[1]) - 25*3600 
      x_max <- as.POSIXct(xr[2]) + 25*3600
      
      # median over valid days only
      med_val <- stats::median(df_bar$value[df_bar$valid_day %in% TRUE], na.rm = TRUE)
      
      shapes <- list()
      annotations <- list()
      
      if (is.finite(med_val)) {
        shapes <- c(shapes, list(
          list(type="line", xref="paper", x0=0, x1=1, yref="y", y0=med_val, y1=med_val,
               line=list(dash="dash", width=2))
        ))
        annotations <- c(annotations, list(
          list(xref="paper", x=1.02, yref="y", y=med_val,
               text=paste0("Med: ", format(round(med_val))),
               xanchor="left", showarrow=FALSE,
               font=list(size=12, color="#111111"),
               bgcolor="rgba(255,255,255,0.95)", bordercolor="#D1D5DB", borderwidth=1, borderpad=4)
        ))
      }
      
      p <- daily_plot_result()$plot %>%
        plotly::layout(
          xaxis = list(type="date", range=c(x_min, x_max), tickformat="%Y-%m-%d", tickangle=-45, title="Date"),
          shapes = shapes, annotations = annotations
        )
      
      style_plotly(p)
    })
    
    
    # function to check if X and Y are met in server to redefine slider input default value
    rv_defaults <- reactiveValues(rec_sf = NULL, rec_mi = NULL)
    success_flags <- function(st, curk, t) {
      has_prev_X <- !is.null(st$last_X) && is.finite(st$last_X) && t > 0
      has_prev_Y <- !is.null(st$last_Y) && is.finite(st$last_Y) && st$last_Y > 0 && t > 6
      has_prev_Z <- !is.null(st$last_Z) && is.finite(st$last_Z) && t > 6
      
      steps_ok <- steps_met(curk$med_steps_day, st$last_X, 1)
      
      cur_minutes_at_prevZ <- dplyr::case_when(
        has_prev_Z && st$last_Z == 100 ~ curk$med_steps_100plus,
        has_prev_Z && st$last_Z ==  90 ~ curk$med_steps_90plus,
        has_prev_Z && st$last_Z ==  80 ~ curk$med_steps_80plus,
        TRUE ~ NA_real_
      )
      mins_ok <- if (has_prev_Y && !is.na(cur_minutes_at_prevZ))
        cur_minutes_at_prevZ >= st$last_Y else NA
  
      list(steps_ok = steps_ok, mins_ok = mins_ok)
    }
    # ---- Message Decision Engine ----
    observeEvent(input$generate_prompt, {
      # load messages
      msgs <- load_messages()
      message_templates <- msgs$templates
      tips_pasos        <- msgs$tips$pasos
      tips_intensidad   <- msgs$tips$intensidad
      tips_mixto        <- msgs$tips$mixto
      
      # data
      df <- steps_data()
      req(nrow(df) > 0)
      
      # Aggregate to daily
      dsum <- daily_summary(df, tz = local_tz)
      wins <- active_window()
      processed_dir <- get_processed_dir()
      st <- load_participant_state(processed_dir, input$participant_id)
      
      # Restrict to the selected window and keep only valid days
      cur_all    <- wins$current
      cur_valid  <- dplyr::filter(cur_all, valid_day %in% TRUE)
      
      # Branch 1: "No data (3 days)" -> always show automatic nodata message
      if (no_data_last_3_days(dsum, end_date = wins$end_date)) {
        # ---- AUTO: NODATA ----
        # Build a minimal env (we also reuse previous targets, if available, for overrides)
        glue_env <- list(
          nombre = input$name %||% "Nombre",
          X = format(round(st$last_X %||% NA_real_), big.mark = ".", decimal.mark = ","), # may be NA
          Y = st$last_Y %||% 0L,
          Z = st$last_Z %||% NA_integer_,
          tips_pasos = tips_pasos,
          tips_intensidad = tips_intensidad,
          tips_mixto = tips_mixto
        )
        
        # Update override context
        rv_ctx$available      <- TRUE
        rv_ctx$glue_env       <- glue_env
        rv_ctx$auto_key       <- "nodata3"
        rv_ctx$final_key      <- "nodata3"
        rv_ctx$add_supportive <- FALSE
        rv_ctx$add_congrats   <- FALSE
        rv_ctx$processed_dir  <- processed_dir
        rv_ctx$start_date     <- wins$start_date
        rv_ctx$end_date       <- wins$end_date
        
        # Render the automatic message first (always show auto)
        auto_txt <- do.call(glue::glue, c(list(message_templates$nodata3), glue_env))
        output$step_prompt <- renderText(auto_txt)
        updateSelectInput(session, "override_select", selected = "auto")
        return(invisible())
      }
      
      # KPIs on the *selected* current and previous windows
      curk <- kpis(cur_all)
      prevk <- if (length(st$history) > 0) st$history[[input$t_index_input]]$kpis else NULL
      prevstepsfactor <- if (length(st$history) > 0) st$history[[input$t_index_input]]$steps_factor else NULL
      prevminutesinc <- if (length(st$history) > 0) st$history[[input$t_index_input]]$minutes_inc else NULL
      
      # Researcher-adjusted factors
      # Initial recommended values based on ok flags
      ok <- success_flags(st, curk, input$t_index_input)
      rec_sf <- if (isTRUE(ok$steps_ok)) 1.05 else 1.00
      rec_mi <- if (isTRUE(ok$mins_ok))     5L else 0L
      
      # Did the user override the recommended values?
      is_override_sf <- !is.null(rv_defaults$rec_sf) && !isTRUE(all.equal(input$steps_factor,     rv_defaults$rec_sf))
      is_override_mi <- !is.null(rv_defaults$rec_mi) && !isTRUE(all.equal(input$minutes_increment, rv_defaults$rec_mi))
      
      # Values to apply
      apply_sf <- if (is_override_sf) input$steps_factor     else rec_sf
      apply_mi <- if (is_override_mi) input$minutes_increment else rec_mi
      
      # decide message based on n valid days
      n_valid <- nrow(cur_valid)
      if (is.na(n_valid) || n_valid < 7) {
        # ---- FORCE AUTO: NODATA ----
        msgs <- load_messages()
        message_templates <- msgs$templates
        tips_pasos        <- msgs$tips$pasos
        tips_intensidad   <- msgs$tips$intensidad
        tips_mixto        <- msgs$tips$mixto
        
        glue_env <- list(
          nombre = input$name %||% "Nombre",
          X = format(round(st$last_X %||% NA_real_), big.mark = ".", decimal.mark = ","),
          Y = st$last_Y %||% 0L,
          Z = st$last_Z %||% NA_integer_,
          tips_pasos = tips_pasos,
          tips_intensidad = tips_intensidad,
          tips_mixto = tips_mixto
        )
        
        rv_ctx$available      <- TRUE
        rv_ctx$glue_env       <- glue_env
        rv_ctx$auto_key       <- "nodata3"
        rv_ctx$final_key      <- "nodata3"
        rv_ctx$add_supportive <- FALSE
        rv_ctx$add_congrats   <- FALSE
        rv_ctx$processed_dir  <- processed_dir
        rv_ctx$start_date     <- wins$start_date
        rv_ctx$end_date       <- wins$end_date
        rv_ctx$curk           <- NULL
        rv_ctx$next_X         <- st$last_X
        rv_ctx$next_Y         <- st$last_Y
        rv_ctx$next_Z         <- st$last_Z
        rv_ctx$message        <- do.call(glue::glue, c(list(message_templates$nodata3), glue_env))
        
        output$step_prompt <- renderText(rv_ctx$message)
        updateSelectInput(session, "override_select", selected = "auto")
        
        # keep/save reminder
        removeNotification(ns("set_target"))
        showNotification(
          ui = tagList(shiny::icon("info-circle"),
                       sprintf("Only %d valid days in the selected window (need ≥ 7). Sent 'No data (3 days)'.", n_valid)),
          type = "warning", duration = 8
        )
        return(invisible())
      }
      
      # Generate message based on decided steps_factor and minutes increment
      res <- decide_message(
        state = st, cur_k = curk, prev_k = prevk, nombre = input$name,
        steps_factor = apply_sf, minutes_inc = apply_mi, t = input$t_index_input
      )
      
      # Sync sliders to decided values
      shiny::freezeReactiveValue(input, "steps_factor")
      updateSliderInput(session, "steps_factor", value = apply_sf)
      shiny::freezeReactiveValue(input, "minutes_increment")
      updateSliderInput(session, "minutes_increment", value = apply_mi)
      
      # Save current values
      rv_defaults$rec_sf <- rec_sf
      rv_defaults$rec_mi <- rec_mi
      
      # Prepare glue env used by any template (auto or override)
      glue_env <- list(
        nombre = input$name %||% "**Nombre**",
        X = format(round(res$next_X), big.mark = ".", decimal.mark = ","),
        Y = res$next_Y,
        Z = res$next_Z,
        tips_pasos = tips_pasos,
        tips_intensidad = tips_intensidad,
        tips_mixto = tips_mixto
      )
      
      # Fill context - but DO NOT write the state yet
      rv_ctx$available           <- TRUE
      rv_ctx$auto_key            <- res$key
      rv_ctx$final_key           <- res$key
      rv_ctx$glue_env            <- glue_env
      rv_ctx$add_supportive      <- res$consecutive_fails >= 2L
      rv_ctx$add_congrats        <- res$consecutive_success >= 2L
      rv_ctx$curk                <- curk
      rv_ctx$start_date          <- wins$start_date
      rv_ctx$end_date            <- wins$end_date
      rv_ctx$next_X              <- res$next_X
      rv_ctx$next_Y              <- res$next_Y
      rv_ctx$next_Z              <- res$next_Z
      rv_ctx$consecutive_fails   <- res$consecutive_fails
      rv_ctx$consecutive_success <- res$consecutive_success
      rv_ctx$processed_dir       <- processed_dir
      rv_ctx$message             <- res$text
      rv_ctx$steps_met           <- res$steps_met
      rv_ctx$cadence_met         <- res$cadence_met
      
      # ALWAYS show the automatic message first
      output$step_prompt <- renderText(res$text)
      updateSelectInput(session, "override_select", selected = "auto")
      
      # remove notification about setting targets
      removeNotification(ns("set_target"))     # <- hide the reminder
      
      # Notification about saving state
      showNotification(
        ui = tagList(
          shiny::icon("save"),
          " Don't forget to Save Intervention State"
        ),
        type = "warning",
        duration = NULL,
        id = ns("save_reminder")
      )
    })
    
    observeEvent(input$override_select, {
      req(input$override_select)
      
      # Only allow override after an automatic message was generated
      if (!isTRUE(rv_ctx$available)) {
        if (input$override_select != "auto") {
          showNotification("Generate the automatic message first.", type = "warning")
          updateSelectInput(session, "override_select", selected = "auto")
        }
        return(invisible())
      }
      
      # "Auto" -> restore the automatic choice
      if (input$override_select == "auto") {
        rv_ctx$final_key <- rv_ctx$auto_key
        txt <- render_template_with_env(rv_ctx$auto_key, rv_ctx$glue_env, 
                                        rv_ctx$add_supportive, rv_ctx$add_congrats)
        output$step_prompt <- renderText(txt)
        return(invisible())
      }
      
      # Manual override: change the template used for display (targets stay the same)
      rv_ctx$final_key <- input$override_select
      txt <- render_template_with_env(rv_ctx$final_key, 
                                      rv_ctx$glue_env, 
                                      rv_ctx$add_supportive,
                                      rv_ctx$add_congrats)
      output$step_prompt <- renderText(txt)
      
    })
    
    # Save state for this participant.
    observeEvent(input$save_state, {
      # Requisites: (1) a computed message; (2) a saved measurement with t-index
      if (!isTRUE(rv_ctx$available)) {
        showNotification("Please generate the automatic message first.", type = "error")
        return(invisible())
      }
      if (is.null(rv$current_t)) {
        showNotification("Please load & confirm the measurement first ('t' is unknown).", type = "error")
        return(invisible())
      }
      
      processed_dir <- rv_ctx$processed_dir %||% get_processed_dir()
      if (is.null(processed_dir) || !nzchar(processed_dir)) {
        showNotification("Processed directory is not available.", type = "error")
        return(invisible())
      }
      st <- load_participant_state(processed_dir, input$participant_id)
      
      # --- Use the SELECTED WINDOW for calculations ---
      wins <- active_window()
      cur  <- dplyr::filter(wins$current, valid_day %in% TRUE)
      if (!nrow(cur)) {
        showNotification("Selected window has no data to save.", type = "error")
        return(invisible())
      }
      win_start <- min(cur$date); win_end <- max(cur$date)
      
      # Build local day bounds (inclusive)
      start_dt <- as.POSIXct(paste0(win_start, " 00:00:00"), tz = local_tz)
      end_dt   <- as.POSIXct(paste0(win_end,   " 23:59:59"), tz = local_tz)
      
      # Minute-level filtered to the selected window, comparing in local tz
      steps_ts <- rv$df %>%
        dplyr::mutate(timestamp = lubridate::with_tz(timestamp, local_tz)) %>%
        dplyr::filter(timestamp >= start_dt & timestamp <= end_dt)
      
      # (optional) ensure saved timestamps carry the local tz explicitly
      steps_ts$timestamp <- lubridate::force_tz(steps_ts$timestamp, tzone = local_tz)
      
      # Daily summary for the selected window only
      dsum <- daily_summary(steps_ts, tz = local_tz)
      
      # Medians for the selected window
      medians <- list(
        window_start         = win_start,
        window_end           = win_end,
        median_steps_day     = stats::median(cur$steps_day,     na.rm = TRUE),
        median_steps_80plus  = stats::median(cur$steps_80plus,  na.rm = TRUE),
        median_steps_90plus  = stats::median(cur$steps_90plus,  na.rm = TRUE),
        median_steps_100plus = stats::median(cur$steps_100plus, na.rm = TRUE)
      )
      
      # Persist the windowed data under the canonical filename (overwrite if exists)
      fname <- file.path(
        processed_dir,
        sprintf("%s-%s-t%d.RData", rv$current_id, rv$current_date_str, rv$current_t)
      )
      save(steps_ts, dsum, medians, file = fname)
      
      # Upsert by t_index: replace if exists, otherwise append
      entry <- list(
        t_index           = rv$current_t,
        date_str          = rv$current_date_str,
        start_date        = wins$start_date,
        end_date          = wins$end_date,
        kpis              = kpis(cur),
        steps_factor      = input$steps_factor,
        minutes_inc       = input$minutes_increment,
        target_steps      = rv_ctx$next_X,
        target_minutes    = rv_ctx$next_Y,
        target_cadence    = rv_ctx$next_Z,
        auto_message_key  = rv_ctx$auto_key,
        final_message_key = rv_ctx$final_key,
        manual_override   = !identical(rv_ctx$final_key, rv_ctx$auto_key),
        message           = rv_ctx$message,
        steps_met         = rv_ctx$steps_met,
        cadence_met       = rv_ctx$cadence_met
      )
      
      replaced <- FALSE
      if (length(st$history)) {
        for (i in seq_along(st$history)) {
          ti <- tryCatch(st$history[[i]]$t_index, error = function(e) NULL)
          if (!is.null(ti) && identical(ti, rv$current_t)) {
            st$history[[i]] <- entry
            replaced <- TRUE
            break
          }
        }
      }
      if (!replaced) st$history <- append(st$history, list(entry))
      
      # Update last targets only for "normal" decisions (rv_ctx$curk != NULL)
      if (!is.null(rv_ctx$curk)) {
        st$last_X <- rv_ctx$next_X
        st$last_Y <- rv_ctx$next_Y
        st$last_Z <- rv_ctx$next_Z
        st$last_steps_factor <- input$steps_factor
        st$last_minutes_inc <- input$minutes_increment
        st$consecutive_fails <- rv_ctx$consecutive_fails
        st$consecutive_success <- rv_ctx$consecutive_success
        st$total_steps_int <- if (length(st$history)) {
          sum(sapply(st$history, function(x) x$kpis$total_steps_alldays)) 
        } else {
          kpis(cur)$total_steps_alldays
        }
        # TARGETS STEPS
        st$n_targets_steps = if (!is.na(st$last_X) & rv$current_t > 0) st$n_targets_steps + 1L else st$n_targets_steps
        st$n_targets_steps_t1 = if (!is.na(st$last_X) & rv$current_t %in% 1:6) st$n_targets_steps_t1 + 1L else st$n_targets_steps_t1
        st$n_targets_steps_t2 = if(!is.na(st$last_X) & rv$current_t %in% 7:12) st$n_targets_steps_t2 + 1L else st$n_targets_steps_t2
        st$n_targets_steps_t3 = if(!is.na(st$last_X) & rv$current_t >= 13) st$n_targets_steps_t3 + 1L else st$n_targets_steps_t3
        st$n_targets_steps_met = if (length(st$history)) {
          as.integer(sum(sapply(st$history, function(x) x$steps_met), na.rm = T))
        } else {
          0L
        }
        st$n_targets_steps_met_t1 = if (length(st$history)) {
          as.integer(sum(sapply(st$history[1:(min(6, length(st$history)))], function(x) x$steps_met), na.rm = T))
        } else {
          0L
        }
        st$n_targets_steps_met_t2 = if (length(st$history) > 6) {
          as.integer(sum(sapply(st$history[7:(min(12, length(st$history)))], function(x) x$steps_met), na.rm = T))
        } else {
          0L
        }
        st$n_targets_steps_met_t3 = if (length(st$history) > 12) {
          as.integer(sum(sapply(st$history[13:length(st$history)], function(x) x$steps_met), na.rm = T))
        } else {
          0L
        }
        # TARGETS CADENCE
        st$n_targets_cadence = if (!is.na(st$last_X) & kpis(cur)$n_days >= 7 & rv$current_t > 6) st$n_targets_cadence + 1L else st$n_targets_cadence
        # st$n_targets_cadence_t1 = if (!is.na(st$last_X) & rv$current_t %in% 1:6) st$n_targets_cadence_t1 + 1L else st$n_targets_cadence_t1
        st$n_targets_cadence_t2 = if(!is.na(st$last_X) & rv$current_t %in% 7:12) st$n_targets_cadence_t2 + 1L else st$n_targets_cadence_t2
        st$n_targets_cadence_t3 = if(!is.na(st$last_X) & rv$current_t >= 13) st$n_targets_cadence_t3 + 1L else st$n_targets_cadence_t3
        st$n_targets_cadence_met = if (length(st$history)) {
          as.integer(sum(sapply(st$history, function(x) x$cadence_met), na.rm = T))
        } else {
          0L
        }
        # st$n_targets_cadence_met_t1 = if (length(st$history) > 1) {
        #   as.integer(sum(sapply(st$history[1:(min(6, length(st$history)))], function(x) x$cadence_met), na.rm = T))
        # } else {
        #   0L
        # }
        st$n_targets_cadence_met_t2 = if (length(st$history) > 6) {
          as.integer(sum(sapply(st$history[7:(min(12, length(st$history)))], function(x) x$cadence_met), na.rm = T))
        } else {
          0L
        }
        st$n_targets_cadence_met_t3 = if (length(st$history) > 12) {
          as.integer(sum(sapply(st$history[13:length(st$history)], function(x) x$cadence_met), na.rm = T))
        } else {
          0L
        }
      }
      save_participant_state(st, processed_dir, input$participant_id)
      removeNotification(ns("save_reminder"))     # <- hide the reminder
      showNotification(
        sprintf("Saved window %s \u2192 %s for t%d.", win_start, win_end, rv$current_t),
        type = "message"
      )
      
      # ---- Build and show the team summary table (after saving state) ----
      steps_total <- fmt_num(st$total_steps_int)
      
      steps_total_targets     <- fmt_int(st$n_targets_steps)
      steps_t1_targets        <- fmt_int(st$n_targets_steps_t1)
      steps_t2_targets        <- fmt_int(st$n_targets_steps_t2)
      steps_t3_targets        <- fmt_int(st$n_targets_steps_t3)
      
      steps_total_met         <- fmt_int(st$n_targets_steps_met)
      steps_t1_met            <- fmt_int(st$n_targets_steps_met_t1)
      steps_t2_met            <- fmt_int(st$n_targets_steps_met_t2)
      steps_t3_met            <- fmt_int(st$n_targets_steps_met_t3)
      
      cad_total_targets       <- fmt_int(st$n_targets_cadence)
      cad_t1_targets          <- fmt_int(st$n_targets_cadence_t1)
      cad_t2_targets          <- fmt_int(st$n_targets_cadence_t2)
      cad_t3_targets          <- fmt_int(st$n_targets_cadence_t3)
      
      cad_total_met           <- fmt_int(st$n_targets_cadence_met)
      # cad_t1_met              <- fmt_int(st$n_targets_cadence_met_t1)
      cad_t2_met              <- fmt_int(st$n_targets_cadence_met_t2)
      cad_t3_met              <- fmt_int(st$n_targets_cadence_met_t3)
      
      summary_df <- data.frame(
        Category = c(
          "Steps", "Steps", "Steps", "Steps", "Steps",
          "Cadence", "Cadence", "Cadence"#, "Cadence"
        ),
        Metric = c(
          "Total steps (cumulative)", 
          "Targets — total",
          "Targets — T1 (t 1–6)",
          "Targets — T2 (t 7–12)",
          "Targets — T3 (t > 12)",
          "Targets — total",
          # "Targets — T1 (t 1–6)",
          "Targets — T2 (7–12)",
          "Targets — T3 (t > 12)"
        ),
        Value = c(
          fmt_big(steps_total),
          sprintf("%d/%d", steps_total_met, steps_total_targets),
          sprintf("%d/%d", steps_t1_met, steps_t1_targets),
          sprintf("%d/%d", steps_t2_met, steps_t2_targets),
          sprintf("%d/%d", steps_t3_met, steps_t3_targets),
          sprintf("%d/%d", cad_total_met, cad_total_targets),
          # sprintf("%d/%d", cad_t1_met, cad_t1_targets),
          sprintf("%d/%d", cad_t2_met, cad_t2_targets),
          sprintf("%d/%d", cad_t3_met, cad_t3_targets)
        ),
        check.names = FALSE
      )
      
      output$team_summary_table <- renderTable({
        summary_df
      }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")
      
      # Quick toast for the team
      showModal(modalDialog(
        title = "Intervention summary",
        easyClose = TRUE,
        footer = modalButton("OK"),
        htmltools::HTML(sprintf(
          paste0(
            "<p><strong>Cumulative steps:</strong> %s</p>",
            "<p><strong>Steps targets met:</strong> %d / %d</p>",
            "<p><strong>Cadence targets met:</strong> %d / %d</p>"
          ),
          fmt_big(steps_total),
          steps_total_met, steps_total_targets,
          cad_total_met,  cad_total_targets
        ))
      ))
    })
    
    output$team_summary_table <- renderTable({
      # blank until first save
      NULL
    }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")
    
  })
}
