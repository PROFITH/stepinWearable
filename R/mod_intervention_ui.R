#' Intervention UI
#'
#' @description User interface for the Intervention module, dedicated to the Fitbit
#' data processing, visualization, and personalized challenge message generation workflow.
#'
#' @param id Module namespace ID.
#'
#' @section UI Structure and Features:
#' The UI is organized into three main bslib cards:
#' \itemize{
#'   \item \strong{1. Participant & File:} Handles user input for \code{Participant ID} (via \code{selectizeInput} with refresh capabilities for existing IDs) and file selection via \code{shinyFilesButton}. The \code{load_ts} action button triggers the data processing pipeline.
#'   \item \strong{2. Daily Step Totals (Visualization):} Displays the interactive bar plot (\code{plotlyOutput}) of daily steps/cadence minutes. Includes controls for selecting the \code{cadence_filter} (e.g., "Steps \u2265 90") and the \code{window_picker} UI for defining the 14-day analysis period.
#'   \item \strong{3. Challenge Message (Decision Engine Controls):} Contains all inputs necessary to customize and finalize the intervention message.
#' }
#'
#' @section Challenge Message Controls:
#' This section includes:
#' \itemize{
#'   \item \code{generate_prompt}: Button to run the decision engine and generate the automatic message.
#'   \item \code{steps_factor}: Slider to adjust the multiplication factor for step targets (X).
#'   \item \code{minutes_increment}: Slider to adjust the minute increment for intensity targets (Y).
#'   \item \code{override_select}: Dropdown allowing manual selection of any message template, overriding the automatic decision.
#'   \item \code{step_prompt}: \code{verbatimTextOutput} displaying the final, formatted message ready for copying.
#'   \item \code{save_state}: Crucial button to persist the current intervention targets (X, Y, Z) and decision metadata to the participant's state file.
#' }
#'
#' @importFrom shiny NS tagList fluidRow column br actionButton selectInput verbatimTextOutput uiOutput sliderInput
#' @importFrom plotly plotlyOutput
#' @importFrom shinyFiles shinyFilesButton
#' @import bslib
#' @import fontawesome
#' @noRd
mod_intervention_ui <- function(id) {
  ns <- NS(id)
  
  spacer <- function(px = 8) tags$div(style = paste0("height:", px, "px;"))
  
  tags$div(
    id = ns("wrap"),
    # vpn status (conditional, only if not connected)
    uiOutput(ns("vpn_status_ui")),
    
    # Tutorial
    shiny::br(),
    shiny::actionButton(
      inputId = ns("start_tutorial_btn"),
      label = "Start Tutorial",
      icon = shiny::icon("circle-question"),
      class = "btn-info btn-sm mb-3"
    ),
    
    # Interface
    fluidRow(
      shiny::column(
        width = 4,
        bslib::card(class = "compact-card",
                    bslib::card_header("Participant & File"),
                    bslib::card_body(
                      
                      # === START MAIN ROW: Holds ID/Name on left, File/Process on right ===
                      fluidRow(
                        class = "g-compact", 
                        # INNER ROW 1: ID Input + Refresh Button
                        fluidRow(
                          class = "g-compact align-items-end",
                          shiny::column(
                            width = 9, # ID Input
                            tags$div(
                              id = ns("id_container"),
                              selectizeInput(ns("participant_id"), "Participant ID", choices = NULL,
                                             options = list(create = TRUE, persist = TRUE,
                                                            placeholder = "Pick an existing ID or type a new one")
                              )
                            )
                          ),
                          shiny::column(
                            width = 3, # Refresh Button
                            shiny::actionButton(ns("refresh_ids"), "", 
                                         icon = icon("sync"),
                                         class = "btn btn-light")
                          )
                        ),
                        
                        # Vertical Spacer Override: Bring Name input closer to ID input
                        textInput(ns("name"), "Participant name",
                                  placeholder = "Name that will appear in WhatsApp messages"),
                        # File Chooser (Upper Row)
                        fluidRow(
                          shiny::column(
                            width = 9,
                            shinyFiles::shinyFilesButton(
                              id = ns("fitbit_file"), 
                              label = "Fitbit file (.csv/.xlsx)", 
                              title = "Fitbit file (.csv/.xlsx)",
                              multiple = FALSE
                            ),
                            tags$div(style = "margin-top: 0px; text-align: right;",
                                     shiny::verbatimTextOutput(ns("chosen_path"))
                            )
                          ),
                          shiny::column(
                            width = 3,
                            shiny::actionButton(ns("load_ts"), "",
                                         icon = icon("play"),
                                         class = "btn btn-success")
                          )
                        )
                        
                      ) 
                    ),
        )
      ),
      shiny::column(
        width = 8,
        bslib::card(
          class = "compact-card",
          bslib::card_header("Daily Step Totals"),
          bslib::card_body(
            fluidRow(
              shiny::column(
                width = 6,
                tags$div(
                  id = ns("cadence_container"),
                  shiny::selectInput(ns("cadence_filter"), "Select cadence",
                                     choices = c("Total steps","Steps \u2265 80",
                                                 "Steps \u2265 90","Steps \u2265 100",
                                                 "Steps \u2265 110", "Steps \u2265 120"))
                )
              ),
              shiny::column(
                width = 6,
                tags$div(
                  id = ns("window_container"),
                  uiOutput(ns("window_picker"))
                )
              )
            ),
            shinycssloaders::withSpinner(
              plotlyOutput(ns("steps_day"), height = "380px"), 
              type = 3, color = "#21604C", color.background = "#F8F8F8"
            )
          )
        )
      ),
    ),
    
    # Message generation interface
    fluidRow(
      bslib::card(
        class = "compact-card",
        bslib::card_header("Challenge Message"),
        bslib::card_body(
          fluidRow(
            # options
            shiny::column(
              width = 4,
              fluidRow(
                shiny::column(
                  width = 10,
                  tags$div(
                    style = "margin-top: 10px;",
                    shiny::actionButton(
                      inputId = ns("generate_prompt"), 
                      label = "Set Targets & Generate Message",
                      icon = icon("whatsapp", lib = "font-awesome"),
                      class = "btn btn-primary"
                    )
                  )
                ),
                shiny::column(
                  width = 2,
                  tags$div(
                    style = "margin-top: 10px;",
                    shiny::actionButton(
                      inputId = ns("open_vignette"), 
                      label = "",
                      icon = shiny::icon("circle-info"),
                      class = "btn btn-light"
                    )
                  )
                ),
              ),
              br(),
              # === SLIDER X: multiplication factor for steps target ===
              fluidRow(
                tags$div(
                  id = ns("steps_factor_container"),
                  shiny::sliderInput(
                    ns("steps_factor"), 
                    "Factor de Aumento (X)", 
                    min = 0.30, max = 1.10, value = 1.05, step = 0.01
                  )
                )
              ),
              
              # === SLIDER Y: Minutes increment ===
              fluidRow(
                tags$div(
                  id = ns("minutes_increment_container"),
                  shiny::sliderInput(
                    ns("minutes_increment"), 
                    "Incremento de Minutos (Y)", 
                    min = -60, max = 60, value = 5, step = 1
                  )
                )
              ),
              # === SLIDER Z: Cadence threshold ===
              fluidRow(
                tags$div(
                  id = ns("cadence_threshold_container"),
                  shiny::sliderInput(
                    ns("cadence_threshold"),
                    "Umbral de Cadencia (Z)",
                    min = 80, max = 120, value = 100, step = 10
                  )
                )
              ),
              # Save state button
              fluidRow(
                shiny::actionButton(ns("save_state"), "Save Intervention State", 
                                    icon = icon("save"),
                                    class = "btn btn-success")
              )
            ),
            # message preview
            shiny::column(
              width = 8,
              fluidRow(
                shiny::column(
                  width = 6,
                  tags$label("Preview")
                ), 
                shiny::column(
                  width = 6,
                  tags$div(
                    id = ns("mess_override_container"),
                    shiny::selectInput(
                      ns("override_select"), 
                      label = "Message override",
                      choices = c("Auto (recommended)" = "auto",
                                  "MENSAJE 0" = "msg0",
                                  "PASOS 1" = "pasos1",
                                  "PASOS 2" = "pasos2",
                                  "AMBOS 1" = "ambos1",
                                  "AMBOS 2" = "ambos2",
                                  "AMBOS 3" = "ambos3",
                                  "AMBOS 4" = "ambos4",
                                  "AMBOS 5" = "ambos5",
                                  "AMBOS 6" = "ambos6", 
                                  "AMBOS 7" = "ambos7",
                                  "AMBOS 8" = "ambos8",
                                  "No data (3 days)" = "nodata3"),
                      selected = "auto"
                    )
                  )
                )
              ),
              tags$div(
                style = "white-space: pre-wrap; background:#F8FAFC; border:1px solid #E5E7EB; border-radius:.65rem; padding:10px;",
                shiny::verbatimTextOutput(ns("step_prompt"), placeholder = TRUE)
              )
            )
          )
        )       
      )
    ),
    
    # --- Team-facing summary (auto-updates after Save) ---
    fluidRow(
      bslib::card(
        class = "compact-card",
        bslib::card_header("Intervention Summary"),
        bslib::card_body(
          tags$p(class = "text-muted", "Updated after each Save Intervention State"),
          tableOutput(ns("team_summary_table"))
        )
      )
    )
  )
}