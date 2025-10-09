#' Application Server Logic
#'
#' @description Defines the primary server logic for the entire `stepinWearable`
#' application. Its main responsibility is to call the server functions for all
#' active modules and manage any application-wide state or global observers.
#'
#' @details
#' Currently, the server runs the \code{\link[=mod_intervention_server]{Intervention Module}}
#' (\code{mod_intervention_server}), which contains the core logic for data processing,
#' visualization, challenge generation, and state persistence.
#'
#' In future releases, this function will be updated to include the server logic
#' for the Assessment Module.
#'
#' @param input Internal parameter for the Shiny server.
#' @param output Internal parameter for the Shiny server.
#' @param session Internal parameter for the Shiny server.
#'
#' @noRd
app_server <- function(input, output, session) {
  
  # 1. Intervention Module Server
  # This module contains all the core functionality for file loading,
  # t-index confirmation, plotting, and challenge generation.
  mod_intervention_server("intervention_1")
  
  # 2. Assessment Module Server (Future)
  # When the Assessment module is implemented, its server function will be called here.
  # # mod_assessment_server("assessment_1")
}