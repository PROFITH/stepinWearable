#' Application User Interface
#'
#' @description Defines the entire user interface (UI) structure for the
#' `stepinWearable` application, setting up the layout, external dependencies,
#' and the modular structure for the Intervention and Assessment sections.
#'
#' @details The UI is currently organized with two main sections:
#' \itemize{
#'   \item \strong{Intervention:} The active tab containing modules for data loading,
#'         t-index confirmation, interactive visualization, and personalized challenge
#'         generation (\code{mod_intervention_ui}).
#'   \item \strong{Assessment:} The future tab, currently hidden or disabled, which is
#'         reserved for modules related to advanced analysis, multi-device data (Matrix,
#'         StepWatch), and data quality checks.
#' }
#'
#' @param request Internal parameter for the Shiny application.
#'
#' @importFrom shiny fluidPage titlePanel tabsetPanel tabPanel
#' @importFrom rintrojs introjsUI
#' @importFrom shinyjs useShinyjs
#'
#' @return A Shiny UI definition object.
#' @noRd
app_ui <- function(request) {
  
  # Theme for the app
  read_brand_yml <- function() {
    path <- app_sys("app/www/_brand.yml", mustWork = FALSE)
    if (!nzchar(path) || !file.exists(path)) return(list())
    path
  }
  brand <- read_brand_yml()
  theme <- bslib::bs_theme(brand = brand)
  
  # Dependencies and required external scripts (e.g., for Plotly, rintrojs)
  tagList(
    
    golem_add_external_resources(),
    bslib::bs_theme_dependencies(theme),
    shinyjs::useShinyjs(),
    
    # Leave this function call to enable the interactive tutorial
    rintrojs::introjsUI(),
    
    fluidPage(
      theme = theme,
      tags$div(class = "card app-hero mb-3",
               tags$div(class = "card-body d-flex justify-content-between align-items-center",
                        tags$div(
                          tags$h1(class = "h1 mb-0", "STEP-IN"),
                          tags$p(class = "subtitle mb-0", "Wearable Data Management")
                        ),
                        tags$img(src = "www/Jhmigueles_colored_wide.png", height = "98")
               )
      ),
      
      tabsetPanel(
        id = "main_tabs",
        # 1. INTERVENTION TAB (Active Module)
        tabPanel("Intervention",
                 mod_intervention_ui("intervention_1") # Assuming the UI module is called here
        ),
        
        # 2. ASSESSMENT TAB (Future Module)
        # The UI for this tab should be hidden or display a message about ongoing development.
        tabPanel("Assessment",
                 br(), br(),
                 h3("Assessment Module (Under Development)"),
                 p("This section is reserved for future integration of advanced analysis methods, including support for Matrix and StepWatch data, and visualizations for data quality and outcomes.")
        )
      )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "stepinWearable"
    )
    # Add here other external resources
    # e.g., shinyalert::useShinyalert()
  )
}
