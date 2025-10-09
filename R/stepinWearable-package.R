#' stepinWearable: Personalized Wearable Platform for STEP-IN Project
#'
#' A Shiny application to ingest, process, visualize, and generate
#' personalized targets from minute-level wearable data (Fitbit, Matrix, StepWatch).
#' 
#'
#' @section Modules:
#' - **Intervention**: data loading, t-index, plots, decision engine.
#' - **Assessment**: (in development).
#'
#' @section Package options:
#' \describe{
#'   \item{\code{stepin.server_host}}{VPN server (char).}
#'   \item{\code{stepin.path_windows}}{Windows default path.}
#'   \item{\code{stepin.path_unix}}{MacOS default path.}
#'   \item{\code{stepin.output_dir}}{Default output folder.}
#'   \item{\code{stepin.fitbit_download}}{Default fitbit download folder.}
#' }
#' @keywords internal
"_PACKAGE"