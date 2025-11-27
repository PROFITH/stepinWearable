#' Preprocess Matrix Accelerometer File with GGIR
#'
#' @description
#' Executes a customized GGIR analysis pipeline on raw Matrix device `.bin` files
#' to extract step count time series. It includes file preparation, step detection
#' using a custom function (`verisense_count_steps()`), and aggregation of results.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Copies the raw `.bin` file to a participant-specific folder.
#'   \item Configures and runs GGIR in \code{mode = 1} with a user-defined step-counting function.
#'   \item Loads the processed results from GGIR's \code{meta/basic} folder.
#'   \item Converts timestamps, renames columns, and resamples step data to hourly resolution.
#' }
#' It assumes a sampling frequency of 15 Hz and uses preconfigured parameters suited for Verisense devices.
#'
#' @param matrix_download_dir Character. Path to the directory in which the input `.bin` files recorded by the Matrix device are stored.
#' @param timeseries_dir Character. Path to the output directory for storing processed GGIR results.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{\code{time}}{POSIXct timestamps (hourly).}
#'   \item{\code{steps}}{Number of detected steps in each hour.}
#' }
#'
#' @import GGIR
#' @import GGIRread
#' @seealso \code{\link{verisense_count_steps}}, \code{\link{resample_epoch}}
#'
#' @examples
#' \dontrun{
#' steps_df <- preprocess_matrix(
#'   matrix_download_dir = "path/to/raw_matrix_dir/",
#'   timeseries_dir = "data/processed/timeseries/matrix"
#' )
#' }
#'
#' @export
preprocess_matrix <- function(matrix_download_dir = character(0),
                              timeseries_dir = character(0)) {
  message("Pre-processing matrix file with GGIR...")
  
  # define directories if null
  if (length(matrix_download_dir) == 0) {
    if (dir.exists(MATRIX_DOWNLOAD_DIR)) {
      matrix_download_dir = MATRIX_DOWNLOAD_DIR
    } else {
      stop("define directories for preprocessing")
    }
  }
  if (length(timeseries_dir) == 0) {
    if (dir.exists(WEARABLE_OUTPUT_DIR)) {
      timeseries_dir = file.path(WEARABLE_OUTPUT_DIR, "output", "matrix")
    } else {
      stop("define directories for preprocessing")
    }
  }
  
  # parameters for calculation of steps
  myfun <- list(
    FUN = verisense_count_steps,
    parameters = c(4, 4, 20, -1.0, 4, 4, 0.01, 1.25),
    expected_sample_rate = 15,
    expected_unit = "g",
    colnames = c("step_count"),
    outputres = 1,
    minlength = 1,
    outputtype = "numeric",
    aggfunction = sum,
    timestamp = FALSE,
    reporttype = "event"
  )
  
  # in pre-processing, we do not use diary:
  GGIR::GGIR(
    datadir = matrix_download_dir,
    outputdir = timeseries_dir,
    do.report = c(),
    mode = 1,
    overwrite = FALSE,
    myfun = myfun,
    visualreport = FALSE
  )
  
  # load timeseries
  message("Matrix analysis completed.")
}

# plot_matrix_timeseries <- function(participant_file) {
#   # load timeseries
#   basicdir = file.path(timeseries_dir, "output_matrix", "meta", "basic")
#   p1files = dir(basicdir, full.names = T)
#   file2load = grep(paste0("meta_", basename(rawfile), ".RData"),
#                    p1files, value = T, fixed = T)
#   M = NULL
#   load(file2load)
#   steps = M$metashort[, c("timestamp", "step_count")]
#   
#   # format time
#   steps$timestamp = as.POSIXct(steps$timestamp, format = "%Y-%m-%dT%H:%M:%S%z")
#   colnames(steps) = c("timestamp", "steps")
#   
#   # extract heart rate
#   message("Extracting heart rate...")
#   hr = GGIRread::readParmayMatrix(rawfile, output = "all", read_acc = TRUE,
#                                   read_gyro = FALSE, read_heart = TRUE)
#   hr = hr$data[hr$data$time %in% as.numeric(steps$timestamp), c("time", "hr")]
#   hr$timestamp = as.POSIXct(hr$time, tz = "Europe/Madrid")
#   hr = hr[, c("timestamp", "hr")]
#   
#   # save hr timeseries
#   if (!dir.exists(file.path(timeseries_dir, "output_hr-matrix"))) {
#     dir.create(file.path(timeseries_dir, "output_hr-matrix"))
#   }
#   save(hr,
#        file = file.path(timeseries_dir, "output_hr-matrix",
#                         paste0(participant_id, ".RData")))
#   
#   # aggregate to 60 seconds
#   steps = resample_epoch(steps)
#   
#   # return for visualization
#   message("Matrix analysis completed.")
#   return(steps[, c("timestamp", "steps")])
# }

#' Finalize GGIR processing (parts 2:5) on matrix file and load reports
#'
#' @param timeseries_dir Directory where GGIR should write output.
#' @param ggir_sleep_diary Optional path to a GGIR sleep diary (CSV).
#'
#' @return A named list of data frames (`ggir_reports`), one per report CSV
#'   found under the GGIR results folder. List names are the base filenames.
#' @export
process_matrix <- function(timeseries_dir = NULL,
                           ggir_sleep_diary = NULL) {
  message("Pre-processing matrix file with GGIR...")
  
  # in pre-processing, we do not use diary:
  GGIR::GGIR(
    datadir = "matrix",
    outputdir = timeseries_dir,
    mode = 2:5,
    overwrite = FALSE, ## temporal
    visualreport = TRUE,
    idloc = 6,
    # sleep
    loglocation = ggir_sleep_diary,
    HASPT.ignore.invalid = NA,
    # physical activity
    mvpathreshold = 100,
    threshold.lig = 35, threshold.mod = 100, threshold.vig = 400,
    boutdur.in = c(30, 60), boutdur.mvpa = c(1, 2, 5, 10), boutdur.lig = 10,
    # cleaning
    includedaycrit.part5 = 8,
    includedaycrit = 1,
    # reports
    do.report = c(2, 4, 5),
    visualreport = TRUE, old_visualreport = FALSE,
    visualreport_without_invalid = FALSE
  )
  
  # load and return GGIR output
  resultsdir = file.path(timeseries_dir, "output_matrix", "results")
  ggir_reports_paths = list.files(resultsdir, recursive = T, pattern = "*\\.csv$",
                                  full.names = TRUE)
  
  if (length(ggir_reports_paths) == 0) {
    warning("No GGIR report CSVs found in: ", resultsdir)
    return(list())
  }
  
  # read each CSV into a list of data frames
  ggir_reports <- lapply(ggir_reports_paths, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  })
  names(ggir_reports) <- basename(ggir_reports_paths)
  
  message("Matrix analysis completed.")
  return(ggir_reports)
}