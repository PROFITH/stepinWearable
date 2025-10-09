#' Preprocess StepWatch CSV Files
#'
#' @description
#' Reads and processes StepWatch accelerometer CSV files to extract step count data, 
#' applies basic formatting, and optionally saves the raw and processed data 
#' to participant-specific directories.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Reads and merges one or more StepWatch CSV files.
#'   \item Extracts timestamped step count columns (identified by "BINNED").
#'   \item Converts timestamps to POSIXct format and forces local timezone.
#'   \item Converts step counts from 30-second to 60-second epochs by doubling the count.
#'   \item Resamples the time series using \code{resample_epoch()}.
#'   \item Optionally saves raw and processed files to specified output folders.
#' }
#'
#' @param file_path Character vector. One or more file paths to StepWatch CSV files.
#' @param participant_id Character. Unique participant identifier, used in output filenames.
#' @param timeseries_dir Character. Directory to save the processed time series.
#'                       If \code{NULL}, the time series is not saved.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{\code{timestamp}}{POSIXct timestamps (1-minute resolution).}
#'   \item{\code{steps}}{Number of steps per minute.}
#' }
#'
#' @importFrom data.table fread fwrite
#' @importFrom lubridate force_tz
#' @seealso \code{\link{resample_epoch}}
#'
#' @examples
#' \dontrun{
#' preprocess_stepwatch(
#'   file_path = c("data/stepwatch_001.csv"),
#'   participant_id = "001",
#'   timeseries_dir = "data/processed/timeseries/stepwatch"
#' )
#' }
#'
#' @export
preprocess_stepwatch = function(file_path, participant_id, 
                                timeseries_dir = NULL) {
  
  # set up directory paths
  if (is.null(timeseries_dir)) {
    save_result = FALSE
  } else {
    save_result = TRUE
  }
  
  # save raw to folder
  data = do.call(rbind, lapply(file_path, FUN = function(x) data.table::fread(x, data.table = FALSE)))

  # read data in and r-bind
  stepwatch = data[, grep("^BINNED", colnames(data))]
  colnames(stepwatch) = ifelse(grepl("TIME", colnames(stepwatch)), "timestamp", "steps")
  
  # format time
  stepwatch$timestamp = lubridate::force_tz(as.POSIXct(stepwatch$timestamp,
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                            tzone = "")
  stepwatch$steps = stepwatch$steps * 2
  
  # resample
  stepwatch = resample_epoch(stepwatch)
  
  # save to file
  if (save_result) {
    nfiles = length(list.files(timeseries_dir))
    tsfile = file.path(timeseries_dir, 
                       paste0(participant_id, ".t", nfiles, ".RData"))
    save(stepwatch, file = tsfile)
    message("Processed data saved to: ", tsfile)
  }
  
  # final message
  message("Processed data saved to: ", timeseries_dir)
  return(stepwatch[, c("timestamp", "steps")])
}
