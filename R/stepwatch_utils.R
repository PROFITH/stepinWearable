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
#' @param stepwatch_download_dir Character vector. One or more file paths to StepWatch CSV files.
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
preprocess_stepwatch = function(stepwatch_download_dir = character(0),
                                timeseries_dir = character(0)) {
  
  message("Pre-processing stepwatch data...")
  # define directories if null
  if (length(stepwatch_download_dir) == 0) {
    if (dir.exists(STEPWATCH_DOWNLOAD_DIR)) {
      stepwatch_download_dir = STEPWATCH_DOWNLOAD_DIR
    } else {
      stop("define directories for preprocessing")
    }
  }
  if (length(timeseries_dir) == 0) {
    if (dir.exists(WEARABLE_OUTPUT_DIR)) {
      timeseries_dir = file.path(WEARABLE_OUTPUT_DIR, "output", "stepwatch", "meta")
      if (!dir.exists(timeseries_dir)) dir.create(timeseries_dir)
    } else {
      stop("define directories for preprocessing")
    }
  }
  
  # loop through participants
  participants = dir(stepwatch_download_dir, recursive = F, 
                     full.names = TRUE, include.dirs = T)
  
  for (i in seq_along(participants)) {
    pfolders = Sys.glob(file.path(participants[i], "*10 Sec Bin*"))
    for (pfolder in pfolders) {    
    file_path = dir(pfolder, pattern = "csv$", full.names = T)
    
    file_path <- sort(file_path)
    participant_id = basename(participants[i])
    file_path <- sort(file_path)
    f1 <- basename(file_path[1])
    date0 <- regmatches(f1, regexpr("\\d{8}", f1))
    if (!is.na(date0) && nzchar(date0)) {
      tsfile0 <- file.path(timeseries_dir, paste0(participant_id, "-", date0, ".RData"))
      if (file.exists(tsfile0)) next
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
    date = format(stepwatch$timestamp[1], format = "%Y%m%d")
    participant_id = basename(participants[i])
    tsfile = file.path(timeseries_dir, 
                       paste0(participant_id, "-", date, ".RData"))
    if (file.exists(tsfile)) next
    save(stepwatch, file = tsfile)
    }
  }
  
  # final message
  message("Processed data saved to: ", timeseries_dir)
}
