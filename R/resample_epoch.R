#' Resample time series data to user-defined epochs
#'
#' Aggregates a numeric variable in a two-column data frame into regular epochs
#' of user-defined duration (in seconds), returning both the aggregated value
#' and number of valid samples per epoch.
#'
#' @param df A data frame with exactly two columns:
#'   - First column: POSIXct timestamps.
#'   - Second column: numeric values to aggregate.
#' @param FUN A function to aggregate the numeric values (default is \code{sum}).
#' @param desired_epoch_s Length of epoch in seconds (default: 60).
#'
#' @return A data frame with:
#'   - \code{timestamp_epoch}: the start of each epoch.
#'   - \code{n_samples}: number of non-NA values in that epoch.
#'   - \code{value}: aggregated value for the epoch.
#'
#' @examples
#' df <- data.frame(
#'   timestamp = as.POSIXct("2025-05-30 18:00:00") + seq(0, 180, by = 5),
#'   steps = sample(0:5, 37, replace = TRUE)
#' )
#' resample_epoch(df, FUN = sum, desired_epoch_s = 60)
#'
#' @export
resample_epoch <- function(df, FUN = sum, desired_epoch_s = 60) {
  if (ncol(df) != 2) stop("df must have exactly 2 columns: timestamp and a numeric variable.")
  if (!inherits(df[[1]], "POSIXct")) stop("First column must be POSIXct timestamp.")
  
  # Rename columns
  cnames = colnames(df)
  colnames(df) <- c("timestamp", "value")
  
  # Define epoch start times by flooring to nearest epoch
  epoch_start <- as.POSIXct(floor(as.numeric(df$timestamp) / desired_epoch_s) * desired_epoch_s,
                            origin = "1970-01-01", tz = attr(df$timestamp, "tzone"))
  
  df$timestamp_epoch <- epoch_start
  
  if (all(df$timestamp == df$timestamp_epoch)) {
    
    output = data.frame(timestamp = df$timestamp,
                        n_samples = rep(1, nrow(df)),
                        value = df[[2]])
  } else {
    # Split and aggregate
    groups <- split(df, df$timestamp_epoch)
    
    output <- do.call(rbind, lapply(groups, function(g) {
      data.frame(
        timestamp_epoch = g$timestamp_epoch[1],
        n_samples = sum(!is.na(g$value)),
        value = FUN(g$value, na.rm = TRUE)
      )
    }))
    
    rownames(output) <- NULL
    
  }
  
 
  colnames(output) = c("timestamp", paste0("n_", cnames[2]), cnames[2])
  return(output)
}
