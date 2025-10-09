#' Parse diary Excel data frame into start–end POSIXct segments
#'
#' @description
#' Accepts a data frame read from an Excel diary file and converts it into
#' a clean two–column data frame (`start`, `end`) with `POSIXct` timestamps.
#' 
#' The function supports two formats:
#' 
#' 1. **Full timestamps**: columns named `start` and `end`.
#' 2. **Split date + time**: columns named `start_date`, `start_time`,
#'    `end_date`, `end_time`.
#' 
#' Rows with invalid or missing values are dropped, and the result is
#' ordered chronologically.
#'
#' @param df A `data.frame` from [readxl::read_excel()] or similar.
#' @param tz Time zone to interpret timestamps in. Defaults to system time zone.
#'
#' @return A `data.frame` with two columns:
#'   \describe{
#'     \item{start}{POSIXct vector with start times}
#'     \item{end}{POSIXct vector with end times}
#'   }
#'
#' @examples
#' \dontrun{
#'   library(readxl)
#'   raw <- read_excel("diary_step_in_010pilot.xlsx")
#'   diary <- parse_diary_df(raw, tz = "UTC")
#' }
#'
#' @export
parse_diary_df <- function(df, tz = Sys.timezone()) {
  # normalize names
  names(df) <- tolower(trimws(names(df)))
  
  if (all(c("start", "end") %in% names(df))) {
    start <- as.POSIXct(df$start, tz = tz)
    end   <- as.POSIXct(df$end,   tz = tz)
  } else if (all(c("start_date", "start_time", "end_date", "end_time") %in% names(df))) {
    start <- as.POSIXct(paste(format(df$start_date), format(df$start_time, format = "%H:%M:%S")), tz = tz)
    end   <- as.POSIXct(paste(format(df$end_date), format(df$end_time, format = "%H:%M:%S")), tz = tz)
  } else {
    stop("Excel must contain either `start` and `end` columns, or `start_date`/`start_time`/`end_date`/`end_time`.")
  }
  
  out <- data.frame(start = start, end = end)
  # basic validation
  out <- out[!is.na(out$start) & !is.na(out$end) & out$start < out$end, , drop = FALSE]
  out <- out[order(out$start), , drop = FALSE]
  rownames(out) <- NULL
  out
}
