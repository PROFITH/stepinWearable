#' @title Data Preparation and Window Selection Helpers
#'
#' @description
#' Helper functions for summarizing minute-level data and selecting the
#' current and previous analysis windows based on custom criteria.
#'
#' @keywords internal utility data-prep
#' @noRd
NULL


#' Calculate Daily Summary from Minute-Level Data
#'
#' @description
#' Aggregates minute-level steps data into a daily summary, calculating total steps
#' and the number of minutes accumulated above specific cadence thresholds (80, 90, 100
#' steps/minute).
#'
#' @param df A data frame/tibble with minute-level data, including a POSIXct \code{timestamp}
#'   and numeric \code{steps} columns.
#' @param tz Character string specifying the local time zone (e.g., "Europe/Madrid") to be used
#'   for anchoring the day boundaries.
#'
#' @return A tibble with daily summaries, including: \code{date} (Date), \code{steps_day} (total steps),
#'   \code{steps_80plus}, \code{steps_90plus}, \code{steps_100plus} (minutes with steps >= threshold).
#'
#' @importFrom dplyr mutate group_by summarise n_distinct
#' @importFrom lubridate as_date with_tz
daily_summary <- function(df, tz = "Europe/Madrid") {
  df %>%
    dplyr::mutate(
      # Derive "calendar day" in the chosen local tz (robust if df is UTC)
      date = lubridate::as_date(lubridate::with_tz(timestamp, tz))
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      steps_day     = sum(steps, na.rm = TRUE),
      steps_80plus  = sum(steps >= 80,  na.rm = TRUE),
      steps_90plus  = sum(steps >= 90,  na.rm = TRUE),
      steps_100plus = sum(steps >= 100, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Select Current and Previous Analysis Windows
#'
#' @description
#' Given the complete daily summary (\code{dsum}) and the desired \code{start_date}/\code{end_date}
#' for the current window, this function extracts the data for the current window and
#' the data for the preceding window of the same length.
#'
#' @param dsum A daily summary data frame (output of \code{daily_summary}).
#' @param start_date Date object marking the start of the current analysis window (inclusive).
#' @param end_date Date object marking the end of the current analysis window (inclusive).
#'
#' @return A list with two tibbles:
#'   \itemize{
#'     \item \code{current}: Daily data within the specified \code{start_date} and \code{end_date}.
#'     \item \code{previous}: Daily data for the period immediately preceding the current window, with the same duration.
#'     \item \code{start_date}, \code{end_date}: The confirmed start/end dates of the current window.
#'   }
#'
#' @importFrom dplyr filter
select_windows <- function(dsum, start_date, end_date) {
  stopifnot(nrow(dsum) > 0)
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (end_date < start_date) {
    tmp <- start_date; start_date <- end_date; end_date <- tmp
  }
  # current window (inclusive)
  cur <- dplyr::filter(dsum, date >= start_date & date <= end_date)
  # enforce integer day count (could be < requested if missing days in dsum)
  n_days <- as.integer(end_date - start_date + 1)
  prev_end   <- start_date - 1
  prev_start <- prev_end - (n_days - 1)
  prev <- dplyr::filter(dsum, date >= prev_start & date <= prev_end)
  list(current = cur, previous = prev, end_date = end_date)
}

#' Check for Recent Data Gaps
#'
#' @description
#' Determines if there is a significant data gap (3 consecutive days with zero
#' activity or missing entries) at the end of the analysis window, indicating a
#' connection or wear failure.
#'
#' @param dsum A daily summary data frame.
#' @param end_date Date object marking the end of the current analysis window.
#'
#' @return Logical. \code{TRUE} if the last three days of the window have no data, \code{FALSE} otherwise.
#' @importFrom dplyr filter pull
no_data_last_3_days <- function(dsum, end_date = NULL) {
  if (nrow(dsum) == 0) return(TRUE)
  if (is.null(end_date)) end_date <- max(dsum$date)
  recent <- dplyr::filter(dsum, date >= (end_date - 2) & date <= end_date)
  nrow(recent) < 3 || all(recent$steps_day == 0)
}
