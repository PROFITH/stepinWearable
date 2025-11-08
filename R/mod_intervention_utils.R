#' @title Data Preparation and Window Selection Helpers
#'
#' @description
#' Helper functions for summarizing minute-level data and selecting the
#' current and previous analysis windows based on custom criteria.
#'
#' @keywords internal utility data-prep
#' @noRd
NULL


#' Calculate Daily Summary from Minute-Level Data (+ validity flags)
#'
#' Aggregates minute-level data to daily totals and flags valid/invalid days by:
#'   1) HR criterion: >= 10h of heart-rate minutes with max consecutive HR gap <= 90 min
#'   2) Steps criterion: >= steps_threshold steps in the day (default 500)
#'
#' @param df Data frame with POSIXct `timestamp`, numeric `steps`, and a heart-rate column
#'   (any of: `heart_rate`, `HeartRate`, `hr`, `HR`, `bpm`). One row per minute is ideal;
#'   missing minutes are filled during evaluation.
#' @param tz Local timezone for day anchoring (default "Europe/Madrid").
#' @param steps_threshold Integer, daily step threshold for valid SC day (default 500).
#' @param gap_allowance Integer, maximum allowed consecutive minutes without HR (default 90).
#' @param required_hr_minutes Integer, HR minutes required per day (default 600 = 10h).
#'
#' @return Tibble with daily totals plus:
#'   wear_minutes_hr, max_hr_gap_min, valid_hr_day, valid_sc_day
#'
#' @importFrom dplyr mutate group_by summarise n_distinct arrange
#' @importFrom tidyr complete
#' @importFrom lubridate as_date with_tz floor_date ceiling_date
daily_summary <- function(df,
                          tz = "Europe/Madrid",
                          steps_threshold = 500L,
                          gap_allowance = 90L,
                          required_hr_minutes = 600L) {
  
  # ---- basic checks / column resolution ----
  if (!("timestamp" %in% names(df)))
    stop("`df` must contain a POSIXct `timestamp` column.")
  
  if (!("steps" %in% names(df)))
    stop("`df` must contain a numeric `steps` column.")
  
  hr_candidates <- intersect(names(df), c("heart_rate","HeartRate","hr","HR","bpm"))
  if (length(hr_candidates) == 0)
    stop("`df` must contain a heart-rate column: one of `heart_rate`, `HeartRate`, `hr`, `HR`, `bpm`.")
  hr_col <- hr_candidates[1]
  
  # coerce types safely
  df <- df %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, tz = attr(timestamp, "tzone") %||% "UTC"),
      steps = as.numeric(steps),
      hr    = suppressWarnings(as.numeric(.data[[hr_col]]))
    )
  
  # derive local calendar day
  df <- df %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::with_tz(timestamp, tz)))
  
  # helper: evaluate HR criterion for a single day
  eval_hr_day <- function(day_df, day,
                          tz = "Europe/Madrid",
                          required_hr_minutes = 600L,
                          gap_allowance = 90L) {
    # Build the full minute index for the day in local tz
    day_start <- as.POSIXct(paste0(day, " 00:00:00"), tz = tz)
    day_end   <- as.POSIXct(paste0(day, " 23:59:00"), tz = tz)
    full_idx  <- data.frame(timestamp = seq(day_start, day_end, by = "1 min"))
    
    # Align timestamps and join HR
    day_df$timestamp <- lubridate::with_tz(day_df$timestamp, tz)
    x <- merge(full_idx, day_df[, c("timestamp","hr")], by = "timestamp", all.x = TRUE)
    
    # Wear minute = hr > 0
    wear_raw <- !is.na(x$hr) & x$hr > 0
    raw_hr_minutes <- sum(wear_raw, na.rm = TRUE)
    
    # RLE over wear/no-wear to identify *bounded* gaps
    r <- rle(wear_raw)              # TRUE = wear, FALSE = no-HR gap
    vals <- r$values
    lens <- r$lengths
    nrun <- length(vals)
    
    credited_from_gaps <- 0L
    disallowed_from_gaps <- 0L
    longest_no_hr_run <- if (any(!vals)) max(lens[!vals]) else 0L
    
    if (nrun > 0) {
      idx_nohr <- which(!vals)
      for (i in idx_nohr) {
        prev_is_wear <- (i > 1)       && isTRUE(vals[i - 1])
        next_is_wear <- (i < nrun)    && isTRUE(vals[i + 1])
        
        gap_len <- lens[i]
        
        if (prev_is_wear && next_is_wear) {
          # BOUNDED GAP: credit up to gap_allowance; anything beyond is disallowed
          credit_i     <- min(gap_len, gap_allowance)
          disallow_i   <- max(gap_len - gap_allowance, 0L)
        } else {
          # UNBOUNDED GAP (touches day boundary or no wear on one side): no credit
          credit_i     <- 0L
          disallow_i   <- gap_len
        }
        
        credited_from_gaps   <- credited_from_gaps + credit_i
        disallowed_from_gaps <- disallowed_from_gaps + disallow_i
      }
    }
    
    adjusted_wear_minutes <- as.integer(raw_hr_minutes + credited_from_gaps)
    valid <- adjusted_wear_minutes >= required_hr_minutes
    
    list(
      wear_minutes_hr_raw      = as.integer(raw_hr_minutes),
      wear_minutes_hr_adjusted = adjusted_wear_minutes,
      credited_gap_minutes     = as.integer(credited_from_gaps),
      disallowed_gap_minutes   = as.integer(disallowed_from_gaps),
      longest_no_hr_run        = as.integer(longest_no_hr_run),
      valid_hr_day             = valid
    )
  }
  
  # aggregate steps + cadence minutes (as before)
  base_daily <- df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      steps_day     = sum(steps, na.rm = TRUE),
      steps_80plus  = sum(steps >= 80,  na.rm = TRUE),
      steps_90plus  = sum(steps >= 90,  na.rm = TRUE),
      steps_100plus = sum(steps >= 100, na.rm = TRUE),
      .groups = "drop"
    )
 
  # evaluate HR validity per day
  hr_daily <- df %>%
    dplyr::group_by(date) %>%
    dplyr::group_modify(~{
      day <- .y$date[[1]]
      res <- eval_hr_day(.x, day = day, tz = "Europe/Madrid",
                         required_hr_minutes = 600L, gap_allowance = 90L)
      data.frame(
        wear_minutes_hr_raw      = res$wear_minutes_hr_raw,
        wear_minutes_hr_adjusted = res$wear_minutes_hr_adjusted,
        credited_gap_minutes     = res$credited_gap_minutes,
        disallowed_gap_minutes   = res$disallowed_gap_minutes,
        longest_no_hr_run        = res$longest_no_hr_run,
        valid_hr_day             = res$valid_hr_day
      )
    }) %>%
    dplyr::ungroup()
  
  # steps-based validity
  sc_daily <- base_daily %>%
    dplyr::mutate(valid_sc_day = steps_day >= steps_threshold) %>%
    dplyr::select(date, valid_sc_day)
  
  # combine
  out <- base_daily %>%
    dplyr::left_join(hr_daily, by = "date") %>%
    dplyr::left_join(sc_daily, by = "date") %>%
    dplyr::arrange(date)
  
  # valid
  out$valid_day = out$valid_hr_day
  
  out
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
