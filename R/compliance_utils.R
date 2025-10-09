#' @title KPI Calculation and Compliance Check Helpers
#'
#' @description
#' Helper functions for calculating KIPs (medians) and checking compliance
#' flags used by the core decision engine (\code{decide_message}).
#'
#' @keywords internal utility kpi compliance
#' @noRd
NULL

#' Calculate Key Performance Indicators (KPIs) for a Window
#'
#' @description
#' Calculates the key median metrics for a given analysis window (steps and minutes
#' at different cadence thresholds).
#'
#' @param win A daily summary data frame for the target window.
#'
#' @return A list containing median values: \code{med_steps_day}, \code{med_steps_80plus},
#'   \code{med_steps_90plus}, \code{med_steps_100plus}.
#'
#' @importFrom stats median
kpis <- function(win) {
  tibble::tibble(
    n_days        = dplyr::n_distinct(win$date),
    med_steps_day = median(win$steps_day, na.rm = TRUE),
    med_steps_80plus  = median(win$steps_80plus,  na.rm = TRUE),
    med_steps_90plus  = median(win$steps_90plus,  na.rm = TRUE),
    med_steps_100plus = median(win$steps_100plus, na.rm = TRUE)
  )
}


#' Check Compliance for Step Goal (X)
#'
#' @description
#' Checks if the current window's median steps (X) shows an adequate increase
#' compared to the previous window's median (based on \code{steps_factor}).
#'
#' @param cur_med_steps Current median daily steps.
#' @param prev_med_steps Previous median daily steps.
#' @param steps_factor The growth factor required for success (e.g., 1.05 for +5\%).
#'
#' @return Logical. \code{TRUE} if compliance threshold is met.
steps_met <- function(cur_med_steps, prev_med_steps, steps_factor) {
  if (is.na(prev_med_steps) || is.nan(prev_med_steps)) return(TRUE) # return TRUE if not previous x, only in t = 0
  (cur_med_steps / max(1, prev_med_steps)) >= steps_factor - 0.01
}

#' Check Compliance for Minute Goal (Y)
#'
#' @description
#' Checks if the current window's median minutes at the target cadence (Y) meets
#' the previous target \code{target_Y}. Note: compliance is based on meeting or
#' exceeding the exact previous target Y.
#'
#' @param cur_med_minutes Current median minutes at the target cadence (Z).
#' @param target_Y The previous minute target (Y).
#' @param minutes_inc The increment factor (used in \code{decide_message} but kept here for completeness).
#'
#' @return Logical. \code{TRUE} if compliance threshold is met.
minutes_met <- function(cur_med_minutes, target_Y, minutes_inc) {
  if (is.na(target_Y) || target_Y <= 0) return(NA)
  cur_med_minutes >= (minutes_inc - 1)
}


#' Check if Cadence Z=80 Should Escalate to Z=90
#'
#' @description
#' Rule check: Should escalate from Z=80 to Z=90 if the median minutes at 80+
#' are sufficiently high (e.g., >= 15 minutes).
#'
#' @param cur_med_steps_80plus Current median minutes at 80+ steps/min.
#'
#' @return Logical.
should_escalate_from_80 <- function(cur_med_steps_80plus)  cur_med_steps_80plus  >= 15  # escalate to ≥90


#' Check if Cadence Z=90 Should Escalate to Z=100
#'
#' @description
#' Rule check: Should escalate from Z=90 to Z=100 if the median minutes at 90+
#' are sufficiently high (e.g., >= 15 minutes).
#'
#' @param cur_med_steps_90plus Current median minutes at 90+ steps/min.
#'
#' @return Logical.
should_escalate_from_90 <- function(cur_med_steps_90plus)  cur_med_steps_90plus  >= 15  # escalate to ≥100
