#' @title Message Templates, Tips, and Decision Engine
#'
#' @description
#' This file contains the core logic for the STEP-IN personalized intervention:
#' \enumerate{
#'   \item Participant-facing message templates (in Spanish).
#'   \item Tip blocks for steps, intensity, and mixed failures.
#'   \item The main decision engine (`decide_message`).
#'   \item Helper functions for state comparison and message rendering.
#' }
#'
#' NOTE: The intervention logic is structured around different "phases" (post-basal, m1-m3, m4-m9)
#' inferred from the number of successful reviews saved in the participant's history.
#'
#' @keywords internal messages intervention decision
#' @importFrom yaml read_yaml
#' @noRd
NULL

# ============================
#        Message templates
# ============================
# NOTE: Templates are in Spanish (participant-facing), by project design.
# All developer comments and documentation are in English.
pkg_file <- function(...) {
  system.file(..., package = "stepinWearable", mustWork = TRUE)
}

load_messages <- local({
  cache <- NULL
  function(refresh = FALSE) {
    if (isTRUE(refresh) || is.null(cache)) {
      path  <- pkg_file("extdata/messages.yml")
      cache <<- yaml::read_yaml(path)
    }
    cache
  }
})


# Small helper for defaulting values
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

# ============================
#      Decision engine
# ============================
#' Decision Engine for Intervention Messages and Targets
#'
#' @description
#' The core function that determines the next step, minutes, and cadence goals
#' (X, Y, Z) and selects the appropriate message template based on the current
#' intervention phase and the participant's compliance in the last 2-week window.
#'
#' Logic Phases are:
#' \itemize{
#'   \item \code{post_basal} (0 reviews): Set initial step goal \code{X} based on current data.
#'   \item \code{m1_3} (1-6 reviews): Focus on achieving step goal \code{X}.
#'   \item \code{init_m4} (7 reviews): Introduce the intensity goal (\code{Y} and \code{Z}).
#'   \item \code{m4_9} (>=8 reviews): Evaluate both step and intensity goals, allowing for escalation of cadence (\code{Z}).
#' }
#'
#' @param state A list representing the participant's historical intervention state (from \code{load_participant_state}), including \code{history}, \code{last_X}, \code{last_Y}, \code{last_Z}, and \code{consecutive_fails}.
#' @param cur_k A list of KPIs calculated for the current 2-week window.
#' @param prev_k A list of KPIs calculated for the previous 2-week window (or \code{NULL}).
#' @param nombre Character string for the participant's name (used for message personalization).
#' @param steps_factor Numeric factor used to increase the step goal \code{X} upon success (e.g., 1.05 for +5%).
#' @param minutes_inc Integer increment for the minute goal \code{Y} upon success (e.g., 5 minutes).
#' @param t Integer t-index for the current 2-week window.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{key}: The message template key chosen (e.g., "ambos3").
#'   \item \code{text}: The final rendered message text (Spanish).
#'   \item \code{next_X}, \code{next_Y}, \code{next_Z}: The new targets to be saved for the next cycle.
#'   \item \code{consecutive_fails}: Updated failure streak count (used for supportive message inclusion).
#' }
#'
#' @importFrom dplyr case_when
#' @importFrom glue glue
#' @noRd
decide_message <- function(state, cur_k, prev_k, nombre,
                           steps_factor = 1.05, minutes_inc = 5L, t) {
  # load messages
  msgs <- load_messages()  # <-- runtime
  message_templates <- msgs$templates
  
  # Phase is inferred from t-index that is being processed (confirmed by user)
  # 0 -> post-basal; 1..6 -> months 1-3; 7 -> start of month 4; >=8 -> months 4-9.
  phase <- dplyr::case_when(
    t == 0          ~ "post_basal",
    t >= 1 & t <= 5 ~       "m1_3",
    t == 6          ~    "init_m4",
    TRUE            ~       "m4_9"
  )
  
  # Previous targets (if any)
  prev_X <- state$last_X
  prev_Y <- state$last_Y
  prev_Z <- state$last_Z
  last_steps_factor <- state$last_steps_factor
  last_minutes_inc <- state$last_minutes_inc
  
  has_prev_X <- !is.null(prev_X) && is.finite(prev_X) && t > 0
  has_prev_Y <- !is.null(prev_Y) && is.finite(prev_Y) && prev_Y > 0 && t > 6
  has_prev_Z <- !is.null(prev_Z) && is.finite(prev_Z) && t > 6
  
  # Steps improvement flag (>=(X-1)% vs. previous window)
  steps_ok <- if (!is.null(prev_k) && nrow(prev_k) == 1) {
    steps_met(cur_k$med_steps_day, prev_X, last_steps_factor)
  } else NA
  
  # Minutes improvement flag
  cur_minutes_at_prevZ <- dplyr::case_when(
    has_prev_Z && prev_Z == 100 ~ cur_k$med_steps_100plus,
    has_prev_Z && prev_Z ==  90 ~ cur_k$med_steps_90plus,
    has_prev_Z && prev_Z ==  80 ~ cur_k$med_steps_80plus,
    TRUE ~ NA_real_
  )
  mins_ok <- if (has_prev_Y && !is.na(cur_minutes_at_prevZ)) {
    minutes_met(cur_minutes_at_prevZ, prev_Y, last_minutes_inc)
  } else NA
  
  # ---- Next targets ----
  # X: base on previous target
  base_X <- if (has_prev_X && isFALSE(steps_ok)) prev_X else cur_k$med_steps_day
  next_X <- round((base_X * steps_factor)/10)*10 # round to tens
  
  # Z: introduce on init_m4 (t = 6), maybe escalate on m4_9, otherwise keep
  if (phase == "init_m4" && !has_prev_Z) {
    next_Z <- next_Z <- dplyr::case_when(
      cur_k$med_steps_100plus >= 5 ~ 100L,
      cur_k$med_steps_90plus  > 15 ~ 100L,
      cur_k$med_steps_90plus  >= 5 ~  90L,
      cur_k$med_steps_80plus  > 15 ~  90L,
      TRUE                         ~  80L
    )
    # start intensity with next of currently accumulated among 5, 10, 15
    # after rounding the median minutes to the closer 5
    varname = paste0("med_steps_", next_Z, "plus")
    next_Y <- dplyr::case_when(
      # if next_Z is 100 and participant already accumulates more than 12.5, 
      # then next 5-min multiple of current median
      cur_k[[varname]] >= 12.5 ~ as.integer(((cur_k[[varname]] %/% 5) + 1) * 5),
      cur_k[[varname]] >= 7.5 ~ 15L,
      cur_k[[varname]] >= 2.5 ~ 10L,
      TRUE                   ~  5L
    )
  } else {
    # default keep same Z
    next_Z <- if (has_prev_Z) as.integer(prev_Z) else NA_integer_
    
    # Z escalation on m4_9
    escalate <- FALSE
    if (phase == "m4_9" && has_prev_Z) {
      if (prev_Z == 90 && should_escalate_from_90(cur_k$med_steps_90plus)) {
        next_Z <- 100L; escalate <- TRUE
      } else if (prev_Z == 80 && should_escalate_from_80(cur_k$med_steps_80plus)) {
        next_Z <- 90L;  escalate <- TRUE
      }
    }
    # Y from slider: reset to minutes_inc on escalation/first-time, else add minutes_inc
    if (escalate || !has_prev_Y) next_Y <- as.integer(minutes_inc)
    else                         next_Y <- as.integer(prev_Y + minutes_inc)
  }
  
  # ------------------------------------------------------
  
  # Template key (depends on success flags + phase)
  key <- dplyr::case_when(
    phase == "post_basal" ~ "msg0",
    phase == "m1_3"    & isTRUE(steps_ok)                     ~ "pasos1",
    phase == "m1_3"    & !isTRUE(steps_ok)                    ~ "pasos2",
    phase == "init_m4" & isTRUE(steps_ok)                     ~ "ambos1",
    phase == "init_m4" & !isTRUE(steps_ok)                    ~ "ambos2",
    phase == "m4_9"    & isTRUE(steps_ok) & isTRUE(mins_ok)   ~ "ambos3",
    phase == "m4_9"    & isTRUE(steps_ok) & !isTRUE(mins_ok)  ~ "ambos6",
    phase == "m4_9"    & !isTRUE(steps_ok) & isTRUE(mins_ok)  ~ "ambos7",
    phase == "m4_9"    & !isTRUE(steps_ok) & !isTRUE(mins_ok) ~ "ambos5",
    TRUE ~ "ambos5" # safe fallback
  )
  
  # Failure streak update (used to append supportive text after 2 consecutive fails)
  failed_this_round <- (phase == "m1_3"  & !isTRUE(steps_ok)) ||
    (phase == "m4_9"  & (!isTRUE(steps_ok) | !isTRUE(mins_ok)))
  new_consecutive_fails <- if (isTRUE(failed_this_round)) (state$consecutive_fails %||% 0L) + 1L else 0L
  new_consecutive_success <- if (phase != "post_basal" && isFALSE(failed_this_round)) (state$consecutive_success %||% 0L) + 1L else 0L
  
  # Render message with the slider-derived targets
  glue_env <- list(
    nombre = nombre %||% "Nombre",
    X = format(next_X, big.mark = ".", decimal.mark = ","),
    Y = next_Y,
    Z = next_Z,
    tips_pasos = msgs$tips$pasos,
    tips_intensidad = msgs$tips$intensidad,
    tips_mixto = msgs$tips$mixto
  )
  txt <- do.call(glue::glue, c(list(message_templates[[key]]), glue_env))
  if (new_consecutive_fails >= 2L) {
    txt <- paste0(txt, "\n\n", msgs$support$consecutive_failures_paragraph)
  }
  
  if (new_consecutive_success >= 2L) {
    txt <- paste0(txt, "\n\n", msgs$support$consecutive_success_paragraph)
  }
  
  # return
  list(
    key = key, text = txt,
    next_X = next_X, next_Y = next_Y, next_Z = next_Z,
    consecutive_fails = new_consecutive_fails,
    consecutive_success = new_consecutive_success,
    steps_met = steps_ok, cadence_met = mins_ok
  )
}


#' Render a Message Template with Glue Variables
#'
#' @description
#' A helper function to re-render a message template (used when the user manually
#' overrides the automatic message choice). It ensures the same targets (X, Y, Z)
#' and tips are used.
#'
#' @param template_key Character string identifying the template in \code{message_templates}.
#' @param glue_env List of glue variables (\code{nombre}, \code{X}, \code{Y}, \code{Z}, \code{tips_...}) pre-computed by \code{decide_message}.
#' @param add_supportive Logical. If \code{TRUE}, the special supportive paragraph for consecutive failures is appended.
#'
#' @return The final rendered message text (character string, Spanish).
#'
#' @importFrom glue glue
#' @noRd
render_template_with_env <- function(template_key, glue_env, add_supportive, add_congrats) {
  # load messages
  msgs <- load_messages()
  txt  <- do.call(glue::glue, c(list(msgs$templates[[template_key]]), glue_env))
  if (isTRUE(add_supportive)) {
    txt <- paste0(txt, "\n\n", msgs$support$consecutive_failures_paragraph)
  }
  if (isTRUE(add_congrats)) {
    txt <- paste0(txt, "\n\n", msgs$support$consecutive_success_paragraph)
  }
  txt
}