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
#' @param force_steps_factor Numeric factor manually selected in the UI to
#' override the automatically recommended steps factor.
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
#' Logic phases are: (1) post_basal (t = 0) sets the initial step goal (X) based on 
#' baseline assessment data; (2) m1_3 (t: 1:4) focuses on achieving step accumulation goals (X);
#' (3) init_m4 (t = 5) introduces the intensity goal (Y minutes at a Z cadence);
#' (4) m4_9 (t >= 6) evaluates both step accumulation and intensity goals, allowing for escalation.
#'
#' @param state A list representing the participant's historical intervention state (from \code{load_participant_state}), including \code{history}, \code{last_X}, \code{last_Y}, \code{last_Z}, and \code{consecutive_fails}.
#' @param cur_k A list of KPIs calculated for the current 2-week window.
#' @param prev_k A list of KPIs calculated for the previous 2-week window (or \code{NULL}).
#' @param nombre Character string for the participant's name (used for message text).
#' @param steps_factor Numeric factor used to increase the step goal \code{X} upon success (e.g., 1.05 for +5\%).
#' @param t Integer index of the current 2-week review period (14-day window).
#'   t = 0 corresponds to the first 14-day recording processed to set the initial targets and is considered part of the intervention.
#' @param force_Z Integer to manually override the assigned cadence target \code{Z}.
#' @param force_Y Integer to manually override the assigned minute target \code{Y}.
#'
#' @returns A list containing:
#' \itemize{
#'   \item \code{key}: The message template key chosen (e.g., "ambos3").
#'   \item \code{text}: The final rendered message text (Spanish).
#'   \item \code{next_X}, \code{next_Y}, \code{next_Z}: The new targets to be saved for the next cycle.
#'   \item \code{consecutive_fails}: Updated failure streak count (used for supportive message inclusion).
#' }
#'
#' @importFrom dplyr case_when
#' @importFrom glue glue
#' @export
decide_message <- function(state, cur_k, prev_k, nombre,
                           steps_factor = 1.05, t,
                           force_Z = NULL,
                           force_Y = NULL,
                           force_steps_factor = NULL) {
  # load messages
  msgs <- load_messages()  # <-- runtime
  message_templates <- msgs$templates
  
  # Phase is inferred from t-index that is being processed (confirmed by user)
  # Each t refers to one consecutive 2-week review (14-day window) used for KPI computation and target updates.
  # t = 0 is the first processed 2-week recording used to generate the first target (already part of the intervention).
  # 0 -> post-basal; 1..4 -> months 1-3; 5 -> start of month 4 (init cadence); >=6 -> months 4-9.
  phase <- dplyr::case_when(
    t == 0          ~ "post_basal",
    t >= 1 & t <= 4 ~       "m1_3",
    t == 5          ~    "init_m4",
    TRUE            ~       "m4_9"
  )
  
  # Previous targets (if any)
  prev_X <- state$last_X
  prev_Y <- state$last_Y
  prev_Z <- state$last_Z
  last_steps_factor <- state$last_steps_factor
  last_minutes_inc <- state$last_minutes_inc
  
  has_prev_X <- !is.null(prev_X) && is.finite(prev_X) && t > 0
  has_prev_Y <- !is.null(prev_Y) && is.finite(prev_Y) && prev_Y > 0 && t > 5
  has_prev_Z <- !is.null(prev_Z) && is.finite(prev_Z) && t > 5
  
  # Z override from UI (force_Z). Robust to NULL -> integer(0)
  forceZ <- suppressWarnings(as.integer(force_Z))
  has_forceZ <- length(forceZ) == 1 &&
    !is.na(forceZ) &&
    forceZ %in% c(80L, 90L, 100L, 110L, 120L)
  
  # Optional manual override of the steps factor
  forced_steps_factor <- suppressWarnings(
    as.numeric(force_steps_factor)
  )
  
  has_forced_steps_factor <-
    length(forced_steps_factor) == 1L &&
    is.finite(forced_steps_factor) &&
    forced_steps_factor > 0
  
  applied_steps_factor <- if (has_forced_steps_factor) {
    forced_steps_factor
  } else {
    steps_factor
  }
  
  # Steps improvement flag (>=(X-1)% vs. previous window)
  steps_ok <- if (!is.null(prev_k) && nrow(prev_k) == 1) {
    steps_met(cur_k$med_steps_day, prev_X, last_steps_factor)
  } else NA
  
  # Minutes improvement flag
  cur_minutes_at_prevZ <- dplyr::case_when(
    has_prev_Z && prev_Z == 120 ~ cur_k$med_steps_120plus,
    has_prev_Z && prev_Z == 110 ~ cur_k$med_steps_110plus,
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
  
  # Check baseline for +5000 rule in m4_9 (t >= 6)
  if (phase == "m4_9") {
    baseline_steps <- NA_real_
    if (length(state$history) > 0) {
      for (h in state$history) {
        if (!is.null(h$t_index) && h$t_index == 0) {
          baseline_steps <- h$kpis$med_steps_day
          break
        }
      }
    }
    
    # If baseline is found and we have a previous target to fall back to
    if (!is.na(baseline_steps) && has_prev_X) {
      # If current steps OR the previous target have reached the +5000 limit
      if (cur_k$med_steps_day >= (baseline_steps + 5000) || prev_X >= (baseline_steps + 5000)) {
        # At the ceiling, always use the previously deployed target as the base
        base_X <- prev_X        # Freeze to the last deployed target
        # Automatic generation, or a manual factor >= 1, must not increase X
        if (
          !has_forced_steps_factor ||
          applied_steps_factor >= 1.0
        ) {
          applied_steps_factor <- 1.0
        }
      }
    }
  }
  
  next_X <- round((base_X * applied_steps_factor)/10)*10 # round to tens
  
  # Z: introduce on init_m4 (t = 5), maybe escalate on m4_9, otherwise keep
  if (phase == "init_m4" && !has_prev_Z) {
    next_Z <- dplyr::case_when(
      cur_k$med_steps_110plus >= 45 ~ 120L,
      cur_k$med_steps_100plus >= 45 ~ 110L,
      cur_k$med_steps_100plus >= 5 ~ 100L,
      cur_k$med_steps_90plus  >= 15 ~ 100L,
      cur_k$med_steps_90plus  >= 5 ~ 90L,
      cur_k$med_steps_80plus  >= 15 ~  90L,
      TRUE                         ~  80L
    )
    
    # Z from slider: override cadence target if provided
    if (has_forceZ) next_Z <- forceZ
    
    # start intensity with next of currently accumulated after
    # rounding the median minutes to the closer 5 and adding 5 minutes
    
    # Y target (init_m4): base on achieved minutes at the assigned cadence (next_Z)
    varname   <- paste0("med_steps_", next_Z, "plus")
    base_mins <- cur_k[[varname]]
    if (is.null(base_mins) || is.na(base_mins)) base_mins <- 0
    base_mins <- as.numeric(base_mins)
    
    # AUTO (protocol): round to 5 and add +5
    next_Y_auto <- as.integer((floor((base_mins / 5) + 0.5) * 5) + 5)
    
    # FORCED (slider): absolute target
    y_force <- suppressWarnings(as.integer(force_Y))
    if (length(y_force) == 1 && !is.na(y_force)) {
      next_Y <- y_force
    } else {
      next_Y <- next_Y_auto
    }
    
    # Guardrails
    next_Y <- max(0L, next_Y)
    
    # ABSOLUTE MAX CAP: 45 minutes at 120 steps/min
    if (!is.na(next_Z) && next_Z >= 120L && next_Y > 45L) {
      next_Y <- 45L
    }
    
    # Store the *real increment* applied 
    minutes_inc <- as.integer(next_Y - base_mins)
    
  } else {
    # default keep same Z
    next_Z <- if (has_prev_Z) as.integer(prev_Z) else NA_integer_
    
    # Z escalation on m4_9
    escalate <- FALSE
    if (phase == "m4_9" && has_prev_Z) {
      if (prev_Z == 110 && should_escalate_from_110(cur_k$med_steps_110plus)) {
        next_Z <- 120L; escalate <- TRUE
      } else if (prev_Z == 100 && should_escalate_from_100(cur_k$med_steps_100plus)) {
        next_Z <- 110L; escalate <- TRUE
      } else if (prev_Z == 90 && should_escalate_from_90(cur_k$med_steps_90plus)) {
        next_Z <- 100L; escalate <- TRUE
      } else if (prev_Z == 80 && should_escalate_from_80(cur_k$med_steps_80plus)) {
        next_Z <- 90L;  escalate <- TRUE
      }
    }
    
    # Z from slider: override cadence target if provided
    if (has_forceZ) {
      if (has_prev_Z && forceZ != as.integer(prev_Z)) escalate <- TRUE
      next_Z <- forceZ
    }
    
    # Y progression (m4_9): compute from achieved minutes
    cur_minutes_at_nextZ <- dplyr::case_when(
      !is.na(next_Z) && next_Z == 120 ~ cur_k$med_steps_120plus,
      !is.na(next_Z) && next_Z == 110 ~ cur_k$med_steps_110plus,
      !is.na(next_Z) && next_Z == 100 ~ cur_k$med_steps_100plus,
      !is.na(next_Z) && next_Z ==  90 ~ cur_k$med_steps_90plus,
      !is.na(next_Z) && next_Z ==  80 ~ cur_k$med_steps_80plus,
      TRUE ~ NA_real_
    )
    
    # base minutes depend on whether Z changed (escalate) or not
    base_mins <- if (escalate || !has_prev_Y) cur_minutes_at_nextZ else cur_minutes_at_prevZ
    
    # Optional override: slider sets absolute target Y
    y_force <- suppressWarnings(as.integer(force_Y))
    has_y <- length(y_force) == 1 && !is.na(y_force)
    
    if (has_y) {
      next_Y <- max(0L, y_force)
    } else if (!isTRUE(escalate) && has_prev_Y && !isTRUE(mins_ok)) {
      next_Y <- as.integer(prev_Y)
    } else {
      if (is.na(base_mins)) base_mins <- 0
      
      # Default target from achieved minutes
      next_Y <- as.integer((floor((base_mins / 5) + 0.5) * 5) + 5)
      next_Y <- max(0L, next_Y)
    }
    
    # ABSOLUTE MAX CAP: 45 minutes at 120 steps/min
    if (!is.na(next_Z) && next_Z >= 120L && next_Y > 45L) {
      next_Y <- 45L
    }
    
    # Store the increment that was effectively applied
    minutes_inc <- as.integer(next_Y - base_mins)
    
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
    (phase == "init_m4" & !isTRUE(steps_ok)) ||
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
    steps_factor = applied_steps_factor,
    consecutive_fails = new_consecutive_fails,
    consecutive_success = new_consecutive_success,
    steps_met = steps_ok, cadence_met = mins_ok, minutes_inc = minutes_inc
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