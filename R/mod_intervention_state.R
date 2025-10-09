# ============================
#       Persistence helpers
# ============================

#' State Persistence Helpers
#'
#' @description
#' Helper functions for loading and saving the participant's intervention state
#' (targets, history, and failure streaks). These files are stored as serialized
#' R objects (\code{.rds}).
#'
#' @keywords internal persistence state
#' @noRd
NULL

#' Build the Participant State File Path
#'
#' @description
#' Constructs the canonical file path for a participant's intervention state
#' file (\code{<participant_id>_state.rds}) within the processed output directory.
#'
#' @param outdir Character string specifying the processed output directory.
#' @param participant_id Character ID of the participant.
#'
#' @return A character string representing the full file path.
#' @noRd
.participant_state_path <- function(outdir, participant_id) {
  file.path(outdir, paste0(participant_id, "_state.rds"))
}

#' Load Per-Participant Intervention State
#'
#' Reads the participant's intervention **state file** from `outdir`. If no state
#' file exists yet, a new state object with sensible defaults is created and returned.
#'
#' @param outdir Character scalar. Directory that holds the participant's state file
#'   (one state file per participant).
#' @param participant_id Character scalar. Participant ID.
#'
#' @return A named list with the current participant state:
#' \describe{
#'   \item{history}{List (possibly empty) of past intervention entries, one per \emph{t}-index.
#'     See \emph{History entry fields} below for the structure of each entry.}
#'   \item{last_X}{Numeric. Last saved target for \strong{daily steps} (X).}
#'   \item{last_Y}{Integer. Last saved target for \strong{minutes/day} at cadence Z (Y).}
#'   \item{last_Z}{Integer. Last saved \strong{cadence threshold} (Z), one of 80, 90, or 100.}
#'   \item{consecutive_fails}{Integer. Current streak of consecutive failed cycles.}
#' }
#'
#' @section History entry fields:
#' Each element of `history` is a named list with:
#' \describe{
#'   \item{t_index}{Integer. Cycle index \eqn{t} (0 = post-basal, 1..7 = months 1–3, 8+ = months 4–9).}
#'   \item{date_str}{Character. Start date (YYYYMMDD) of the corresponding minute series.}
#'   \item{start_date}{Date. First day of the 14-day analysis window used for this entry.}
#'   \item{end_date}{Date. Last day of the 14-day analysis window used for this entry.}
#'   \item{kpis}{Named list of window medians used by the decision engine, typically:
#'     `med_steps_day`, `med_steps_80plus`, `med_steps_90plus`, `med_steps_100plus`.}
#'   \item{steps_factor}{Numeric. Multiplication factor applied when computing the next X.}
#'   \item{minutes_inc}{Integer. Minute increment applied when computing/adjusting Y.}
#'   \item{target_steps}{Numeric. Next \strong{X} target (steps/day).}
#'   \item{target_minutes}{Integer. Next \strong{Y} target (minutes/day at cadence Z).}
#'   \item{target_cadence}{Integer. Next \strong{Z} target (one of 80, 90, 100 steps/min).}
#'   \item{auto_message_key}{Character. Template key chosen by the engine
#'     (e.g., `"msg0"`, `"pasos1"`, ..., `"ambos8"`, `"nodata3"`).}
#'   \item{final_message_key}{Character. Template key finally used/shown (may differ if overridden).}
#'   \item{manual_override}{Logical. `TRUE` iff `final_message_key` differs from `auto_message_key`.}
#'   \item{message}{Character. Fully rendered WhatsApp message text stored for audit.}
#' }
#'
#' @details
#' The state is updated by the Intervention module after you click **Save Intervention State**.
#' It persists targets (X, Y, Z), the automatic/overridden template key, and the rendered
#' message together with the KPIs computed on the selected 14-day window.
#'
#' @seealso \code{save_participant_state()}, \code{decide_message()}, \code{kpis()}
#' @noRd
load_participant_state <- function(outdir, participant_id) {
  p <- .participant_state_path(outdir, participant_id)
  if (file.exists(p)) readRDS(p) else list(
    history = list(),                # list of past 2-week review summaries
    last_X = NA_integer_,            # last daily steps target
    last_Y = 0L,                     # last minutes/day at Z target
    last_Z = NA_integer_,            # last cadence target (80/90/100)
    last_steps_factor = NA,
    last_minutes_inc = NA,
    consecutive_fails = 0L           # consecutive review failures
  )
}

#' Save Per-Participant Intervention State
#'
#' @description
#' Saves the provided state list to the canonical participant state file
#' in the specified output directory. Creates the directory structure if it does not exist.
#'
#' @param state A list object containing the participant's intervention state.
#' @param outdir Character string specifying the processed output directory.
#' @param participant_id Character ID of the participant.
#'
#' @return Invisible \code{NULL}. The primary effect is writing the state file to disk.
#' @noRd
save_participant_state <- function(state, outdir, participant_id) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(state, .participant_state_path(outdir, participant_id))
}