#' @title File System and Participant Metadata Helpers
#'
#' @description
#' These internal helper functions manage the application's directory structure,
#' resolve OS-specific paths (Windows vs. Unix), and handle the reading and
#' writing of basic participant metadata (ID and name).
#'
#' @keywords internal filesystem metadata persistence
#' @noRd
NULL

# Path resolution constants (defined in R/constants.R) 
# DEFAULT_PATH_WINDOWS, DEFAULT_PATH_UNIX

#' Get OS-specific Base Directory
#'
#' @description
#' Determines the appropriate base directory for shared data depending on the
#' operating system (Windows or Unix).
#'
#' @return Character string of the base directory path.
#' @noRd
get_base_dir <- function() {
  if (.Platform$OS.type == "windows") DEFAULT_PATH_WINDOWS else DEFAULT_PATH_UNIX
}

#' Get Root Output Directory
#'
#' @description
#' Constructs the root path where all processed and output files (Fitbit, Matrix, etc.)
#' will be stored under the standard "StepIn_Wearable/output" hierarchy.
#'
#' @return Character string of the root output directory path.
#' @noRd
get_output_root <- function() file.path(get_base_dir(), "StepIn_Wearable", "output")

#' Get Fitbit Output Root Directory
#' @noRd
path_fitbit_root    <- function() file.path(get_output_root(), "fitbit")

#' Get StepWatch Output Root Directory
#' @noRd
path_stepwatch_root <- function() file.path(get_output_root(), "stepwatch")

#' Get Matrix Output Root Directory
#' @noRd
path_matrix_root    <- function() file.path(get_output_root(), "matrix")

#' Get Summary Output Root Directory
#' @noRd
path_summary_root   <- function() file.path(get_output_root(), "summary")

#' Get Participant Metadata Path
#'
#' @description
#' Constructs the file path for saving/loading participant name metadata
#' (\code{participant_info.rds}) within the participant's dedicated Fitbit directory.
#'
#' @param id Character ID of the participant.
#' @return Character string of the metadata file path.
#' @noRd
participant_meta_path <- function(id) file.path(path_fitbit_root(), id, "participant_info.rds")


#' Null Coalescing Operator
#'
#' @description
#' A simplified operator that returns the value of \code{x} if it is not \code{NULL}
#' and not \code{NA}; otherwise, it returns the value of \code{y}.
#'
#' @param x A value to test.
#' @param y The fallback value.
#' @return Either \code{x} or \code{y}.
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x



#' Read Participant Name from Metadata File
#'
#' @description
#' Reads the participant's display name from their metadata file. Handles
#' file non-existence and potential errors gracefully, defaulting to \code{NA}.
#'
#' @param id Character ID of the participant.
#' @return Character string of the participant's name, or \code{NA_character_} if not found or on error.
#' @noRd
read_participant_name <- function(id) {
  p <- participant_meta_path(id)
  if (!file.exists(p)) return(NA_character_)
  tryCatch({
    info <- readRDS(p)
    nm <- info$name %||% info$participant_name %||% NA_character_
    if (is.null(nm)) NA_character_ else as.character(nm)
  }, error = function(e) NA_character_)
}


#' Write Participant Name to Metadata File
#'
#' @description
#' Saves the display name associated with a participant ID to their metadata file.
#' Creates the necessary directory structure if it does not exist.
#'
#' @param id Character ID of the participant.
#' @param name Character string of the participant's name.
#' @noRd
write_participant_name <- function(id, name) {
  dir.create(file.path(path_fitbit_root(), id), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(id = id, name = name, updated = Sys.time()), participant_meta_path(id))
}

#' Scan for Existing Participant IDs
#'
#' @description
#' Scans the Fitbit, StepWatch, and Matrix output directories to identify all
#' unique participant IDs that have processed data subdirectories.
#'
#' @return Sorted, unique character vector of existing participant IDs.
#' @noRd
scan_existing_ids <- function() {
  ids <- character(0)
  if (dir.exists(path_fitbit_root())) {
    fb_dirs <- list.dirs(path_fitbit_root(), full.names = TRUE, recursive = FALSE)
    ids <- c(ids, basename(fb_dirs))
  }
  if (dir.exists(path_stepwatch_root())) {
    sw_dirs <- list.dirs(path_stepwatch_root(), full.names = TRUE, recursive = FALSE)
    ids <- c(ids, basename(sw_dirs))
  }
  if (dir.exists(path_matrix_root())) {
    mx_dirs <- list.dirs(path_matrix_root(), full.names = TRUE, recursive = FALSE)
    if (length(mx_dirs)) ids <- c(ids, basename(mx_dirs))
  }
  sort(unique(ids[nzchar(ids)]))
}

#' Ensure Necessary Output Directories Exist
#'
#' @description
#' Ensures the required directory structure exists for a new or existing participant ID,
#' creating directories recursively if necessary.
#'
#' @param id Character ID of the participant.
#' @return A list containing the normalized paths to the processed directories for
#'         Fitbit, Matrix, StepWatch, and the final summary location.
#' @noRd
ensure_dirs_for_id <- function(id) {
  out_root <- get_output_root()
  dirs <- c(
    file.path(out_root, "fitbit", id),
    file.path(out_root, "matrix"),
    file.path(out_root, "stepwatch", id),
    file.path(out_root, "summary")
  )
  invisible(lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE)))
  list(
    processed_fitbit_dir    = file.path(out_root, "fitbit", id),
    processed_matrix_dir    = file.path(out_root, "matrix"),
    processed_stepwatch_dir = file.path(out_root, "stepwatch", id),
    final_dir               = file.path(out_root, "summary")
  )
}