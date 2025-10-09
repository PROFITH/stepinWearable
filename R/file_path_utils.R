#' Robust Path Normalization with UNC Support
#'
#' Attempts to normalize a given file path, ensuring forward slashes (`/`)
#' are used across all platforms, and importantly, includes robustness for
#' Universal Naming Convention (UNC) paths used in Windows network shares.
#'
#' If `normalizePath()` fails (which can occur with UNC paths like
#' `\\server\share` if not configured correctly, or if `path` is not recognized
#' as valid), the original path is returned unchanged within the `tryCatch` block.
#' This prevents the application from crashing during path validation.
#'
#' @param path A character string representing a file path (can be a standard path or a UNC network path).
#'
#' @return A character string representing the normalized path (using forward slashes), or the original path if normalization fails.
#'
#' @keywords internal path UNC
#' @noRd
normalize_unc <- function(path) {
  tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
           error = function(e) path)
}