#' Resolve a file path within the installed package
#'
#' Thin wrapper around [base::system.file()] that always uses the
#' `stepinWearable` package. Useful for locating resources shipped under
#' `inst/` after installation (e.g., `inst/app/www/...`).
#'
#' @param ... Character vectors giving path components inside the package.
#'   Components are combined using the platform file separator.
#' @param mustWork Logical; if `TRUE` (default) an error is thrown if the file
#'   cannot be found. If `FALSE`, an empty string (`""`) is returned when not found.
#'
#' @return A length-1 character string with the absolute path to the requested
#'   file, or `""` if not found and `mustWork = FALSE`.
#'
#' @seealso [base::system.file()]
#'
#' @examples
#' # Lookup a resource (returns "" if not present):
#' app_sys("app", "www", "_brand.yml", mustWork = FALSE)
#'
#' @export
app_sys <- function(..., mustWork = TRUE) {
  system.file(..., package = "stepinWearable", mustWork = mustWork)
}