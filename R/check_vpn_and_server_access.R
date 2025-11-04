#' Check VPN and Shared Folder Access
#'
#' This function verifies whether the user is connected to a VPN by attempting to 
#' ping a known server and checks access to a shared network folder.
#'
#' @param server_host Character, server host or IP to check.
#' @param path_test Optional. A file path to test access to a shared network volume.
#' If `NULL`, a platform-specific default path is used internally.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{`vpn_connected`}{Logical. `TRUE` if the VPN connection is detected via a successful ping to the server.}
#'   \item{`server_accessible`}{Logical. `TRUE` if the specified (or default) shared folder is accessible.}
#'   \item{`message`}{Character string. A user-friendly message indicating the result of the check, or `NULL` if no issues are found.}
#' }
#'
#' @details
#' The function uses a `ping` command to check connectivity to a known server, typically only accessible via VPN.
#' It also tests whether a shared network path is accessible using `file.exists()`. This can help diagnose 
#' network or VPN-related access issues when working with remote resources.
#'
#' @examples
#' # Run the check using a default shared folder path
#' check_vpn_and_server_access()
#'
#' # Run the check with a custom folder path
#' check_vpn_and_server_access("/your/custom/path")
#'
#' @export
check_vpn_and_server_access <- function(server_host = NULL, path_test = NULL) {
  if (is.null(server_host)) server_host <- DEFAULT_SERVER_HOST
  
  if (is.null(path_test)) {
    if (.Platform$OS.type == "windows") {
      path_test <- DEFAULT_PATH_WINDOWS
    } else {
      path_test <- DEFAULT_PATH_UNIX
    }
  }
  
  ping_cmd <- if (.Platform$OS.type == "windows") {
    paste("ping -n 1", server_host)
  } else {
    paste("ping -c 1", server_host)
  }
  
  ping_result <- suppressWarnings(system(ping_cmd, intern = TRUE, ignore.stderr = TRUE))
  suppressWarnings({
    vpn_connected <- any(grepl("TTL=", ping_result)) || any(grepl("bytes from", ping_result))
  })
  server_accessible <- file.exists(path_test)
  
  result <- list(
    vpn_connected = vpn_connected,
    server_accessible = server_accessible,
    message = if (!vpn_connected) {
      paste0("ot connected to UGR VPN (connect for using the app)")
    } else if (!server_accessible) {
      paste0("annot access shared folder at: ", path_test)
    } else {
      NULL
    }
  )
  
  return(result)
}

