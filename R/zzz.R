#' # R/zzz.R
#' 
#' @noRd
.onLoad <- function(libname, pkgname) {
  read_pkg_cfg <- function() {
    p <- system.file("config.yml", package = pkgname)
    if (!nzchar(p) || !file.exists(p)) return(list())
    yaml::read_yaml(p)
  }
  
  cfg <- tryCatch(read_pkg_cfg(), error = function(e) list())
  
  # 1) Safe, neutral defaults (public GitHub)
  defaults <- list(
    stepin.server_host     = "localhost",
    stepin.path_windows    = "",
    stepin.path_unix       = "",
    stepin.output_dir      = tools::R_user_dir("stepinWearable", "data"),
    stepin.fitbit_download = ""  # if empty we’ll derive below from output_dir
  )
  
  # 2) From config.yml (if present in the installed package)
  from_cfg <- list(
    stepin.server_host     = cfg$server_host,
    stepin.path_windows    = cfg$paths$windows,
    stepin.path_unix       = cfg$paths$unix,
    stepin.output_dir      = cfg$paths$output_dir,
    stepin.fitbit_download = cfg$paths$fitbit_download
  )
  
  # 3) From environment (takes precedence if set)
  from_env <- list(
    stepin.server_host     = Sys.getenv("STEPIN_SERVER_HOST",     ""),
    stepin.path_windows    = Sys.getenv("STEPIN_PATH_WINDOWS",    ""),
    stepin.path_unix       = Sys.getenv("STEPIN_PATH_UNIX",       ""),
    stepin.output_dir      = Sys.getenv("STEPIN_OUTPUT_DIR",      ""),
    stepin.fitbit_download = Sys.getenv("STEPIN_FITBIT_DOWNLOAD", "")
  )
  
  # Compose in order: defaults -> cfg -> env (non-empty only)
  op <- defaults
  for (nm in names(from_cfg)) if (!is.null(from_cfg[[nm]])) op[[nm]] <- from_cfg[[nm]]
  for (nm in names(from_env)) if (nzchar(from_env[[nm]]))   op[[nm]] <- from_env[[nm]]
  
  # Derive fitbit_download if empty but output_dir is set
  if (!nzchar(op$stepin.fitbit_download) && nzchar(op$stepin.output_dir)) {
    op$stepin.fitbit_download <- file.path(op$stepin.output_dir, "download", "fitbit")
  }
  
  # Set options only if currently unset
  toset <- setdiff(names(op), names(options()))
  if (length(toset)) options(op[toset])
  
  # Active bindings (guard against redefinition)
  ns <- asNamespace(pkgname)
  bind <- function(sym, fn) {
    if (!exists(sym, envir = ns, inherits = FALSE) || !bindingIsActive(sym, ns)) {
      makeActiveBinding(sym, fn, ns)
    }
  }
  bind("DEFAULT_SERVER_HOST",  function() getOption("stepin.server_host"))
  bind("DEFAULT_PATH_WINDOWS", function() getOption("stepin.path_windows"))
  bind("DEFAULT_PATH_UNIX",    function() getOption("stepin.path_unix"))
  bind("WEARABLE_OUTPUT_DIR",  function() getOption("stepin.output_dir"))
  bind("FITBIT_DOWNLOAD_DIR",  function() getOption("stepin.fitbit_download"))
}
