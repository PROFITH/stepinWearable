#' Preprocess Fitbit data (minute-level Steps)
#'
#' Reads Fitbit downloaded files (Excel or CSV), filters for 'Steps' data,
#' handles various timestamp formats (character, Excel serial, POSIXct),
#' and aggregates data to ensure a single step count per minute.
#'
#' The function performs robust time zone handling by ensuring the output
#' timestamp reflects the local clock time of the participant in the specified
#' timezone (`tz`).
#'
#' @param file_path Path to the Fitbit file (.xlsx/.xls or .csv). The function uses `readxl::read_excel` internally, which handles both Excel and CSV files.
#' @param tz Timezone that represents the participant's local clock (e.g., "Europe/Madrid" or "America/New_York"). This is the timezone assigned to the final 'timestamp' column.
#' @param timestamps_are_local Logical.
#'   \itemize{
#'     \item \code{TRUE} (default): Assumes the datetimes in the input file are **local clock times** (most common in Fitbit exports). The timezone is assigned without shifting the clock.
#'     \item \code{FALSE}: Assumes the datetimes in the input file represent **UTC instants**. The clock time is shifted to match the local timezone (\code{tz}).
#'   }
#' @return A \code{tibble} with two columns:
#'   \itemize{
#'     \item \code{timestamp} (POSIXct, \code{tz} is set to the value of the \code{tz} argument).
#'     \item \code{steps} (numeric, aggregated to sum if multiple entries exist for the same minute).
#'   }
#' @importFrom readxl read_excel
#' @importFrom lubridate parse_date_time force_tz with_tz
#' @importFrom dplyr filter arrange group_by summarise
#' @importFrom tibble tibble
#' @export
preprocess_fitbit <- function(file_path,
                              tz = "Europe/Madrid",
                              timestamps_are_local = TRUE) {
  
  message("Processing Fitbit file: ", file_path)
  stopifnot(file.exists(file_path))
  
  # ---- 1) Read file (Excel or CSV) ----
  data <- readxl::read_excel(file_path)
  
  # Required columns
  req_cols <- c("MeasurementType", "MeasurementDateTime", "MeasurementValue")
  if (!all(req_cols %in% names(data))) {
    stop("Input file must contain columns: ",
         paste(req_cols, collapse = ", "))
  }
  
  # ---- 2) Keep only Steps; rename to (timestamp_raw, steps) ----
  d <- data[data$MeasurementType == "Steps",
            c("MeasurementDateTime", "MeasurementValue")]
  if (nrow(d) == 0) stop("No 'Steps' rows found in file.")
  names(d) <- c("timestamp_raw", "steps")
  
  # ---- 3) Parse datetimes robustly ----
  v <- d$timestamp_raw
  
  parse_char <- function(x, tz_local, local_clock) {
    # Try common day-month / ISO layouts. Add more if your export varies.
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        x,
        orders = c("d-m-Y H:M:S", "d-m-Y H:M",
                   "Y-m-d H:M:S", "Y-m-d H:M",
                   "d/m/Y H:M:S", "d/m/Y H:M",
                   "Y/m/d H:M:S", "Y/m/d H:M"),
        tz = if (local_clock) tz_local else "UTC"
      )
    )
    if (local_clock) parsed else lubridate::with_tz(parsed, tz_local)
  }
  
  # Excel serial to POSIXct in local tz, *without* moving the clock
  parse_excel_serial <- function(x, tz_local) {
    # Excel origin is 1899-12-30
    as.POSIXct(x * 86400, origin = "1899-12-30", tz = tz_local)
  }
  
  ts <- if (inherits(v, "POSIXct")) {
    if (timestamps_are_local) {
      lubridate::force_tz(v, tzone = tz)  # assign tz, do not shift
    } else {
      lubridate::with_tz(v, tzone = tz)   # convert UTC -> local
    }
  } else if (inherits(v, "Date")) {
    # If you only had dates, set midnight local
    as.POSIXct(v, tz = tz)
  } else if (is.numeric(v)) {
    # Excel serial numbers
    parse_excel_serial(as.numeric(v), tz_local = tz)
  } else {
    # Character datetimes
    parse_char(as.character(v), tz_local = tz, local_clock = timestamps_are_local)
  }
  
  # ---- 4) Clean and aggregate ----
  d <- tibble::tibble(timestamp = ts, steps = suppressWarnings(as.numeric(d$steps))) %>%
    dplyr::filter(!is.na(timestamp), !is.na(steps)) %>%
    dplyr::arrange(timestamp)
  
  # Remove exact duplicate rows
  d <- d[!duplicated(d), ]
  
  # Ensure one row per minute: if duplicates per same timestamp exist, sum them
  d <- d %>%
    dplyr::group_by(timestamp) %>%
    dplyr::summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(timestamp)
  
  # Coerce steps to numeric (integer-like)
  d$steps <- as.numeric(d$steps)
  
  # Safety: ensure the clock time is stamped as local (no clock shift)
  d$timestamp <- lubridate::force_tz(d$timestamp, tzone = tz)
  
  d
}
