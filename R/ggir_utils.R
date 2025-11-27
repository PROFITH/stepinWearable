#' Convert Sleep Diary to GGIR Sleep Log Format (Wide Format)
#'
#' @description
#' Converts a diary of sleep segments (`start`/`end`, POSIXct) into the specific
#' **wide format** required for GGIR sleep log files. The output table is anchored
#' by the date prior to sleep onset (noon anchoring), and includes imputed
#' values for the first wakeup and last sleep onset to span the entire observation period.
#'
#' **Key Assumptions:**
#' \itemize{
#'   \item The log assumes **at most one sleep segment per anchored date** (date matched to noon prior to onset); if multiple exist, only the earliest onset per date is kept.
#'   \item The log imputes the **first wakeup time** to "06:00:00" for the first log entry.
#'   \item The log imputes a **final sleep onset time** of "23:30:00" for the final date entry.
#' }
#'
#' @param diary A data frame with columns `start` and `end` (both must be `POSIXct`).
#' @param participant_id Character ID written to the `ID` column.
#' @param tz Time zone for formatting the times (default: system time zone). The times in the output file are clock times (\code{hh:mm:ss}).
#'
#' @importFrom plyr rbind.fill
#' @return The function **invisibly returns** the wide data frame compatible with GGIR, which includes one row per participant and multiple columns for each date-wakeup-sleeponset triplet (\code{date1}, \code{wakeup1}, \code{sleeponset1}, etc.). It also saves this data to the path specified in \code{file}.
#' @examples
#' \dontrun{
#' diary <- data.frame(
#'    start = as.POSIXct(c("2025-07-10 23:15:00","2025-07-12 00:40:00")),
#'    end   = as.POSIXct(c("2025-07-11 06:30:00","2025-07-12 07:05:00"))
#' )
#' # Returns a wide data frame and saves it to the specified path
#' log_file <- ggir_sleep_log(diary, "P001")
#' }
#' @export
ggir_sleep_log <- function(diary, participant_id = "test", tz = Sys.timezone()) {
  stopifnot(all(c("start", "end") %in% names(diary)))
  d <- diary[!is.na(diary$start) & !is.na(diary$end) & diary$start < diary$end, , drop = FALSE]
  if (nrow(d) == 0) {
    return(data.frame(ID = character(), date = as.Date(character()),
                      wakeup = character(), sleeponset = character(),
                      stringsAsFactors = FALSE))
  }
  
  # order by start
  d <- d[order(d$start), , drop = FALSE]
  
  # date anchored at noon prior to onset: date = as.Date(onset - 12h)
  noon_prior_date <- as.Date(d$start - 12 * 3600, tz = tz)
  
  # Keep only the earliest onset per date (if duplicates)
  keep_idx <- !duplicated(noon_prior_date)
  d        <- d[keep_idx, , drop = FALSE]
  noon_prior_date <- noon_prior_date[keep_idx]
  noon_prior_date <- c(noon_prior_date, max(noon_prior_date) + 1)
  
  # sleeponset time for each date (clock time only)
  onset_time_chr <- format(as.POSIXct(d$start, tz = tz), "%H:%M:%S")
  
  # wakeup for a date = previous night's end time (shift by 1); first gets default 06:00
  wake_shifted   <- c(NA, d$end)
  wake_time_chr  <- format(as.POSIXct(wake_shifted, tz = tz), "%H:%M:%S")
  # if (is.na(wake_time_chr[1])) wake_time_chr[1] <- "06:00:00"
  if (length(wake_shifted) > length(onset_time_chr)) {
    onset_time_chr = c(onset_time_chr, NA)
  }
  
  long <- data.frame(
    ID         = rep(as.character(participant_id), length(noon_prior_date)),
    date       = noon_prior_date,
    wakeup     = wake_time_chr,
    sleeponset = onset_time_chr,
    stringsAsFactors = FALSE
  )
  
  long = long[order(long$date), , drop = FALSE]
  
  # ---- wide reshaping ----
  n <- nrow(long)
  pieces <- lapply(seq_len(n), function(i) {
    nm <- paste0(c("date", "wakeup", "sleeponset"), i)
    vals <- list(
      as.character(long$date[i]),
      long$wakeup[i],
      long$sleeponset[i]
    )
    stats::setNames(vals, nm)
  })
  flat <- unlist(pieces, recursive = FALSE, use.names = TRUE)
  wide <- as.data.frame(as.list(flat), stringsAsFactors = FALSE, optional = TRUE)
  rownames(wide) <- NULL
  wide <- cbind(ID = participant_id, wide, stringsAsFactors = FALSE)
  return(wide)
}
