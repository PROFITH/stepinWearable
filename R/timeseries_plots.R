#' Plot a Time Series with Optional Diary Overlay
#'
#' This function creates a time series plot of a numeric signal (`y`) over time (`x`)
#' with optional overlay of diary-based activity periods (e.g., sleep).
#' You can choose between a static `ggplot2` version or an interactive `plotly` version.
#'
#' @param df A data frame containing the time series data.
#' @param x A string specifying the column name in `df` containing timestamps.
#' @param y A string specifying the column name in `df` containing the numeric signal to plot.
#' @param diary An optional data frame with columns `start_date`, `end_date`, 
#'        `start_time`, `end_time`, and `activity`. Times should be in Excel date format (numeric).
#' @param interactive Logical; if TRUE, return a plotly interactive plot instead of a static ggplot.
#' @param na_action method for handling NA values (not available for now).
#' @param tz Character; timezone.
#' 
#' @return A `ggplot` or `plotly` object showing the time series, and if applicable, shaded diary intervals.
#'
#' @import ggplot2
#' @importFrom plotly plot_ly layout
#' @importFrom GGIR is.ISO8601 iso8601chartime2POSIX
#' @importFrom lubridate force_tz
#' @importFrom dplyr rename
#' @importFrom zoo na.approx
#' @export
timeseries_plots <- function(df, x = "timestamp", y,
                             diary = NULL,
                             interactive = FALSE,
                             na_action = "gap",
                             tz = "") {
  na_action <- match.arg(na_action)
  
  # ---- validate inputs ----
  stopifnot(is.data.frame(df))
  if (!x %in% names(df)) stop("x column not found in df: ", x)
  if (!y %in% names(df)) stop("y column not found in df: ", y)
  
  # ---- convert timestamp column if necessary ----
  ts_vec <- df[[x]]
  # If ISO8601 strings, convert; if POSIXct already, keep.
  if (is.character(ts_vec) && length(ts_vec) > 0 && GGIR::is.ISO8601(ts_vec[1])) {
    df[[x]] <- GGIR::iso8601chartime2POSIX(ts_vec, tz = tz)
  } else if (!inherits(ts_vec, "POSIXt")) {
    # try to coerce via as.POSIXct (best-effort)
    df[[x]] <- as.POSIXct(ts_vec, tz = tz, origin = "1970-01-01")
  }
  
  # ---- prepare y numeric vector and NA handling ----
  y_vec <- as.numeric(df[[y]])    # force numeric (NAs introduced if non-numeric)
  if (na_action == "zero") {
    y_proc <- ifelse(is.na(y_vec), 0, y_vec)
    connect_gaps <- TRUE
  } else if (na_action == "interpolate") {
    # linear interpolation, preserves leading/trailing NAs
    # na.approx requires at least two non-NA values; guard for edge-case
    if (all(is.na(y_vec))) {
      y_proc <- y_vec
    } else {
      # Use index = timestamp numeric as x for interpolation to respect irregular spacing
      idx <- as.numeric(df[[x]])
      # na.approx will keep leading/trailing NA if na.rm = FALSE
      y_proc <- zoo::na.approx(y_vec, x = idx, na.rm = FALSE)
    }
    connect_gaps <- TRUE
  } else { # "gap"
    y_proc <- y_vec
    connect_gaps <- FALSE
  }
  
  # Put processed columns into a plotting df
  plot_df <- data.frame(x_plot = df[[x]], y_plot = y_proc, stringsAsFactors = FALSE)
  
  # compute ymax for diaries (if provided)
  if (!is.null(diary) && nrow(diary) > 0) {
    # Expect diary to already have start_time/end_time as POSIXct; if not, try best-effort conversions
    if (!("start_time" %in% names(diary)) || !("end_time" %in% names(diary))) {
      warning("Diary does not have start_time / end_time columns; skipping diary overlay")
      diary <- NULL
    } else {
      # y-range for rectangles
      ymax_val <- max(plot_df$y_plot, na.rm = TRUE)
      if (!is.finite(ymax_val)) ymax_val <- 1
      diary$ymax_plot <- ymax_val * 1.02
    }
  }
  
  # ---- produce interactive Plotly or ggplot ----
  if (interactive) {
    # plotly scatter line; if connect_gaps = FALSE, keep NAs to show gaps
    plt <- plot_ly(data = plot_df, x = ~x_plot, y = ~y_plot,
                   type = "scatter", mode = "lines",
                   line = list(color = "steelblue"),
                   name = y,
                   connectgaps = connect_gaps,
                   hoverinfo = "x+y")
    
    # overlay diary rectangles as filled polygon traces (no legend)
    if (!is.null(diary) && nrow(diary) > 0) {
      for (i in seq_len(nrow(diary))) {
        xs <- c(diary$start_time[i], diary$end_time[i], diary$end_time[i], diary$start_time[i])
        ys <- c(0, 0, diary$ymax_plot[i], diary$ymax_plot[i])
        plt <- add_trace(plt,
                         x = xs, y = ys,
                         type = "scatter", mode = "none",
                         fill = "toself",
                         fillcolor = "rgba(255,0,0,0.15)",
                         hoverinfo = "none",
                         showlegend = FALSE,
                         inherit = FALSE)
      }
    }
    
    plt <- layout(plt,
                  xaxis = list(type = "date", title = "Time", rangeslider = list(visible = TRUE)),
                  yaxis = list(title = y))
    return(style_plotly(plt))
  } else {
    # ggplot: show gaps by default (NA produces holes). If user chose zero/interpolate,
    # those NA have already been replaced in y_proc.
    p <- ggplot(plot_df, aes(x = x_plot, y = y_plot)) +
      geom_line(color = "steelblue", na.rm = FALSE) +
      theme_minimal() +
      labs(x = "Time", y = y, title = paste0(y, " over time"))
    
    if (!is.null(diary) && nrow(diary) > 0) {
      p <- p + geom_rect(data = diary,
                         aes(xmin = start_time, xmax = end_time, ymin = 0, ymax = ymax_plot, fill = activity),
                         alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE)
    }
    return(p)
  }
}