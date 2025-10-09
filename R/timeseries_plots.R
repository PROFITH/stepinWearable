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
#'
#' @return A `ggplot` or `plotly` object showing the time series, and if applicable, shaded diary intervals.
#'
#' @import ggplot2
#' @importFrom plotly plot_ly layout
#' @importFrom GGIR is.ISO8601 iso8601chartime2POSIX
#' @importFrom lubridate force_tz
#' @importFrom dplyr rename
#' @export
#'
#' @examples
#' # timeseries_plots(df = step_data, x = "timestamp", y = "steps", interactive = TRUE)
timeseries_plots <- function(df, x, y, diary = NULL, interactive = FALSE) {
  # Handle diary conversion from Excel format
  if (!is.null(diary)) {
    diary$start_date <- lubridate::force_tz(
      as.Date(as.numeric(diary$start_date), origin = "1899-12-30"), tz = ""
    )
    diary$end_date <- lubridate::force_tz(
      as.Date(as.numeric(diary$end_date), origin = "1899-12-30"), tz = ""
    )
    diary$start_time <- diary$start_date + (as.numeric(diary$start_time) * 24 * 3600)
    diary$end_time <- diary$end_date + (as.numeric(diary$end_time) * 24 * 3600)
  }
  
  # Convert ISO8601 timestamps if needed
  if (GGIR::is.ISO8601(df[[x]][1])) {
    df[[x]] <- GGIR::iso8601chartime2POSIX(df[[x]], tz = "")
  }
  
  # Ensure numeric signal
  df[[y]] <- as.numeric(df[[y]])
  ymax <- max(df[[y]], na.rm = TRUE)
  
  if (!is.null(diary)) {
    diary$ymax_plot <- ymax * 1.02
  }
  
  df <- dplyr::rename(df, x_plot = !!x, y_plot = !!y)
  
  if (interactive) {
    # Plotly version
    plt <- plotly::plot_ly(
      data = df,
      x = ~x_plot,
      y = ~y_plot,
      type = "scatter",
      mode = "lines",
      line = list(color = "steelblue"),
      name = y
    )
    
    if (!is.null(diary)) {
      for (i in seq_len(nrow(diary))) {
        plt <- plotly::add_trace(
          plt,
          x = c(diary$start_time[i], diary$end_time[i], diary$end_time[i], diary$start_time[i]),
          y = c(0, 0, diary$ymax_plot[i], diary$ymax_plot[i]),
          type = "scatter",
          mode = "none",
          fill = "toself",
          fillcolor = "rgba(255, 0, 0, 0.2)",
          name = diary$activity[i],
          showlegend = FALSE,
          inherit = FALSE
        )
      }
    }
    
    plt <- plotly::layout(
      plt,
      xaxis = list(
        type = "date",
        title = "Time",
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(title = y)
    )
    
    return(plt)
    
  } else {
    # ggplot version
    p <- ggplot(df, aes(x = x_plot, y = y_plot)) +
      geom_line(color = "steelblue") +
      theme_minimal() +
      labs(x = "Time", y = y)
    
    if (!is.null(diary)) {
      p <- p + geom_rect(
        data = diary,
        aes(xmin = start_time, xmax = end_time, ymin = 0, ymax = ymax_plot, fill = activity),
        alpha = 0.3, inherit.aes = FALSE
      ) +
        labs(title = paste(y, "with diary overlay"))
    }
    
    return(p)
  }
}
