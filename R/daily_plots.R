#' Plot Daily Aggregated Time Series Data
#'
#' This function aggregates a numeric variable (`y`) by day from a minute-level time series
#' (`df`) and creates a responsive bar chart visualization. Days with less than 2 hours
#' (120 minutes) of recorded data are automatically excluded from the analysis.
#'
#' @param df A data frame containing the minute-level time series data.
#' @param x A string specifying the **timestamp** column name in `df` (must be POSIXct).
#' @param y A string specifying the **numeric value** column to aggregate and plot (e.g., 'steps').
#' @param fun A function used to aggregate the daily data (e.g., `sum`, `mean`, `median`). Default is `sum`.
#' @param interactive Logical; if TRUE, returns a `plotly` object and the aggregated data. If FALSE, returns a `ggplot2` object.
#' @param cadence_cutoff Numeric; The minimum required value in the `y` column for a minute to be included in the daily accumulation (e.g., set to 80 to count minutes with 80+ steps/min). Default is 0.
#'
#' @return If `interactive = FALSE`, returns a single `ggplot2` object.
#'         If `interactive = TRUE`, returns a **list** with two elements:
#'         \itemize{
#'           \item \code{plot}: The `plotly` object.
#'           \item \code{data}: A data frame containing the daily aggregated values used for the plot.
#'         }
#'
#' @import ggplot2
#' @importFrom dplyr mutate group_by summarise across filter
#' @importFrom lubridate as_date
#' @importFrom plotly plot_ly layout add_trace
#' @export
daily_plots <- function(df, x, y, fun = sum, interactive = FALSE, cadence_cutoff = 0) {
  stopifnot(is.function(fun))
  
  # Preprocessing
  df <- df |>
    mutate(
      .date = as_date(.data[[x]]),
      .time = .data[[x]],
      .value = as.numeric(.data[[y]])
    )
  
  # Helper: Filter + Aggregate
  filter_and_aggregate <- function(data, cutoff = NULL) {
    if (!is.null(cutoff)) {
      data <- data |> filter(.value >= cutoff)
    }
    
    # Only keep days with ≥ 2h of data
    valid_days <- data |>
      group_by(.date) |>
      summarise(duration_mins = as.numeric(difftime(max(.time), min(.time), units = "mins")), .groups = "drop") |>
      filter(duration_mins >= 120)
    
    data <- data |> filter(.date %in% valid_days$.date)
    
    # Daily summary
    data |>
      group_by(.date) |>
      summarise(value = fun(.value, na.rm = TRUE), .groups = "drop")
  }
  
  # Non-interactive ggplot2 version
  s_data <- filter_and_aggregate(df, cutoff = cadence_cutoff)
  if (!interactive) {
    return(
      ggplot(s_data, aes(x = .date, y = value)) +
        geom_col(aes(fill = value), show.legend = FALSE) +
        scale_fill_gradient(low = "red", high = "green") +
        labs(
          x = "Date",
          y = paste("Daily", y),
          title = paste("Daily", deparse(substitute(fun)), y)
        ) +
        theme_minimal()
    )
  }
  
  # Interactive version with plotly + dropdown
  s_data$.date  <- as.character(s_data$.date)
  maxi  <- max(s_data$value, na.rm = TRUE)
  
  plotly_obj = plotly::plot_ly() %>%
    plotly::add_trace(data = s_data,  x = ~.date, y = ~value, type = "bar",
              visible = TRUE,  marker = list(color = ~value, coloraxis = "coloraxis", cmin = 0, cmax = maxi), name = " ") %>%
    plotly::layout(
      xaxis = list(title = "Date", type = "category"),
      yaxis = list(title = paste("Daily", y)),
      bargap = 0.2,
      coloraxis = list(
        colorscale = list(
          list(0, "#d73027"),
          list(0.5, "#fee08b"),
          list(1, "#1a9850")
        ),
        colorbar = list(title = "Steps")
      )
    )
  return(list(plot = plotly_obj, data = s_data))
}