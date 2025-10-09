#' @noRd
style_plotly <- function(p) {
  p %>%
    plotly::layout(
      font = list(
        family = "Inter",
        color = "#444444"
      ),
      margin = list(l = 60, r = 30, t = 30, b = 50),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    ) %>%
    plotly::config(
      displaylogo = FALSE
      # ,modeBarButtonsToRemove = c("toImage")  # optional
    )
}