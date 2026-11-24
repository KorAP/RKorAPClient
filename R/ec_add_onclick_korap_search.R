#' Add KorAP search click events to ECharts plots
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Clicks on data points launch the KorAP web UI query stored with the point, in
#' a separate tab that is reused for subsequent clicks. Only HTTP(S) links are
#' opened; points without a valid link are not clickable. The other `ec_`
#' functions include this already.
#'
#' @family echarts-helpers
#' @param e An echarts4r htmlwidget whose data points have a `webUIRequestUrl`,
#'   see [echarts4r::e_add_unnested()].
#' @return The input widget with added on-click events.
#' @export
#' @examples
#' \dontrun{
#' library(echarts4r)
#' KorAPConnection() |>
#'   frequencyQuery("Team", vc = paste("pubDate in", 2014:2018)) |>
#'   dplyr::mutate(year = as.character(2014:2018), ipm = f * 1e6) |>
#'   e_charts(year) |>
#'   e_line(ipm) |>
#'   e_add_unnested("webUIRequestUrl", webUIRequestUrl) |>
#'   ec_add_onclick_korap_search()
#' }
ec_add_onclick_korap_search <- function(e) {
  requireEcharts()
  htmlwidgets::onRender(e, chartJS("ec-click.js"))
}
