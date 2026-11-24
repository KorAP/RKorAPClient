#' Plot distributions underlying a dispersion analysis using ECharts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Counterpart of [hc_dispersion()] based on the open-source Apache ECharts
#' library. It shows the same values, tooltips and KorAP links.
#'
#' @inheritParams hc_dispersion
#' @inherit hc_dispersion details
#' @return An echarts4r htmlwidget, customizable with echarts4r functions.
#' @inheritSection ec_freq_by_year_ci Switching curves
#' @inheritSection ec_freq_by_year_ci Themes
#' @inheritSection ec_freq_by_year_ci Untrusted text
#' @inheritSection hc_freq_bar_ci Display language
#' @family echarts-helpers
#' @export
#' @examples
#' \dontrun{
#' KorAPConnection() |>
#'   corpusQuery(c(star = "[orth='Nutzer*innen']",
#'                 colon = "[orth='Nutzer:innen']")) |>
#'   fetchSamplePages(maxHits = Inf) |>
#'   dispersionAnalysis(by = "source") |>
#'   ec_dispersion(top = 12)
#' }
ec_dispersion <- function(df, partition = NULL, top = NULL) {
  requireEcharts()
  spec <- dispersionChartSpec(df, partition, top)
  bar <- function(name, points, ...) list(type = "bar", name = name, ...,
    barGap = "10%", barCategoryGap = "25%", animation = FALSE,
    label = list(show = TRUE, position = "right"),
    data = lapply(points, function(p) chartEchartPoint(chartEchartValue(p$y), p)))
  series <- c(list(bar("Corpus tokens", spec$reference, color = "#B8B8B8")),
    lapply(seq_along(spec$labels), function(j) bar(spec$labels[j], spec$points[[j]])))
  # The categories keep their order from top to bottom.
  korapEchart(list(
    title = list(list(text = spec$title, left = "center")),
    grid = list(top = 85, bottom = 30, left = 10, right = 65),
    xAxis = list(type = "value", min = 0, max = 100, interval = 20, name = spec$locale$defaultY,
      nameLocation = "middle", nameGap = 28),
    yAxis = list(type = "category", data = as.list(spec$categories), inverse = TRUE,
      axisLabel = list(interval = 0, width = 200, overflow = "truncate")),
    # The chart can be higher than the page.
    legend = list(left = 10, right = 60, top = 38),
    series = series
  ), spec$locale, chartPalette(length(spec$labels)), "item", horizontal = TRUE,
    height = spec$height, unit = "%", categories = length(spec$categories))
}
