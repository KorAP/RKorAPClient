#' Plot frequency curves with confidence intervals using ECharts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Counterpart of [hc_freq_by_year_ci()] based on the open-source Apache ECharts
#' library. It shows the same values, intervals, tooltips and KorAP links.
#'
#' @inheritParams hc_freq_by_year_ci
#' @param smooth Draw curved connections (default FALSE). This only changes the
#'   display; observations remain at the measured years, and the interpolated
#'   band does not supply confidence intervals for dates between them.
#' @param ... Additional options for the curves, as named in the
#'   [ECharts documentation](https://echarts.apache.org/en/option.html#series-line),
#'   such as `symbolSize` or `lineStyle`.
#' @return An echarts4r htmlwidget, customizable with echarts4r functions.
#' @details
#' Years with an undefined frequency leave gaps. Years without a row in `df` are
#' connected, as in [hc_freq_by_year_ci()].
#' @section Switching curves:
#' Click a legend entry to hide or show a curve together with its interval. The
#' All and None buttons next to the legend switch all curves at once.
#' @section Themes:
#' Use [echarts4r::e_theme()] or [echarts4r::e_theme_custom()] on a chart, or set
#' `options(rkorap.echarts.theme = )` for all charts: to the name of an echarts4r
#' theme, the path of a JSON file made with the
#' [ECharts theme builder](https://echarts.apache.org/en/theme-builder.html), or
#' a list such as `list(color = c("#003366", "#CC6600"), backgroundColor = "#FBF7EE")`.
#' @section Untrusted text:
#' Do not pass queries or corpus metadata to echarts4r functions such as
#' [echarts4r::e_title()].
#' @inheritSection hc_freq_bar_ci Display language
#' @family echarts-helpers
#' @export
#' @examples
#' \dontrun{
#' KorAPConnection() |>
#'   frequencyQuery(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"),
#'     vc = paste("textType = /Zeit.*/ & pubDate in", 1990:2018),
#'     as.alternatives = TRUE) |>
#'   ec_freq_by_year_ci()
#' }
ec_freq_by_year_ci <- function(df, as.alternatives = NULL,
                               ylabel = if (as.alternatives) "%" else "IPM",
                               smooth = FALSE, showCI = TRUE, ...) {
  requireEcharts()
  options <- list(...)
  chartEchartManaged(options, c("data", "type", "name", "stack"), "ec_freq_by_year_ci")
  spec <- frequencyYearSpec(df, as.alternatives, showCI)
  as.alternatives <- spec$as.alternatives
  conditions <- unique(spec$df$condition)
  years <- spec$df$year
  if (!is.numeric(years) || anyNA(years) || any(years != floor(years)) || any(years < 1 | years > 9999)) {
    stop("ec_freq_by_year_ci() needs whole calendar years.", call. = FALSE)
  }
  bands <- list()
  curves <- list()
  for (i in seq_along(conditions)) {
    name <- as.character(conditions[i])
    dat <- spec$df[spec$df$condition == conditions[i], ]
    dat <- dat[order(dat$year), ]
    points <- frequencyYearPoints(dat, name, as.alternatives, ylabel, showCI)
    # ECharts stacks by position on a time axis, but would add up numeric years.
    at <- sprintf("%04d-01-01", as.integer(points$year))
    series <- function(values) lapply(seq_along(at), function(k) list(value = list(at[k], chartEchartValue(values[k]))))
    if (showCI) {
      defined <- !is.na(points$value)
      low <- ifelse(defined, points$confLow, NA)
      band <- list(type = "line", name = name, stack = paste0("ci", i), stackStrategy = "all",
        smooth = smooth, smoothMonotone = "x", symbol = "none", silent = TRUE, animation = FALSE,
        lineStyle = list(opacity = 0), emphasis = list(disabled = TRUE), z = 1)
      bands <- c(bands, list(c(band, list(data = series(low))),
        c(band, list(data = series(points$confHigh - low), areaStyle = list(opacity = 0.18)))))
    }
    data <- lapply(seq_along(at), function(k) {
      point <- as.list(points[k, ])
      point$y <- point$value
      chartEchartPoint(list(at[k], chartEchartValue(point$value)), point[names(point) != "value"])
    })
    curves <- c(curves, list(utils::modifyList(list(type = "line", name = name, data = data,
      smooth = smooth, smoothMonotone = "x", symbolSize = 5, showAllSymbol = TRUE, z = 3), options)))
  }
  # Curves first: their order determines palette and legend.
  korapEchart(list(
    title = list(list(text = paste(spec$title, collapse = ", "), left = "center")),
    grid = list(top = if (any(nzchar(spec$title))) 70 else 50, bottom = 30, left = 20, right = 30),
    xAxis = list(type = "time", minInterval = 366 * 24 * 3600 * 1000, axisLabel = list(formatter = "{yyyy}", hideOverlap = TRUE, showMaxLabel = length(unique(years)) > 1L)),
    yAxis = list(type = "value", min = 0,
      name = if (as.alternatives) "" else ylabel),
    legend = list(icon = "circle"),
    series = c(curves, bands)
  ), list(kind = "year"), chartYearPalette(), "axis", unit = if (as.alternatives) ylabel else "",
    frequencySeries = length(curves))
}
