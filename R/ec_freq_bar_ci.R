#' Plot frequency bars with confidence intervals using ECharts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Counterpart of [hc_freq_bar_ci()] based on the open-source Apache ECharts
#' library. It shows the same values, intervals, tooltips and KorAP links.
#'
#' @inheritParams hc_freq_bar_ci
#' @param log Use a logarithmic value axis. Non-positive frequencies and
#'   intervals touching zero are omitted, with a note in the caption.
#' @param ... Additional options for the bars, as named in the
#'   [ECharts documentation](https://echarts.apache.org/en/option.html#series-bar),
#'   such as `barGap` or `label`.
#' @inherit hc_freq_bar_ci details
#' @return An echarts4r htmlwidget, customizable with echarts4r functions.
#' @inheritSection ec_freq_by_year_ci Switching curves
#' @inheritSection ec_freq_by_year_ci Themes
#' @inheritSection ec_freq_by_year_ci Untrusted text
#' @inheritSection hc_freq_bar_ci Display language
#' @family echarts-helpers
#' @export
#' @examples
#' \dontrun{
#' KorAPConnection("https://korap.dnb.de") |>
#'   frequencyQuery(c("Berlin", "Paris"),
#'     vc = c(Buchpreis = "award=buchpreis", DeLiKo = ""), expand = TRUE) |>
#'   ec_freq_bar_ci()
#' }
ec_freq_bar_ci <- function(df, as.alternatives = NULL,
                           ylabel = if (as.alternatives) "%" else "IPM",
                           horizontal = TRUE, log = FALSE, showCI = TRUE, ...) {
  requireEcharts()
  options <- list(...)
  chartEchartManaged(options, c("data", "type", "name"), "ec_freq_bar_ci")
  spec <- frequencyBarSpec(df, as.alternatives, if (missing(ylabel)) NULL else ylabel, horizontal, log, showCI)
  groups <- make.unique(spec$groups)
  low <- unlist(lapply(spec$points, function(g) lapply(g, function(p) c(p$y, p$confLow))))
  bars <- lapply(seq_along(groups), function(j) utils::modifyList(list(type = "bar", name = groups[j],
    barGap = "10%", barCategoryGap = "25%",
    data = lapply(spec$points[[j]], function(p) chartEchartPoint(chartEchartValue(p$y), p))), options))
  whiskers <- if (showCI) lapply(seq_along(groups), function(j) {
    points <- Filter(function(p) !is.na(p$confLow) && !(log && p$confLow <= 0), spec$points[[j]])
    list(type = "custom", name = groups[j], silent = TRUE, clip = TRUE, z = 10, animation = FALSE,
      encode = if (horizontal) list(y = 0, x = list(1, 2)) else list(x = 0, y = list(1, 2)),
      data = lapply(points, function(p) list(p$x, p$confLow, p$confHigh)))
  })
  # Bars on a logarithmic axis start at 1 unless told otherwise.
  base <- if (log) 10^(ceiling(log10(min(low[!is.na(low) & low > 0]))) - 1)
  value <- c(list(type = if (log) "log" else "value", name = spec$ylabel,
    nameLocation = "middle", nameGap = if (horizontal) 28 else 45,
    max = if (spec$as.alternatives) 100 else NULL),
    if (log) list(min = base, startValue = base) else list(min = 0))
  category <- list(type = "category", data = as.list(spec$categories), inverse = horizontal,
    axisLabel = list(interval = 0, width = 180, overflow = if (horizontal) "truncate" else "break"))
  title <- as.character(spec$title)
  korapEchart(list(
    title = list(list(text = title, left = "center")),
    grid = list(top = if (nzchar(title)) 85 else 50, bottom = 30, left = 30, right = 30),
    xAxis = if (horizontal) value else category,
    yAxis = if (horizontal) category else value,
    series = c(bars, whiskers)
  ), list(kind = "frequency", gaps = spec$gaps, log = log, showCI = showCI), chartPalette(length(groups)), "item",
    horizontal = horizontal, height = if (horizontal) spec$height else NULL,
    frequencySeries = length(groups), categories = length(spec$categories))
}
