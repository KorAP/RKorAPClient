#' Plot interactive frequency bars with confidence intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Compare relative frequencies from a [frequencyQuery()] result. Frequencies
#' and confidence limits are converted to instances per million tokens, or to
#' percentages for alternatives. The function uses the stored results without
#' making further requests to KorAP.
#'
#' @param df Unconverted data frame returned by [frequencyQuery()]. Do not call
#'   [ipm()] or [percent()] first.
#' @param as.alternatives Denominator mode. NULL (default) inherits the mode
#'   recorded by [frequencyQuery()]. An explicit conflicting mode is rejected.
#'   For older or manually constructed tables without a recorded mode, the
#'   default is FALSE. Denominators are never recalculated from displayed rows.
#' @param ylabel Value-axis label, default "IPM" (instances per million tokens)
#'   or "%" for alternatives.
#' @param horizontal Draw horizontal bars (default). FALSE draws columns.
#' @param showCI Show confidence intervals (default TRUE). FALSE omits error
#'   bars and confidence-interval tooltip lines; interval columns are not required.
#' @param log Use a logarithmic value axis. Non-positive frequencies and
#'   intervals touching zero are omitted, with a note in the caption. Use this
#'   argument rather than changing only the axis type with hc_yAxis().
#' @param ... Additional bar-series options passed to
#'   [highcharter::hc_add_series()], such as dataLabels, color or borderWidth.
#'   The data, type, id and linkedTo options are managed by this helper.
#' @return A highchart htmlwidget, customizable with highcharter functions.
#' @details
#' A single query is compared across virtual corpora; multiple queries in one
#' corpus are compared across queries. With multiple queries and corpora, each
#' corpus is a category with one bar per query. Categories and query groups keep
#' their first-appearance order. A label column, supplied by named virtual
#' corpora, is used for corpus labels.
#'
#' Each query/corpus pair must occur at most once. Missing combinations and
#' rows without a frequency or a positive denominator appear as gaps, not as
#' zero counts. Zero hits in a non-empty corpus remain visible with their
#' confidence interval. Error bars use the confidence limits supplied in df,
#' including the confidence level chosen in frequencyQuery().
#'
#' Tooltips show absolute and relative frequencies, corpus size (or the total
#' frequency of alternatives) and confidence intervals. Clicking a
#' bar opens its stored KorAP concordance link, when available.
#' @section Display language:
#' Generated chart text and number formatting follow the browser's preferred
#' German or English language, falling back to English. Query names and custom
#' labels are retained. Set `options(rkorap.chart.language = "de")` or `"en"`
#' before creating charts to fix the language for exports; `"auto"` restores
#' browser selection. IPM means instances per million tokens.
#' @seealso [geom_freq_bar_ci()] for ggplot2 figures suitable for print.
#' @family highcharter-helpers
#' @export
#' @examples
#' \dontrun{
#' kco <- KorAPConnection("https://korap.dnb.de")
#' frequencyQuery(kco, "Berlin",
#'   vc = c(Buchpreis = "award=buchpreis", DeLiKo = "")) |>
#'   hc_freq_bar_ci()
#'
#' frequencyQuery(kco, c("Iowa", "Berlin"), vc = "award=buchpreis") |>
#'   hc_freq_bar_ci(horizontal = FALSE)
#'
#' frequencyQuery(kco, c("Berlin", "Paris"),
#'   vc = c(Buchpreis = "award=buchpreis", DeLiKo = ""), expand = TRUE) |>
#'   hc_freq_bar_ci()
#' }
hc_freq_bar_ci <- function(df, as.alternatives = NULL,
                           ylabel = if (as.alternatives) "%" else "IPM",
                           horizontal = TRUE, log = FALSE, showCI = TRUE, ...) {
  validateShowCI(showCI)
  required <- c("query", "vc", "totalResults", "total", "f", if (showCI) c("conf.low", "conf.high"))
  if (!is.data.frame(df) || !nrow(df) || !all(required %in% names(df))) {
    stop("Use an unconverted frequencyQuery() result with counts, relative frequencies",
      if (showCI) " and confidence limits." else ".", call. = FALSE)
  }
  as.alternatives <- frequencyChartMode(df, as.alternatives)
  for (flag in list(horizontal, log)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("horizontal and log must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.character(ylabel) || length(ylabel) != 1L || is.na(ylabel)) {
    stop("ylabel must be a single string.", call. = FALSE)
  }
  if ("ipm" %in% names(df)) {
    stop("Pass frequencyQuery() directly; do not call ipm() or percent() before hc_freq_bar_ci().", call. = FALSE)
  }
  options <- list(...)
  if (any(c("data", "type", "id", "linkedTo") %in% names(options))) {
    stop("data, type, id and linkedTo are managed by hc_freq_bar_ci().", call. = FALSE)
  }
  query <- as.character(df$query)
  vc <- as.character(df$vc)
  if (anyNA(query) || any(!nzchar(query)) || anyNA(vc)) {
    stop("Queries must be non-empty and virtual-corpus definitions must not be missing.", call. = FALSE)
  }
  if (anyDuplicated(data.frame(query = query, vc = vc))) {
    stop("Each query/vc combination must occur only once.", call. = FALSE)
  }
  for (field in required[-(1:2)]) {
    x <- df[[field]]
    if (!is.numeric(x) || any(is.infinite(x)) || any(x < 0, na.rm = TRUE)) {
      stop("Counts, frequencies and confidence limits must be non-negative numeric values or NA.", call. = FALSE)
    }
  }
  known <- !is.na(df$totalResults) & !is.na(df$total)
  if (any(df$totalResults[known] > df$total[known])) {
    stop("Hit counts must not exceed their denominators.", call. = FALSE)
  }
  valid <- known & df$total > 0 & !is.na(df$f)
  expected <- df$totalResults[valid] / df$total[valid]
  if (any(abs(df$f[valid] - expected) > pmax(abs(expected), .Machine$double.eps) * 1e-7)) {
    stop("Relative frequencies must be unconverted proportions; do not call ipm() or percent() first.", call. = FALSE)
  }
  if (showCI && (anyNA(df$conf.low[valid]) || anyNA(df$conf.high[valid]) ||
      any(df$conf.low[valid] > df$f[valid]) ||
      any(df$conf.high[valid] < df$f[valid]) ||
      any(df$conf.high[valid] > 1))) {
    stop("Confidence limits must be unconverted proportions enclosing the frequency.", call. = FALSE)
  }

  queries <- unique(query)
  corpora <- unique(vc)
  byCorpus <- length(corpora) > 1L || length(queries) == 1L
  corpusLabels <- queryStringToLabel(corpora)
  if ("label" %in% names(df)) {
    for (i in seq_along(corpora)) {
      labels <- unique(as.character(df$label[vc == corpora[i]]))
      labels <- labels[!is.na(labels) & nzchar(labels)]
      if (length(labels) > 1L) stop("Use one consistent label per virtual corpus.", call. = FALSE)
      if (length(labels)) corpusLabels[i] <- labels
    }
  }
  corpusLabels[!nzchar(corpusLabels)] <- corpora[!nzchar(corpusLabels)]
  corpusLabels[corpora == "" & !nzchar(corpusLabels)] <- "(all)"
  queryLabels <- queries
  if ("variant" %in% names(df)) {
    for (i in seq_along(queries)) {
      labels <- unique(as.character(df$variant[query == queries[i]]))
      labels <- labels[!is.na(labels) & nzchar(labels)]
      if (length(labels) > 1L) stop("Use one consistent variant label per query.", call. = FALSE)
      if (length(labels)) queryLabels[i] <- labels
    }
  }
  categories <- if (byCorpus) make.unique(corpusLabels) else make.unique(queryLabels)
  category <- match(if (byCorpus) vc else query, if (byCorpus) corpora else queries)
  group <- if (byCorpus) match(query, queries) else rep(1L, nrow(df))
  groups <- if (byCorpus) queryLabels else "Frequency"
  scale <- if (as.alternatives) 100 else 1e6
  positive <- valid & !is.na(df$f) & df$f > 0
  if (log && !any(positive)) stop("A logarithmic chart needs at least one positive frequency.", call. = FALSE)
  note <- paste(c(
    if (any(!valid)) "Undefined frequencies are shown as gaps.",
    if (log) if (showCI) "Log scale: non-positive frequencies and intervals touching zero are omitted." else
      "Log scale: non-positive frequencies are omitted."
  ), collapse = " ")
  height <- if (horizontal) max(380, 170 + length(categories) * (20 * length(groups) + 16)) else NULL
  hc <- korapHighchart(height = height, series = length(groups)) |>
    highcharter::hc_chart(type = if (horizontal) "bar" else "column", inverted = horizontal, height = height) |>
    highcharter::hc_title(text = if (length(queries) == 1L) chartText(queryLabels) else "") |>
    highcharter::hc_xAxis(categories = as.list(categories), title = list(text = NULL),
      labels = list(formatter = chartLabelFormatter("value"))) |>
    highcharter::hc_yAxis(min = if (log) NULL else 0, max = if (as.alternatives) 100 else NULL,
      type = if (log) "logarithmic" else "linear", title = list(text = chartText(ylabel))) |>
    highcharter::hc_legend(enabled = length(groups) > 1L) |>
    highcharter::hc_tooltip(useHTML = FALSE, headerFormat = "<b>{point.point.tooltipLabel}</b><br/>",
      pointFormat = "{point.tooltipText}") |>
    highcharter::hc_caption(text = if (nzchar(note)) note else NULL)

  for (j in seq_along(groups)) {
    rows <- which(group == j)
    rows <- rows[match(seq_along(categories), category[rows])]
    points <- lapply(seq_along(categories), function(i) {
      r <- rows[i]
      present <- !is.na(r) && valid[r] && (!log || positive[r])
      point <- list(x = i - 1L, name = categories[i], tooltipLabel = chartText(categories[i]),
        y = if (present) df$f[r] * scale else NA_real_,
        count = if (present) df$totalResults[r] else NA_real_,
        total = if (present) df$total[r] else NA_real_)
      if (showCI) {
        point$confLow <- if (present) df$conf.low[r] * scale else NA_real_
        point$confHigh <- if (present) df$conf.high[r] * scale else NA_real_
      }
      point$tooltipText <- frequencyChartTooltip(if (length(groups) > 1L) groups[j] else NULL,
        point$y, point$count, point$total, point$confLow, point$confHigh, ylabel, as.alternatives, showCI)
      point$korapTooltipType <- "frequency"
      point$korapTooltipName <- if (length(groups) > 1L) groups[j] else NULL
      point$korapAlternatives <- as.alternatives
      point$korapShowCI <- showCI
      point$korapTooltipUnit <- ylabel
      if (present && "webUIRequestUrl" %in% names(df)) point$webUIRequestUrl <- chartUrl(df$webUIRequestUrl[r])
      point
    })
    id <- paste0("korap-frequency-", j)
    series <- utils::modifyList(list(id = id, name = groups[j], data = points,
      type = if (horizontal) "bar" else "column", colorIndex = j - 1L), options)
    hc <- do.call(highcharter::hc_add_series, c(list(hc = hc), series))
    if (showCI) {
      intervals <- lapply(points, function(p) {
        if (log && !is.na(p$confLow) && p$confLow <= 0) p$confLow <- p$confHigh <- NA_real_
        list(x = p$x, low = p$confLow, high = p$confHigh)
      })
      hc <- highcharter::hc_add_series(hc, name = "Confidence interval",
        data = intervals, type = "errorbar", linkedTo = id,
        showInLegend = FALSE, enableMouseTracking = FALSE)
    }
  }
  chartLocalize(hc, list(kind = "frequency", caption = if (nzchar(note)) note else NULL,
    gaps = any(!valid), log = log, showCI = showCI))
}
