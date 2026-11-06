#' Plot interactive frequency bars with confidence intervals
#'
#' Compare relative frequencies from a [frequencyQuery()] result. Frequencies
#' and confidence limits are converted to instances per million tokens, or to
#' percentages for alternatives. The function uses the stored results without
#' making further requests to KorAP.
#'
#' @param df Unconverted data frame returned by [frequencyQuery()]. Do not call
#'   [ipm()] or [percent()] first.
#' @param as.alternatives Use percentages of alternatives. Also set
#'   as.alternatives = TRUE in [frequencyQuery()]; the plotting function retains
#'   its denominators and does not recalculate them from the displayed rows.
#' @param ylabel Value-axis label, default "ipm" or "%" for alternatives.
#' @param horizontal Draw horizontal bars (default). FALSE draws columns.
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
#' Tooltips show hit counts, denominators and confidence limits. Clicking a
#' bar opens its stored KorAP concordance link, when available.
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
hc_freq_bar_ci <- function(df, as.alternatives = FALSE,
                           ylabel = if (as.alternatives) "%" else "ipm",
                           horizontal = TRUE, ...) {
  required <- c("query", "vc", "totalResults", "total", "f", "conf.low", "conf.high")
  if (!is.data.frame(df) || !nrow(df) || !all(required %in% names(df))) {
    stop("Use an unconverted frequencyQuery() result with counts and confidence limits.", call. = FALSE)
  }
  for (flag in list(as.alternatives, horizontal)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("as.alternatives and horizontal must be TRUE or FALSE.", call. = FALSE)
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
  for (field in required[3:7]) {
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
  if (anyNA(df$conf.low[valid]) || anyNA(df$conf.high[valid]) ||
      any(df$conf.low[valid] > df$f[valid]) ||
      any(df$conf.high[valid] < df$f[valid]) ||
      any(df$conf.high[valid] > 1)) {
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
  corpusLabels[!nzchar(corpusLabels)] <- "(all)"
  categories <- if (byCorpus) make.unique(corpusLabels) else queries
  category <- match(if (byCorpus) vc else query, if (byCorpus) corpora else queries)
  group <- if (byCorpus) match(query, queries) else rep(1L, nrow(df))
  groups <- if (byCorpus) queries else "Frequency"
  scale <- if (as.alternatives) 100 else 1e6
  colors <- c("#1F77B4", "#E69F00", "#009E73", "#CC79A7",
              "#56B4E9", "#D55E00", "#0072B2", "#000000")
  if (length(groups) > length(colors)) colors <- grDevices::hcl.colors(length(groups), "Dark 3")
  text <- frequencyChartText
  note <- paste0("Error bars: supplied confidence intervals. Click bars for concordances.",
    if (any(!valid)) " Undefined frequencies are shown as gaps." else "")
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = if (horizontal) "bar" else "column", inverted = horizontal) |>
    highcharter::hc_add_theme(highcharter::hc_theme_google()) |>
    highcharter::hc_title(text = if (length(queries) == 1L) text(queries) else "") |>
    highcharter::hc_xAxis(categories = text(categories), title = list(text = NULL)) |>
    highcharter::hc_yAxis(min = 0, max = if (as.alternatives) 100 else NULL,
      title = list(text = text(ylabel))) |>
    highcharter::hc_legend(enabled = length(groups) > 1L) |>
    highcharter::hc_tooltip(useHTML = FALSE, headerFormat = "<b>{point.key}</b><br/>",
      pointFormat = paste0("{series.name}: <b>{point.y:.2f}</b> ", text(ylabel),
        "<br/>Hits: {point.count:,.0f}<br/>Denominator: {point.total:,.0f}",
        "<br/>Interval: {point.confLow:.2f}–{point.confHigh:.2f} ", text(ylabel))) |>
    highcharter::hc_plotOptions(series = list(point = list(events = list(
      click = highcharter::JS("function () { var u = this.webUIRequestUrl; if (typeof u === 'string' && (u.toLowerCase().indexOf('https://') === 0 || u.toLowerCase().indexOf('http://') === 0)) window.open(u, 'korap', 'noopener'); }"),
      mouseOver = highcharter::JS("function () { this.series.chart.container.style.cursor = this.webUIRequestUrl ? 'pointer' : 'default'; }"),
      mouseOut = highcharter::JS("function () { this.series.chart.container.style.cursor = 'default'; }"))))) |>
    highcharter::hc_caption(text = note) |>
    highcharter::hc_exporting(enabled = TRUE) |>
    highcharter::hc_credits(enabled = TRUE, text = "KorAP R Client Package",
      href = "https://github.com/KorAP/RKorAPClient/")

  for (j in seq_along(groups)) {
    rows <- which(group == j)
    rows <- rows[match(seq_along(categories), category[rows])]
    points <- lapply(seq_along(categories), function(i) {
      r <- rows[i]
      present <- !is.na(r) && valid[r]
      point <- list(x = i - 1L, name = text(categories[i]),
        y = if (present) df$f[r] * scale else NA_real_,
        count = if (present) df$totalResults[r] else NA_real_,
        total = if (present) df$total[r] else NA_real_,
        confLow = if (present) df$conf.low[r] * scale else NA_real_,
        confHigh = if (present) df$conf.high[r] * scale else NA_real_)
      if (present && "webUIRequestUrl" %in% names(df)) {
        url <- df$webUIRequestUrl[r]
        if (is.character(url) && !is.na(url) &&
            grepl("^https?://[^[:space:]<>\"']+$", url, ignore.case = TRUE)) {
          point$webUIRequestUrl <- unname(url)
        }
      }
      point
    })
    id <- paste0("korap-frequency-", j)
    series <- utils::modifyList(list(id = id, name = text(groups[j]), data = points,
      type = if (horizontal) "bar" else "column", color = colors[j]), options)
    hc <- do.call(highcharter::hc_add_series, c(list(hc = hc), series))
    intervals <- lapply(points, function(p) list(x = p$x, low = p$confLow, high = p$confHigh))
    hc <- highcharter::hc_add_series(hc, name = "Confidence interval",
      data = intervals, type = "errorbar", linkedTo = id, color = "#333333",
      showInLegend = FALSE, enableMouseTracking = FALSE)
  }
  hc
}

frequencyChartText <- function(x) {
  x <- unname(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
