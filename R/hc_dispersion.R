#' Plot interactive distributions underlying a dispersion analysis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Compare each query's share of hits with the corpus-token share of each part.
#' The chart uses stored distributions and makes no requests to KorAP.
#'
#' @param df A query-based [dispersionAnalysis()] or [dispersionQuery()] result.
#' @param partition Name of one partition in the result. May be omitted when
#'   the result contains only one partition. Variants must use the same parts,
#'   token sizes, virtual-corpus definitions, KorAP instance and index revision.
#' @param top Number of parts to show individually, default 12. Parts are ranked
#'   by the largest hit share across the variants, with token size breaking ties.
#'   Remaining parts are combined into an additional labelled bar group.
#'   `Inf` shows all parts. For `year`, `pubDate` and `creationDate` partitions,
#'   displayed four-digit years are ordered chronologically. Dispersion scores
#'   are not recalculated.
#' @return A highchart htmlwidget, customizable with highcharter functions.
#' @details
#' Percentages use hits and tokens within the listed, positive-sized parts,
#' matching the population used by the dispersion measures. Positive-sized
#' parts without hits are retained. A zero-hit variant has no defined hit-share
#' distribution and is identified in the caption without drawing hit-share bars.
#'
#' Grey bars show corpus-token shares; coloured bars show the query variants.
#' Tooltips include counts and frequencies per million tokens. Click individual
#' variant bars to open their stored concordance links. Corpus-token bars and
#' combined remainder bars have no concordance link.
#'
#' Sample-based results are labelled as estimates, with inclusion probabilities
#' in tooltips. Page clustering can strongly bias these shares. The chart does
#' not provide confidence intervals or an overall ranking of dispersion scores.
#' The interface and display may change in future releases.
#' @family highcharter-helpers
#' @export
#' @examples
#' \dontrun{
#' d <- KorAPConnection() |>
#'   corpusQuery(c(star = "[orth='Nutzer*innen']",
#'                 colon = "[orth='Nutzer:innen']")) |>
#'   fetchSamplePages(maxHits = Inf) |>
#'   dispersionAnalysis(by = "source")
#' hc_dispersion(d, top = 12)
#' }
hc_dispersion <- function(df, partition = NULL, top = 12) {
  required <- c("partition", "query", "distribution", "estimated", "sampling")
  if (!is.data.frame(df) || !nrow(df) || !all(required %in% names(df))) {
    stop("Use a query-based dispersionAnalysis() or dispersionQuery() result with distribution data.", call. = FALSE)
  }
  available <- unique(as.character(df$partition))
  if (anyNA(available) || any(!nzchar(available))) stop("Result partition names must be non-empty.", call. = FALSE)
  if (is.null(partition)) {
    if (length(available) != 1L) stop("Choose one partition: ", paste(available, collapse = ", "), ".", call. = FALSE)
    partition <- available
  }
  dispersionScalarString(partition, "partition")
  if (!partition %in% available) stop("Unknown partition. Choose from: ", paste(available, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(top) || length(top) != 1L || is.na(top) || top <= 0 ||
      (is.finite(top) && top != floor(top))) stop("top must be a positive integer or Inf.", call. = FALSE)
  rows <- df[df$partition == partition, , drop = FALSE]
  if (!is.logical(rows$estimated) || anyNA(rows$estimated)) stop("Result sampling indicators are unavailable.", call. = FALSE)
  for (field in c("apiUrl", "indexRevision")) {
    scope <- lapply(rows$sampling, `[[`, field)
    if (!all(vapply(scope, identical, logical(1), scope[[1]]))) {
      stop("Variants must use the same KorAP instance and index revision; select a single corpus.", call. = FALSE)
    }
  }
  parts <- lapply(rows$distribution, dispersionChartDistribution)
  reference <- parts[[1]]
  parts <- lapply(parts, function(part) {
    index <- match(reference$part, part$part)
    if (nrow(part) != nrow(reference) || anyNA(index) ||
        !identical(as.numeric(part$tokens[index]), as.numeric(reference$tokens)) ||
        !identical(as.character(part$vc[index]), as.character(reference$vc))) {
      stop("Variants must use the same corpus parts, token sizes and virtual corpus; select a single corpus.", call. = FALSE)
    }
    part[index, , drop = FALSE]
  })
  keep <- reference$tokens > 0
  reference <- reference[keep, , drop = FALSE]
  parts <- lapply(parts, function(part) part[keep, , drop = FALSE])
  if (!nrow(reference)) stop("There are no positive-sized corpus parts to plot.", call. = FALSE)
  labels <- as.character(if ("variant" %in% names(rows)) rows$variant else rows$query)
  if (anyNA(labels) || any(!nzchar(labels))) stop("Query variant labels must be non-empty.", call. = FALSE)
  labels <- make.unique(c("Corpus tokens", labels))[-1]
  probabilities <- vapply(rows$sampling, function(s) {
    p <- s$inclusionProbability
    if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p <= 0 || p > 1) {
      stop("Result inclusion probabilities are unavailable.", call. = FALSE)
    }
    p
  }, numeric(1))
  if (any(rows$estimated != (probabilities < 1))) stop("Result sampling indicators are inconsistent.", call. = FALSE)
  counts <- do.call(cbind, lapply(parts, `[[`, "frequency"))
  observed <- do.call(cbind, lapply(parts, `[[`, "observed"))
  totals <- colSums(counts)
  shares <- sweep(counts, 2, ifelse(totals > 0, totals, 1), "/")
  shares[, totals == 0] <- NA_real_
  largest <- apply(shares, 1, function(v) if (all(is.na(v))) 0 else max(v, na.rm = TRUE))
  order <- order(-largest, -reference$tokens, reference$part)
  shown <- head(order, min(top, length(order)))
  if (partition %in% c("year", "pubDate", "creationDate") &&
      all(grepl("^[0-9]{4}$", reference$part))) {
    shown <- shown[order(reference$part[shown])]
  }
  buckets <- lapply(shown, function(i) i)
  categories <- reference$part[shown]
  remaining <- setdiff(order, shown)
  if (length(remaining)) {
    buckets <- c(buckets, list(remaining))
    categories <- make.unique(c(categories, paste0("Remaining ", length(remaining),
      if (length(remaining) == 1L) " part" else " parts")))
  }
  tokens <- vapply(buckets, function(i) sum(reference$tokens[i]), numeric(1))
  grouped <- function(values) vapply(buckets, function(i) sum(values[i]), numeric(1))
  notes <- vapply(seq_along(buckets), function(i) {
    if (length(remaining) && i == length(buckets))
      paste0("<br/>", length(remaining), if (length(remaining) == 1L) " part" else " parts",
        " combined for display.") else ""
  }, character(1))
  points <- function(values, frequencies = NULL, sampleCounts = NULL, variant = NULL) {
    lapply(seq_along(buckets), function(i) {
      point <- list(x = i - 1L, name = dispersionChartText(categories[i]), y = values[i],
                    tokens = tokens[i], tokenTotal = sum(tokens), note = notes[i])
      if (!is.null(variant)) {
        point$frequency <- frequencies[i]
        point$totalInParts <- totals[variant]
        point$observed <- sampleCounts[i]
        point$perMillion <- frequencies[i] / tokens[i] * 1e6
        if (length(buckets[[i]]) == 1L && !nzchar(notes[i])) {
          urls <- parts[[variant]]$webUIRequestUrl
          if (!is.null(urls)) point$webUIRequestUrl <- dispersionChartUrl(urls[buckets[[i]]])
        }
      }
      point
    })
  }
  excluded <- ("excludedHits" %in% names(rows) && any(rows$excludedHits > 0, na.rm = TRUE)) ||
    any(vapply(rows$sampling, function(s) is.numeric(s$excludedTokens) &&
      any(s$excludedTokens > 0, na.rm = TRUE), logical(1)))
  zeroExact <- totals == 0 & !rows$estimated
  zeroSample <- totals == 0 & rows$estimated
  caption <- paste0("Shares within listed parts. Click individual query bars for concordances.",
    if (length(remaining)) paste0(" ", length(shown), " of ", nrow(reference),
      " parts shown individually; scores still use all parts.") else "",
    if (any(rows$estimated)) " Page-sample estimates can be biased by clustering." else "",
    if (length(unique(probabilities)) > 1L) " Variants have unequal page coverage." else "",
    if (excluded) " Material outside the listed parts is excluded." else "",
    if (any(zeroExact)) paste0(" No hits within listed parts for ",
      paste(labels[zeroExact], collapse = ", "), "; no hit-share bars.") else "",
    if (any(zeroSample)) paste0(" No observed sample hits within listed parts for ",
      paste(labels[zeroSample], collapse = ", "), "; hit shares are undefined.") else "")
  titles <- c(source = "source", corpusTitle = "corpus title", pubPlaceKey = "publication country",
              region = "region", year = "publication year", pubDate = "publication year",
              creationDate = "creation year")
  title <- if (partition %in% names(titles)) unname(titles[partition]) else partition
  height <- max(380, 170 + length(buckets) * (20 * (nrow(rows) + 1) + 16))
  hc <- highcharter::highchart(height = height) |>
    highcharter::hc_chart(type = "bar", height = height, spacingRight = 65,
      style = list(fontFamily = "sans-serif")) |>
    highcharter::hc_title(text = dispersionChartText(paste("Distribution by", title))) |>
    highcharter::hc_xAxis(categories = dispersionChartText(categories), title = list(text = NULL),
      labels = list(style = list(fontSize = "12px", width = "210px", textOverflow = "ellipsis"))) |>
    highcharter::hc_yAxis(min = 0, max = 100, tickInterval = 20,
      title = list(text = "Share within listed parts (%)"),
      labels = list(format = "{value}%", overflow = "allow")) |>
    highcharter::hc_legend(align = "left", verticalAlign = "top", maxHeight = 80,
      itemStyle = list(fontSize = "12px")) |>
    highcharter::hc_tooltip(useHTML = FALSE, headerFormat = "<b>{point.key}</b><br/>") |>
    highcharter::hc_plotOptions(series = list(animation = FALSE, borderWidth = 0,
      groupPadding = .16, pointPadding = .06,
      dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%", crop = FALSE, overflow = "allow", allowOverlap = TRUE,
        style = list(fontSize = "12px", color = "#333333", textOutline = "none")),
      point = list(events = list(
        click = highcharter::JS("function () { var u = this.webUIRequestUrl; if (typeof u === 'string' && (u.toLowerCase().indexOf('https://') === 0 || u.toLowerCase().indexOf('http://') === 0)) window.open(u, 'korap', 'noopener'); }"),
        mouseOver = highcharter::JS("function () { this.series.chart.container.style.cursor = this.webUIRequestUrl ? 'pointer' : 'default'; }"),
        mouseOut = highcharter::JS("function () { this.series.chart.container.style.cursor = 'default'; }")
      )))) |>
    highcharter::hc_caption(text = dispersionChartText(caption), style = list(fontSize = "12px")) |>
    highcharter::hc_exporting(enabled = TRUE) |>
    highcharter::hc_credits(enabled = TRUE, text = "KorAP R Client Package", href = "https://github.com/KorAP/RKorAPClient/") |>
    highcharter::hc_responsive(rules = list(list(condition = list(maxWidth = 520),
      chartOptions = list(xAxis = list(labels = list(style = list(width = "110px"))))))) |>
    highcharter::hc_add_series(name = "Corpus tokens", color = "#B8B8B8",
      data = points(tokens / sum(tokens) * 100),
      tooltip = list(pointFormat = "{series.name}: <b>{point.y:.2f}%</b><br/>Tokens: {point.tokens:,.0f} / {point.tokenTotal:,.0f}{point.note}"))
  colors <- c("#1F77B4", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#0072B2", "#000000")
  if (nrow(rows) > length(colors)) colors <- grDevices::hcl.colors(nrow(rows), "Dark 3")
  for (j in seq_len(nrow(rows))) {
    frequencies <- grouped(counts[, j])
    values <- if (totals[j] > 0) frequencies / totals[j] * 100 else rep(NA_real_, length(buckets))
    tooltip <- paste0("{series.name}: <b>{point.y:.2f}%</b><br/>",
      if (rows$estimated[j]) "Estimated hits: {point.frequency:,.2f} / {point.totalInParts:,.2f}<br/>Observed sample hits: {point.observed:,.0f}" else "Hits: {point.frequency:,.0f} / {point.totalInParts:,.0f}",
      "<br/>Frequency per million tokens: {point.perMillion:,.3f}",
      if (rows$estimated[j]) paste0("<br/>Page inclusion probability: ", format(probabilities[j] * 100, digits = 4), "%") else "",
      "{point.note}")
    hc <- highcharter::hc_add_series(hc, name = dispersionChartText(labels[j]), color = colors[j],
      data = points(values, frequencies, grouped(observed[, j]), j), tooltip = list(pointFormat = tooltip))
  }
  hc
}

dispersionChartDistribution <- function(part) {
  if (!is.data.frame(part) || !nrow(part) ||
      !all(c("part", "vc", "tokens", "frequency", "observed") %in% names(part)) ||
      !is.character(part$part) || anyNA(part$part) || any(!nzchar(part$part)) || anyDuplicated(part$part) ||
      !is.character(part$vc) || anyNA(part$vc)) stop("Stored corpus-part distributions are unavailable or invalid.", call. = FALSE)
  for (field in c("tokens", "frequency", "observed")) {
    x <- part[[field]]
    if (!is.numeric(x) || any(!is.finite(x)) || any(x < 0) || !is.finite(sum(x))) {
      stop("Stored corpus-part counts must be finite and non-negative.", call. = FALSE)
    }
  }
  if (any(part$tokens == 0 & (part$frequency > 0 | part$observed > 0))) stop("Hits occur in a zero-sized corpus part.", call. = FALSE)
  part
}

dispersionChartText <- function(x) {
  x <- unname(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

dispersionChartUrl <- function(url) {
  if (is.character(url) && length(url) == 1L && !is.na(url) &&
      grepl("^https?://[^[:space:]<>\"']+$", url, ignore.case = TRUE)) unname(url) else NULL
}
