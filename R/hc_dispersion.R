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
#' @param top Number of parts to show individually. NULL (default) shows all
#'   calendar years for year/date partitions, and 12 parts otherwise. Parts are ranked
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
#' Tooltips distinguish shares of total frequency from relative frequencies
#' per million tokens, and show absolute frequencies and subcorpus sizes. Click individual
#' variant bars to open their stored concordance links. Corpus-token bars and
#' combined remainder bars have no concordance link.
#'
#' Sample-based results are labelled as estimates, with inclusion probabilities
#' in tooltips. Page clustering can strongly bias these shares. The chart does
#' not provide confidence intervals or an overall ranking of dispersion scores.
#' The interface and display may change in future releases.
#' @inheritSection hc_freq_bar_ci Display language
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
hc_dispersion <- function(df, partition = NULL, top = NULL) {
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
  calendar <- partition %in% c("year", "pubDate", "creationDate")
  if (is.null(top)) top <- if (calendar) Inf else 12
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
  if (calendar &&
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
      point <- list(x = i - 1L, name = categories[i], tooltipLabel = chartText(categories[i]), y = values[i],
                    tokens = tokens[i], tokenTotal = sum(tokens), note = notes[i])
      point$tooltipText <- paste0("Share of corpus tokens: <b>", chartNumber(values[i]),
        "%</b><br/>Subcorpus size (tokens): ", chartNumber(tokens[i], TRUE),
        "<br/>Corpus size (tokens): ", chartNumber(sum(tokens), TRUE), notes[i])
      point$korapTooltipType <- "corpus"
      point$korapCombinedParts <- if (nzchar(notes[i])) length(remaining) else 0L
      if (!is.null(variant)) {
        point$frequency <- frequencies[i]
        point$totalInParts <- unname(totals[variant])
        point$observed <- sampleCounts[i]
        point$korapTooltipType <- "dispersion"
        point$korapEstimated <- rows$estimated[variant]
        point$korapProbability <- probabilities[variant]
        point$perMillion <- frequencies[i] / tokens[i] * 1e6
        if (length(buckets[[i]]) == 1L && !nzchar(notes[i])) {
          urls <- parts[[variant]]$webUIRequestUrl
          if (!is.null(urls)) point$webUIRequestUrl <- chartUrl(urls[buckets[[i]]])
        }
        point$tooltipText <- paste0("<b>", chartText(labels[variant]), "</b><br/>",
          if (rows$estimated[variant]) "Estimated share of total frequency: " else "Share of total frequency: ",
          "<b>", chartNumber(values[i]), "%</b><br/>",
          if (rows$estimated[variant]) "Estimated absolute frequency: " else "Absolute frequency: ",
          chartNumber(point$frequency, !rows$estimated[variant]),
          if (rows$estimated[variant]) "<br/>Estimated total frequency: " else "<br/>Total frequency: ",
          chartNumber(point$totalInParts, !rows$estimated[variant]),
          if (rows$estimated[variant]) paste0("<br/>Absolute frequency in sample: ", chartNumber(point$observed, TRUE)) else "",
          "<br/>Subcorpus size (tokens): ", chartNumber(point$tokens, TRUE),
          if (rows$estimated[variant]) "<br/>Estimated relative frequency: " else "<br/>Relative frequency: ",
          chartNumber(point$perMillion), " IPM",
          if (rows$estimated[variant]) paste0("<br/>Page inclusion probability: ", chartNumber(probabilities[variant] * 100), "%") else "",
          notes[i])
      }
      point
    })
  }
  excluded <- ("excludedHits" %in% names(rows) && any(rows$excludedHits > 0, na.rm = TRUE)) ||
    any(vapply(rows$sampling, function(s) is.numeric(s$excludedTokens) &&
      any(s$excludedTokens > 0, na.rm = TRUE), logical(1)))
  zeroExact <- totals == 0 & !rows$estimated
  zeroSample <- totals == 0 & rows$estimated
  caption <- trimws(paste0(
    if (length(remaining)) paste0(" ", length(shown), " of ", nrow(reference),
      " parts shown individually; scores still use all parts.") else "",
    if (any(rows$estimated)) " Page-sample estimates can be biased by clustering." else "",
    if (length(unique(probabilities)) > 1L) " Variants have unequal page coverage." else "",
    if (excluded) " Material outside the listed parts is excluded." else "",
    if (any(zeroExact)) paste0(" No hits within listed parts for ",
      paste(labels[zeroExact], collapse = ", "), "; no hit-share bars.") else "",
    if (any(zeroSample)) paste0(" No observed sample hits within listed parts for ",
      paste(labels[zeroSample], collapse = ", "), "; hit shares are undefined.") else ""))
  titles <- c(source = "source", corpusTitle = "corpus title", pubPlaceKey = "publication country",
              region = "region", year = "publication year", pubDate = "publication year",
              creationDate = "creation year")
  title <- if (partition %in% names(titles)) unname(titles[partition]) else partition
  height <- max(380, 170 + length(buckets) * (20 * (nrow(rows) + 1) + 16))
  hc <- korapHighchart(height = height, series = nrow(rows)) |>
    highcharter::hc_chart(type = "bar", height = height, spacingRight = 65) |>
    highcharter::hc_title(text = chartText(paste("Distribution by", title))) |>
    highcharter::hc_xAxis(categories = as.list(categories), title = list(text = NULL),
      labels = list(formatter = chartLabelFormatter("value"), style = list(width = "210px", textOverflow = "ellipsis"))) |>
    highcharter::hc_yAxis(min = 0, max = 100, tickInterval = 20,
      title = list(text = "Share within listed parts (%)"),
      labels = list(format = "{value}%", overflow = "allow")) |>
    highcharter::hc_legend(align = "left", verticalAlign = "top", maxHeight = 80) |>
    highcharter::hc_tooltip(useHTML = FALSE, headerFormat = "<b>{point.point.tooltipLabel}</b><br/>", pointFormat = "{point.tooltipText}") |>
    highcharter::hc_plotOptions(series = list(animation = FALSE, borderWidth = 0,
      groupPadding = .16, pointPadding = .06,
      dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%", crop = FALSE, overflow = "allow", allowOverlap = TRUE,
        style = list(textOutline = "none")))) |>
    highcharter::hc_caption(text = if (nzchar(caption)) chartText(caption) else NULL) |>
    highcharter::hc_responsive(rules = list(list(condition = list(maxWidth = 520),
      chartOptions = list(xAxis = list(labels = list(style = list(width = "110px"))))))) |>
    highcharter::hc_add_series(name = "Corpus tokens", color = "#B8B8B8",
      data = points(tokens / sum(tokens) * 100))
  for (j in seq_len(nrow(rows))) {
    frequencies <- grouped(counts[, j])
    values <- if (totals[j] > 0) frequencies / totals[j] * 100 else rep(NA_real_, length(buckets))
    hc <- highcharter::hc_add_series(hc, name = labels[j], colorIndex = j - 1L,
      data = points(values, frequencies, grouped(observed[, j]), j))
  }
  chartLocalize(hc, list(kind = "dispersion", partition = partition,
    title = chartText(paste("Distribution by", title)), defaultY = "Share within listed parts (%)",
    caption = if (nzchar(caption)) chartText(caption) else NULL,
    remaining = length(remaining), shown = length(shown), parts = nrow(reference),
    estimated = any(rows$estimated), unequal = length(unique(probabilities)) > 1L,
    excluded = excluded, zeroExact = as.list(labels[zeroExact]), zeroSample = as.list(labels[zeroSample])))
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
