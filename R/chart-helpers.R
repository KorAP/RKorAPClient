# Shared presentation for the interactive plotting helpers. Keep raw category
# and series names for CSV export; escape them only at rendering boundaries.
chartText <- function(x) {
  x <- unname(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

chartUrl <- function(url) {
  if (is.character(url) && length(url) == 1L && !is.na(url) &&
      grepl("^https?://[^[:space:]<>\"']+$", url, ignore.case = TRUE)) unname(url) else NULL
}

chartPalette <- function(n = 8L) {
  colors <- c("#1F77B4", "#E69F00", "#009E73", "#CC79A7",
              "#56B4E9", "#D55E00", "#0072B2", "#000000")
  if (n > length(colors)) grDevices::hcl.colors(n, "Dark 3") else colors
}

# Preserve the historical D3 palette of the year-series helper.
chartYearPalette <- function() {
  c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B",
    "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#AEC7E8", "#FFBB78",
    "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94", "#F7B6D2", "#C7C7C7",
    "#DBDB8D", "#9EDAE5")
}

chartLabelFormatter <- function(field) {
  highcharter::JS(paste0("function () { return String(this.", field,
    ").replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;').replace(/'/g, '&#39;'); }"))
}

korapHighchart <- function(height = NULL, series = 1L, colors = chartPalette(series)) {
  fallback <- highcharter::hc_theme_google(colors = colors,
    plotOptions = list(errorbar = list(color = "#737373"),
      arearange = list(color = "#808080", fillOpacity = 0.15, lineWidth = 0),
      areasplinerange = list(color = "#808080", fillOpacity = 0.15, lineWidth = 0)))
  theme <- getOption("highcharter.theme")
  # highcharter installs this theme on load, even when no theme was chosen.
  # Compare the whole theme, so customized fonts/backgrounds/palettes (including
  # IDS themes) still take precedence. Do not change the user's global option.
  startup <- highcharter::hc_theme(chart = list(backgroundColor = "transparent"),
    colors = getOption("highcharter.color_palette"))
  if (!is.null(theme) && !identical(theme, startup))
    fallback <- highcharter::hc_theme_merge(fallback, theme)
  highcharter::highchart(height = height, theme = fallback) |>
    highcharter::hc_chart(events = list(load = chartLocaleJS())) |>
    highcharter::hc_legend(labelFormatter = chartLabelFormatter("name")) |>
    hc_add_onclick_korap_search() |>
    highcharter::hc_exporting(enabled = TRUE) |>
    highcharter::hc_credits(enabled = TRUE, text = "KorAP R Client Package",
      href = "https://github.com/KorAP/RKorAPClient/")
}

chartLocaleJS <- function() {
  highcharter::JS(paste(readLines(system.file("js", "chart-locale.js",
    package = "RKorAPClient", mustWork = TRUE), encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}

chartLocalize <- function(hc, spec) {
  language <- getOption("rkorap.chart.language", "auto")
  if (!is.character(language) || length(language) != 1L || is.na(language) ||
      !language %in% c("auto", "de", "en")) stop("rkorap.chart.language must be 'auto', 'de' or 'en'.", call. = FALSE)
  hc$x$hc_opts$chart$korapLocale <- c(list(language = language), spec)
  hc
}

chartNumber <- function(x, count = FALSE) {
  if (is.na(x)) return("undefined")
  if (count) formatC(x, format = "f", digits = 0, big.mark = ",") else
    format(signif(x, 4), scientific = FALSE, trim = TRUE)
}

chartIntervalColour <- function(paper = "white") {
  rgb <- tryCatch(grDevices::col2rgb(paper), error = function(e) matrix(255, 3, 1))
  if (mean(rgb) < 128) "#808083" else "#737373"
}

frequencyChartMode <- function(df, requested = NULL) {
  recorded <- if ("as.alternatives" %in% names(df)) unique(df$as.alternatives) else
    attr(df, cacheAsAttribute)$parameters$as.alternatives
  if (!is.null(recorded) && (!is.logical(recorded) || length(recorded) != 1L || is.na(recorded))) {
    stop("Frequency results must use one recorded as.alternatives mode.", call. = FALSE)
  }
  if (!is.null(requested) && (!is.logical(requested) || length(requested) != 1L || is.na(requested))) {
    stop("as.alternatives must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(recorded) && !is.null(requested) && !identical(recorded, requested)) {
    stop("as.alternatives disagrees with frequencyQuery(); use its recorded mode or omit the plotting argument.", call. = FALSE)
  }
  if (!is.null(requested)) requested else if (!is.null(recorded)) recorded else FALSE
}

frequencyResultMetadata <- function(result, query, mode) {
  result$as.alternatives <- rep(mode, nrow(result))
  labels <- names(query)
  if (!is.null(labels)) {
    missing <- is.na(labels) | !nzchar(labels)
    labels[missing] <- unname(query[missing])
    result$variant <- unname(labels[match(result$query, query)])
  }
  result
}

frequencyChartTooltip <- function(name, value, count, total, low, high, unit,
                                  as.alternatives = FALSE, showCI = TRUE) {
  unit <- if (as.alternatives) "%" else if (tolower(unit) == "ipm") "IPM" else unit
  paste0(if (!is.null(name)) paste0("<b>", chartText(name), "</b><br/>") else "",
    "Relative frequency: <b>", chartNumber(value), "</b> ", chartText(unit),
    "<br/>Absolute frequency: ", chartNumber(count, TRUE),
    if (as.alternatives) "<br/>Total frequency of alternatives: " else "<br/>Corpus size (tokens): ",
    chartNumber(total, TRUE),
    if (showCI) paste0("<br/>Confidence interval: ", chartNumber(low), "\u2013", chartNumber(high), " ", chartText(unit)) else "")
}

validateShowCI <- function(showCI) {
  if (!is.logical(showCI) || length(showCI) != 1L || is.na(showCI))
    stop("showCI must be TRUE or FALSE.", call. = FALSE)
  invisible(showCI)
}
