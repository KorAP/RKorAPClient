#' Plot interactive frequency curves with confidence intervals
#'
#' @description
#'
#' Convenience function for plotting typical frequency by year graphs with confidence intervals using highcharter.
#'
#' **Warning:** This function may be moved to a new package.
#' @inheritSection hc_freq_bar_ci Display language
#'
#' @family  highcharter-helpers
#' @import highcharter
#' @importFrom tibble add_column
#' @export
#'
#' @param df data frame like the value of a [frequencyQuery()]
#' @param as.alternatives NULL (default) inherits the denominator mode from
#'   [frequencyQuery()]. An explicit conflicting mode is rejected. Older tables
#'   without a recorded mode default to FALSE.
#' @param ylabel defaults to `%` if `as.alternatives` is `TRUE` and to `IPM` (instances per million tokens) otherwise.
#' @param smooth boolean decides whether the graph is smoothed using the highcharts plot types spline and areasplinerange.
#' @param showCI Show confidence bands (default TRUE). FALSE omits bands and
#'   confidence-interval tooltip lines; interval columns are not required.
#' @param ... additional arguments passed to [highcharter::hc_add_series()]
#'
#' @return A highchart htmlwidget object containing the frequency plot.
#'
#' @examples
#' \dontrun{
#'
#' year <- c(1990:2018)
#' alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")
#' KorAPConnection(verbose = TRUE) %>%
#'   frequencyQuery(query = alternatives,
#'                  vc = paste("textType = /Zeit.*/ & pubDate in", year),
#'                  as.alternatives = TRUE) %>%
#'   hc_freq_by_year_ci(as.alternatives = TRUE)
#'
#'
#' kco <- KorAPConnection(verbose = TRUE)
#' expand_grid(
#'   condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"),
#'   year = (2005:2011)
#' ) %>%
#'   cbind(frequencyQuery(
#'     kco,
#'     "[tt/l=Heuschrecke]",
#'     paste0(.$condition, " & pubDate in ", .$year)
#'   ))  %>%
#'   hc_freq_by_year_ci()
#' }
#'
hc_freq_by_year_ci <- function(df, as.alternatives = NULL,
                               ylabel = if(as.alternatives) "%" else "IPM",
                               smooth = FALSE, showCI = TRUE,
                               ...) {
  title <- ""
  validateShowCI(showCI)
  as.alternatives <- frequencyChartMode(df, as.alternatives)
  if (showCI) {
    df <- if (!as.alternatives) ipm(df) else RKorAPClient::percent(df)
  } else if (as.alternatives) df$f <- df$f * 100 else df$ipm <- df$f * 1e6

  if (!"year" %in% colnames(df)) {
    df <- df %>% add_column(year = as.integer(queryStringToLabel(df$vc, pubDateOnly = TRUE)))
  }
  if (!"condition" %in% colnames(df)) {
    if (length(base::unique(df$query)) > 1) {
      df <- df %>% mutate(condition = if ("variant" %in% names(df)) .data$variant else query)
      if(length(base::unique(queryStringToLabel(df$vc, excludePubDate = TRUE ))) > 1) {
        df <- df %>% mutate(condition = paste(condition, " & ",
                                              queryStringToLabel(vc, excludePubDate = TRUE )))
      }
    } else {
      if (length(base::unique(queryStringToLabel(df$vc, excludePubDate = TRUE ))) > 1) {
        title <- base::unique(df$query)
        df <- df %>% add_column(condition = queryStringToLabel(vc, excludePubDate = TRUE ))
      } else {
        df <- df %>% mutate(condition = if ("variant" %in% names(df)) .data$variant else query)
      }
    }
  }
  hc <- korapHighchart(colors = chartYearPalette()) %>%
    hc_title(text = chartText(title)) %>%
    hc_yAxis(
      title = list(text = if (as.alternatives) "" else chartText(ylabel)),
      ceiling = if (as.alternatives) 100 else NULL,
      floor = 0,
      labels = if (as.alternatives) list(format = paste0("{value}\U2009", ylabel)) else NULL
    ) %>%
    hc_xAxis(allowDecimals = FALSE) %>%
    hc_tooltip(headerFormat = "<b>{point.x}</b><br/>",
      pointFormat = '<span style="color:{series.color}">\u25CF</span> {point.tooltipText}<br/>',
      crosshairs = TRUE, shared = TRUE) %>%
    hc_add_series_korap_frequencies(df, smooth, as.alternatives, ylabel = ylabel, showCI = showCI, ...)
  chartLocalize(hc, list(kind = "year", caption = NULL))
}

## Mute notes: "no visible binding for global variable:"
globalVariables(c("value", "query", "condition", "vc"))

hc_add_series_korap_frequencies <- function(hc, df, smooth = FALSE,
                                            as.alternatives = FALSE,
                                            ylabel = if (as.alternatives) "%" else "IPM",
                                            showCI = TRUE,
                                            ...) {
  index <- 0
  type <- ifelse(smooth, "spline", "line")
  areatype <- ifelse(smooth, "areasplinerange", "arearange")
  for(q in unique(df$condition)) {
    dat <- df[df$condition==q,]
    urls <- if ("webUIRequestUrl" %in% names(dat)) dat$webUIRequestUrl else rep(NA_character_, nrow(dat))
    urls <- vapply(urls, function(url) {
      u <- chartUrl(url); if (is.null(u)) NA_character_ else u
    }, character(1))
    tooltipText <- vapply(seq_len(nrow(dat)), function(i) frequencyChartTooltip(q,
      if (as.alternatives) dat$f[i] else dat$ipm[i], dat$totalResults[i],
      if ("total" %in% names(dat)) dat$total[i] else NA_real_,
      if (showCI) dat$conf.low[i] else NULL, if (showCI) dat$conf.high[i] else NULL,
      ylabel, as.alternatives, showCI), character(1))
    hc <- hc %>% hc_add_series(
      marker = list(radius = 2),
      name = q,
      data = data.frame(
        year = dat$year,
        value = if (as.alternatives) dat$f else dat$ipm,
        count = dat$totalResults,
        total = if ("total" %in% names(dat)) dat$total else NA_real_,
        confLow = if (showCI) dat$conf.low else NA_real_,
        confHigh = if (showCI) dat$conf.high else NA_real_,
        korapTooltipType = "frequency",
        korapTooltipName = q,
        korapAlternatives = as.alternatives,
        korapShowCI = showCI,
        korapTooltipUnit = ylabel,
        webUIRequestUrl = unname(urls),
        tooltipText = tooltipText
      ),
      hcaes(year, value),
      type = type,
      colorIndex = index,
      zIndex = 1,
      ...
    )
    if (showCI) hc <- hc %>% hc_add_series(
        name = "ci",
        data = dat[,c('year', 'conf.low', 'conf.high')],
        hcaes(x = year, low = conf.low, high = conf.high),
        type = areatype,
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE,
        linkedTo= ':previous',
        colorIndex = index,
        zIndex = 0
      )
    index <- index+1
  }
  hc
}
