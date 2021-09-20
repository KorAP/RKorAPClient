#' Plot interactive frequency curves with confidence intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Convenience function for plotting typical frequency by year graphs with confidence intervals using highcharter.
#'
#' **Warning:** This function may be moved to a new package.
#'
#' @family  highcharter-helpers
#' @import highcharter
#' @importFrom tibble add_column
#' @export
#'
#' @param df data frame like the value of a [frequencyQuery()]
#' @param as.alternatives boolean decides whether queries should be treated as mutually exclusive and exhaustive wrt. to some meaningful class (e.g. spelling variants of a certain word form).
#' @param ylabel defaults to `%` if `as.alternatives` is `TRUE` and to `ipm` otherwise.
#' @param smooth boolean decides whether the graph is smoothed using the highcharts plot types spline and areasplinerange.
#' @param ... additional arguments passed to [hc_add_series()]
#'
#' @examples
#' \donttest{year <- c(1990:2018)}\dontshow{year <- c(2013:2013)}
#' \donttest{alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")}\dontshow{alternatives <- c("macht []{0,3} Sinn")}
#' new("KorAPConnection", verbose = TRUE) %>%
#'   frequencyQuery(query = alternatives,
#'                  vc = paste("textType = /Zeit.*/ & pubDate in", year),
#'                  as.alternatives = TRUE) %>%
#'   hc_freq_by_year_ci(as.alternatives = TRUE)
#'
#' \donttest{
#' kco <- new("KorAPConnection", verbose = TRUE)
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
hc_freq_by_year_ci <- function(df, as.alternatives = FALSE,
                               ylabel = if(as.alternatives) "%" else "ipm",
                               smooth = FALSE,
                               ...) {
  title <- ""
  df <- df %>%
    { if(! as.alternatives) ipm(.) else RKorAPClient::percent(.) }

  if (!"year" %in% colnames(df)) {
    df <- df %>% add_column(year = as.integer(queryStringToLabel(df$vc, pubDateOnly = TRUE)))
  }
  if (!"condition" %in% colnames(df)) {
    if (length(base::unique(df$query)) > 1) {
      df <- df %>% mutate(condition = query)
      if(length(base::unique(queryStringToLabel(df$vc, excludePubDate = TRUE ))) > 1) {
        df <- df %>% mutate(condition = paste(condition, " & ",
                                              queryStringToLabel(vc, excludePubDate = TRUE )))
      }
    } else {
      if (length(base::unique(queryStringToLabel(df$vc, excludePubDate = TRUE ))) > 1) {
        title <- base::unique(df$query)
        df <- df %>% add_column(condition = queryStringToLabel(vc, excludePubDate = TRUE ))
      } else {
        df <- df %>% mutate(condition = query)
      }
    }
  }
  # use the D3 palette which provides 20 attractive and distinguishable colours
  palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5")
  highcharter::highchart() %>%
    hc_title(text=title) %>%
    hc_yAxis(
      title = list(text = if (as.alternatives) "" else ylabel),
      ceiling = if (as.alternatives) 100 else NULL,
      floor = 0,
      labels = if(as.alternatives) list(format = paste0("{value}\U2009", ylabel)) else NULL
    ) %>%
    hc_xAxis(allowDecimals=FALSE) %>%
    hc_add_theme(hc_theme_google(colors=palette)) %>%
    hc_add_onclick_korap_search() %>%
    hc_credits(enabled = TRUE,
               text = "KorAP R Client Package",
               href = "https://github.com/KorAP/RKorAPClient/") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_tooltip(
      headerFormat = '<span style="font-size: 10pt">{point.key}</span><br/>',
      formatter = JS(paste0("function (tooltip) {
        var str = tooltip.defaultFormatter.call(this, tooltip);
        if(Array.isArray(str))  {
          str = str.join('');
        }
       for (var i = 0; i < this.points.length; i++) {
         str = str.replace(/([0-9.,]+.?)", ylabel, "/, this.points[i].point.count+' ($1@)');
       }
       return str.replace(/@/g, '", ylabel, "')
      } ")),
      crosshairs =  TRUE,
      valueDecimals = 2,
      shared = TRUE,
      valueSuffix = paste0('\U2009', ylabel)
    ) %>%
    hc_add_series_korap_frequencies(df, smooth, as.alternatives, ...)
}

## Mute notes: "no visible binding for global variable:"
globalVariables(c("value", "query", "condition", "vc"))

hc_add_series_korap_frequencies <- function(hc, df, smooth = FALSE,
                                            as.alternatives = FALSE,
                                            ...) {
  index <- 0
  type <- ifelse(smooth, "spline", "line")
  areatype <- ifelse(smooth, "areasplinerange", "arearange")
  for(q in unique(df$condition)) {
    dat <- df[df$condition==q,]
    hc <- hc %>% hc_add_series(
      marker = list(radius = 2),
      name = q,
      data = data.frame(
        year = dat$year,
        value = if (as.alternatives) dat$f else dat$ipm,
        count = dat$totalResults,
        webUIRequestUrl = dat$webUIRequestUrl
      ),
      hcaes(year, value),
      type = type,
      colorIndex = index,
      zIndex = 1,
      ...
    ) %>%
      hc_add_series(
        name = "ci",
        data = dat[,c('year', 'conf.low', 'conf.high')],
        hcaes(x = year, low = conf.low, high = conf.high),
        type = areatype,
        fillOpacity = 0.3,
        lineWidth = 0,
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
