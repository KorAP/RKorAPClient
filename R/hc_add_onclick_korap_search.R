#' Add KorAP search click events to highchart plots
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Adds on-click events to data points of highcharts that were constructed with
#' [frequencyQuery()] or [collocationScoreQuery()]. Clicks on data points
#' then launch KorAP web UI queries for the given query term and virtual corpus in
#' a separate tab.
#'
#' @family highcharter-helpers
#'
#' @param hc      higchart object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(highcharter)
#' library(tidyr)
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#'   collocationScoreQuery("Team", "agil", vc = paste("pubDate in", c(2014:2018)),
#'                         lemmatizeNodeQuery = TRUE, lemmatizeCollocateQuery = TRUE) %>%
#'                          pivot_longer(c("O", "E")) %>%
#'   hchart(type="spline", hcaes(label, value, group=name)) %>%
#'   hc_add_onclick_korap_search()
#' }
#'
hc_add_onclick_korap_search <- function(hc) {
  hc_plotOptions(
    hc,
    series = list(enabled = TRUE),
    spline = list(cursor = 'pointer', point = list(events = list(
      click = JS("function() { window.open(this.webUIRequestUrl, 'korap'); }")
    ))),
    line = list(cursor = 'pointer', point = list(events = list(
      click = JS("function() { window.open(this.webUIRequestUrl, 'korap'); }")
    ))))
}
