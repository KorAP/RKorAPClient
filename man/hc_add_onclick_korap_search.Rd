% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hc_add_onclick_korap_search.R
\name{hc_add_onclick_korap_search}
\alias{hc_add_onclick_korap_search}
\title{Add KorAP search click events to highchart plots}
\usage{
hc_add_onclick_korap_search(hc)
}
\arguments{
\item{hc}{A highchart htmlwidget object generated by e.g. \code{\link[=frequencyQuery]{frequencyQuery()}}.}
}
\value{
The input highchart object with added on-click events.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Adds on-click events to data points of highcharts that were constructed with
\code{\link[=frequencyQuery]{frequencyQuery()}} or \code{\link[=collocationScoreQuery]{collocationScoreQuery()}}. Clicks on data points
then launch KorAP web UI queries for the given query term and virtual corpus in
a separate tab.
}
\examples{
\dontrun{

library(highcharter)
library(tidyr)

KorAPConnection(verbose = TRUE) \%>\%
  collocationScoreQuery("Team", "agil", vc = paste("pubDate in", c(2014:2018)),
                        lemmatizeNodeQuery = TRUE, lemmatizeCollocateQuery = TRUE) \%>\%
                         pivot_longer(c("O", "E")) \%>\%
  hchart(type="spline", hcaes(label, value, group=name)) \%>\%
  hc_add_onclick_korap_search()
}

}
\seealso{
Other highcharter-helpers: 
\code{\link{hc_freq_by_year_ci}()}
}
\concept{highcharter-helpers}
