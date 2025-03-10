#!/usr/bin/env Rscript
#
# Plot frequency of query expressions over time
#
library(RKorAPClient)
library(ggplot2)

freqPerYear <- function(query, kco = KorAPConnection(verbose = TRUE)) {
  g <- data.frame(year = 2000:2018) %>%
    cbind(frequencyQuery(kco, query, sprintf("pubDate in %d", .$year))) %>%
    { . ->> df } %>%
    ipm() %>%
    ggplot(aes(year, ipm)) +
    geom_freq_by_year_ci() +
    xlab("TIME") +
    ylab(sprintf("Observed frequency/million of \u201c%s\u201d", query))
  print(g)
  df
}
#df <- freqPerYear("Car-Bikini")
#df <- freqPerYear("[tt/p=ART & opennlp/p=ART] [tt/l=teilweise] [tt/p=NN]")
df <- freqPerYear("Buschzulage")

