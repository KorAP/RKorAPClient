#!/usr/bin/env Rscript
# Requires RKorAPClient >= 1.5.0 or a current development build.
#
# Plot frequency of query expressions per topic domain
#
library(RKorAPClient)
library(ggplot2)

freqPerDomain <- function(query, con = KorAPConnection(verbose = TRUE)) {
  g <- corpusQuery(con, query = query, vc = "", fields = "dmozDomain") %>%
    fetchAll() %>%
    slot("collectedMatches") %>%
    mutate(Domain = factor(sub(" .*", "", as.character(.$dmozDomain)))) %>%
    group_by(Domain) %>%
    dplyr::filter(!is.na(Domain), as.character(Domain) != "") %>%
    summarise(count = dplyr::n()) %>%
    mutate(total = (corpusStats(con, sprintf("dmozDomain = /%s.*/", .$Domain)))$tokens) %>%
    ci(x = count) %>%
    ipm() %>%
    { df <- . } %>%
    ggplot(aes(x = Domain, y = ipm, ymin = conf.low, ymax = conf.high)) +
    geom_freq_bar_ci() +
    ylab(sprintf("Observed frequency/million of \u201c%s\u201d", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(g)
  df
}
df <- freqPerDomain("Hatespeech")

