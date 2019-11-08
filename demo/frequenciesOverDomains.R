#!/usr/bin/env Rscript
#
# Plot frequency of query expressions per topic domain
#
library(RKorAPClient)
library(ggplot2)

freqPerDomain <- function(query, con = new("KorAPConnection", verbose = TRUE)) {
  g <- corpusQuery(con, query = query, vc="") %>%
    fetchAll() %>%
    slot("collectedMatches") %>%
    mutate(Domain = factor(sapply(strsplit(as.character(.$textClass), " "), `[[`, 1))) %>%
    group_by(Domain) %>%
    dplyr::filter(!is.na(Domain)) %>%
    summarise(count = dplyr::n()) %>%
    mutate(tokens = (corpusStats(con, sprintf("textClass = /%s.*/", .$Domain)))$tokens) %>%
    ci(x = count) %>%
    ipm() %>%
    { df <<- . } %>%
    ggplot(aes(x = Domain, y = ipm, ymin = conf.low, ymax = conf.high)) +
    geom_col() +
    geom_errorbar(width = .3, alpha = .3) +
    ylab(sprintf("Observed frequency/million of \u201c%s\u201d", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(g)
  df
}
df <- freqPerDomain("Hatespeech")

