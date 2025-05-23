#!/usr/bin/env Rscript
#
# Plot proportions of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)

alternativesOverTime <- function(alternatives, years, kco = KorAPConnection(verbose=TRUE)) {
  df <- expand_grid(Variant = alternatives, year = years) %>%
    cbind(frequencyQuery(kco, .$Variant, sprintf("textType = /Zeit.*/ & pubDate in %d", .$year), as.alternatives=TRUE)) %>%
    rename(share=f)
  g <- ggplot(data = df, mapping = aes(x = year, y = share, colour = Variant, fill = Variant)) +
    geom_freq_by_year_ci() +
    ggtitle(paste0(alternatives, collapse = " vs. ")) +
    xlab("TIME") +
    ylab(sprintf("Observed frequency ratio"))
  print(g)
  df
}

df <- alternativesOverTime(c('so "genannte.?"', '"sogenannte.?"'), (1995:2018))
