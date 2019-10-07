#!/usr/bin/env Rscript
#
# Plot frequency of an expressions under multiple conditions over time
#
#library(devtools)
#install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", upgrade="never")
library(RKorAPClient)
library(ggplot2)

conditionsOverTime <- function(query, conditions, years, kco = new("KorAPConnection", verbose = TRUE)) {
  g <- expand_grid(condition = conditions, year = years) %>%
    cbind(frequencyQuery(kco, query, sprintf("%s & pubDate in %d", .$condition, .$year))) %>%
    ipm() %>%
    ggplot(aes(x = year, y = ipm, fill=condition, color=condition, ymin=conf.low, ymax=conf.high)) +
    geom_freq_by_year_ci(aes(url=webUIRequestUrl)) +
    xlab("TIME") +
    labs(color="Virtual Corpus", fill="Virtual Corpus") +
    ylab(sprintf("Observed frequency/million of \u201c%s\u201d", query))
  print(g)
  g
}

g <- conditionsOverTime("[tt/l=Heuschrecke]", c("textClass = /natur.*/", "textClass=/politik.*/", "textClass=/wirtschaft.*/"), (2002:2018))
