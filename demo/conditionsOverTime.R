#!/usr/bin/env Rscript
#
# Plot frequency of an expressions under multiple conditions over time
#
#library(devtools)
#install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", upgrade="never")
library(RKorAPClient)
library(ggplot2)
#library(plotly)

conditionsOverTime <- function(query, conditions, years, kco = new("KorAPConnection", verbose = TRUE)) {
  g <- expand_grid(condition = conditions, year = years) %>%
    cbind(frequencyQuery(kco, query, sprintf("%s & pubDate in %d", .$condition, .$year))) %>%
    ipm() %>%
    ggplot(aes(x = year, y = ipm, fill=condition, color=condition)) +
    geom_freq_by_year_ci() +
    xlab("TIME") +
    labs(color="Virtual Corpus", fill="Virtual Corpus") +
    ylab(sprintf("Observed frequency of \u201c%s\u201d", query))
  p <- RKorAPClient::ggplotly(g)
  print(p)
}
#df <- conditionsOverTime("wegen dem [tt/p=NN]", c("textClass = /sport.*/", "textClass=/politik.*/", "textClass=/kultur.*/"), (1995:2005))

conditionsOverTime("[tt/l=Heuschrecke]", c("textClass = /natur.*/", "textClass=/politik.*/", "textClass=/wirtschaft.*/"), (2002:2018))
