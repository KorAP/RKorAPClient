#!/usr/bin/env Rscript
#
# Plot frequency of an expressions under multiple conditions over time
#
#library(devtools)
#install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", upgrade="never")
library(RKorAPClient)
library(ggplot2)
library(reshape2)
#library(plotly)

conditionsOverTime <- function(query, conditions, years, kco = new("KorAPConnection", verbose = TRUE)) {
  g <- expand_grid(condition = conditions, year = years) %>%
  cbind(frequencyQuery(kco, query, sprintf("%s & pubDate in %d", .$condition, .$year))) %>%
  ggplot(aes(x = year, y = f, fill=condition, color=condition)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=condition, color=condition), alpha=.3, linetype=0) +
    xlab("TIME") +
    labs(color="Virtual Corpus", fill="Virtual Corpus") +
    ylab(sprintf("Observed frequency of \u201c%s\u201d", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_x_continuous(breaks=unique(df$year))
  print(g)
  # print(ggplotly(g, tooltip = c("x", "y")))
}
#df <- conditionsOverTime("wegen dem [tt/p=NN]", c("textClass = /sport.*/", "textClass=/politik.*/", "textClass=/kultur.*/"), (1995:2005))

conditionsOverTime("[tt/l=Heuschrecke]", c("textClass = /natur.*/", "textClass=/politik.*/", "textClass=/wirtschaft.*/"), (2002:2018))
