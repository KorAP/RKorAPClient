#!/usr/bin/env Rscript
#
# Plot frequency of an expressions under multiple conditions over time
#
#library(devtools)
#install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", upgrade="never")
library(RKorAPClient)
library(ggplot2)
library(reshape2)

conditionsOverTime <- function(query, conditions, years, kco = new("KorAPConnection", verbose = TRUE)) {
  df = data.frame(year=years)
  for (c in conditions) {
    df[c] <- sapply(df$year, function(y)
      corpusQuery(kco, query, vc=paste(c, "& pubDate in", y))@totalResults)

  }
  df <- melt(df, measure.vars = conditions, value.name = "afreq", variable.name = "condition")
  df$total <- apply(df[,c('year','condition')], 1, function(x) corpusStats(kco, vc=paste(x[2], "& pubDate in", x[1]))@tokens )
  df$ci <- t(sapply(Map(prop.test, df$afreq, df$total), "[[","conf.int"))
  df$freq <- df$afreq / df$total
  g <- ggplot(data = df, mapping = aes(x = year, y = freq, color=condition)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=ci[, 1], ymax=ci[, 2]), width=.5, alpha=.5) +
    xlab("TIME") +
    labs(color="Virtual Corpus") +
    ylab(sprintf("Observed frequency of “%s”", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_x_continuous(breaks=unique(df$year))
  print(g)
  df
}

df <- conditionsOverTime("[tt/l=Heuschrecke]", c("textClass = /natur.*/", "textClass=/politik.*/", "textClass=/wirtschaft.*/"), (2002:2018))
#df <- conditionsOverTime("wegen dem [tt/p=NN]", c("textClass = /sport.*/", "textClass=/politik.*/", "textClass=/kultur.*/"), (1995:2005))
