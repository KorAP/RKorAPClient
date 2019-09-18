#!/usr/bin/env Rscript
#
# Plot frequency of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)
library(reshape2)

alternativesOverTime <- function(alternatives, years, kco = new("KorAPConnection", verbose=TRUE)) {
  df = data.frame(year=years)
  vc = "textType = /Zeitung.*/ & pubDate in"
  for (v in alternatives) {
    df[v] <- sapply(df$year, function(y)  corpusQuery(kco, query=v, vc=paste(vc, y))@totalResults)
  }
  df$total <- apply(df[,alternatives], 1, sum)
  df <- melt(df, measure.vars = alternatives, value.name = "afreq", variable.name = "Variant")
  df$ci <- t(sapply(Map(prop.test, df$afreq, df$total), "[[","conf.int"))
  df$share <- df$afreq / df$total
  g <- ggplot(data = df, mapping = aes(x = year, y = share, color=Variant)) +
    geom_point() +
    geom_line() +
    xlab("TIME") +
    ylab(sprintf("Observed frequency ratio")) +
    geom_errorbar(aes(ymin=ci[, 1], ymax=ci[, 2]), width=.5, alpha=.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_x_continuous(breaks=unique(df$year))
  print(g)
  df
}

df <- alternativesOverTime(c("so genannter", "sogenannter"), (1995:2018))
