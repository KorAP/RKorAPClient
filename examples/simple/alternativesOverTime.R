#!/usr/bin/env Rscript
#
# Plot frequency of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)
library(reshape2)

alternativesOverTime <- function(alternatives, years, kco = new("KorAPConnection", verbose=TRUE)) {
  df = data.frame(year=years)
  vc = "textType = /Zeit.*/ & pubDate in"
  for (v in alternatives) {
    df[v] <- sapply(df$year, function(y)  corpusQuery(kco, query=v, vc=paste(vc, y))@totalResults)
  }
  df$total <- apply(df[,alternatives], 1, sum)
  df <- melt(df, measure.vars = alternatives, value.name = "afreq", variable.name = "Variant")
  df$ci <- t(sapply(Map(prop.test, df$afreq, df$total), "[[","conf.int"))
  df$share <- df$afreq / df$total
  g <- ggplot(data = df, mapping = aes(x = year, y = share, color=Variant, fill=Variant)) +
    geom_ribbon(aes(ymin=ci[, 1], ymax=ci[, 2], color=Variant, fill=Variant), alpha=.3, linetype=0) +
    geom_line() +
    geom_point() +
    xlab("TIME") +
    ylab(sprintf("Observed frequency ratio")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks=unique(df$year))
  print(g)
  # print(ggplotly(g, tooltip = c("x", "y")))
  df
}

df <- alternativesOverTime(c("so genannter", "sogenannter"), (1995:2018))
