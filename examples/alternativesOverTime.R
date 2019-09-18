#!/usr/bin/env Rscript
#
# Plot frequency of alternative expressions or spellings over time
#
library(devtools)
install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", upgrade="never")
library(RKorAPClient)
library(ggplot2)
library(reshape2)

alternativesOverTime <- function(alternatives = c("so genannter", "sogenannter"), years = (1995:2018), kco = new("KorAPConnection")) {
  df = data.frame(year=years)
  vc = "textType = /Zeitung.*/ & pubDate in"
  for (v in alternatives) {
    df[v] <- sapply(df$year, function(y)  corpusQuery(kco, query=v, vc=paste(vc, y), verbose=TRUE)@totalResults)
  }
  df$total <- apply(df[,alternatives], 1, sum)
  df <- melt(df, measure.vars = alternatives, value.name = "afreq", variable.name = "alternative")
  df$ci <- t(sapply(Map(prop.test, df$afreq, df$total), "[[","conf.int"))
  df$share <- df$afreq / df$total
  df
}

df <- alternativesOverTime()
ggplot(data = df, mapping = aes(x = year, y = share, color=alternative)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=ci[, 1], ymax=ci[, 2]), width=.5, alpha=.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_x_continuous(breaks=unique(df$year))
