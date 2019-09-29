#!/usr/bin/env Rscript
#
# Plot frequency of query expressions over time
#
library(RKorAPClient)
library(ggplot2)

freqPerYear <- function(query, con = new("KorAPConnection", verbose = TRUE)) {
  vc <- "pubDate since 2000 & pubDate until 2018 & textType = /Zeit.*/"
  q <- corpusQuery(con, query = query, vc=vc)
  q <- fetchAll(q)
  tokensPerYear <- function(year) {
    return(corpusStats(con, sprintf("%s & pubDate in %s", vc, year))@tokens)
  }
  df <- as.data.frame(table(as.numeric(format(q@collectedMatches$pubDate,"%Y")), dnn="year"),
                      stringsAsFactors = FALSE)
  df <- merge(data.frame(year=min(df$year):max(df$year)), df, all = TRUE)
  df[is.na(df$Freq),]$Freq <- 0
  df$total <- sapply(df$year, tokensPerYear)
  df$freq <- df$Freq / df$total
  df$ci <- t(sapply(Map(prop.test, df$Freq, df$total), "[[","conf.int"))
  g <- ggplot(data = df, aes(x = year, y = freq, group=1)) +
    geom_ribbon(aes(ymin=ci[, 1], ymax=ci[, 2]), alpha=.3) +
    geom_point() +
    geom_line() +
    xlab("TIME") +
    ylab(sprintf("Observed frequency of \u201c%s\u201d", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(g)
  df
}
#df <- freqPerYear("Car-Bikini")
#df <- freqPerYear("[tt/p=ART & opennlp/p=ART] [tt/l=teilweise] [tt/p=NN]")
df <- freqPerYear("Buschzulage")

