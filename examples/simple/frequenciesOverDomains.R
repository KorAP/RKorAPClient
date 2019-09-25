#!/usr/bin/env Rscript
#
# Plot frequency of query expressions per topic domain
#
library(RKorAPClient)
library(ggplot2)

freqPerDomain <- function(query, con = new("KorAPConnection", verbose = TRUE)) {
  q <- corpusQuery(con, query = query, vc="")
  q <- fetchAll(q)
  tokensPerMainTopic <-
    function(topic) {
      return(corpusStats(con, sprintf("textClass = /%s.*/", topic))@tokens)
    }
  q@collectedMatches$primaryTopic <-
    sapply(strsplit(as.character(q@collectedMatches$textClass), " "), `[[`, 1)
  df <- as.data.frame(table(q@collectedMatches$primaryTopic, dnn = "Domain"))
  df$total <- sapply(df$Domain, tokensPerMainTopic)
  df$freq <- df$Freq / df$total
  df$ci <- t(sapply(Map(prop.test, df$Freq, df$total), "[[","conf.int"))
  g <- ggplot(data = df, mapping = aes(x = Domain, y = freq)) +
    geom_col() +
    geom_errorbar(aes(ymin=ci[, 1], ymax=ci[, 2]), width=.5, alpha=.5) +
    ylab(sprintf("Observed frequency of “%s”", query)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(g)
  df
}
df <- freqPerDomain("Hatespeech")

