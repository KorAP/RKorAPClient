#!/usr/bin/env Rscript
#
# Plot frequency of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)
library(reshape2)
library(plotly)
library(htmlwidgets)

alternativesOverTime <- function(alternatives, years, kco = new("KorAPConnection", verbose=TRUE)) {
  df = data.frame(year=years)
  vc = "textType = /Zeit.*/ & pubDate in"
  urls <- data.frame()
  for (v in alternatives) {
    df[v] <- sapply(df$year, function(y) {
        kqo <- corpusQuery(kco, query=v, vc=paste(vc, y))
        urls <<- rbind(urls, data.frame(Variant=v, year=y, url=kqo@webUIRequestUrl))
        kqo@totalResults
    })
  }
  df$total <- apply(df[,alternatives], 1, sum)
  df <- merge(melt(df, measure.vars = alternatives, value.name = "afreq", variable.name = "Variant"),
              urls, by=c("Variant", "year"))
  df$ci <- t(sapply(Map(prop.test, df$afreq, df$total), "[[","conf.int"))
  df$share <- df$afreq / df$total
  g <- ggplot(data = df, mapping = aes(x = year, y = share, color=Variant, fill=Variant)) +
    geom_ribbon(aes(ymin=ci[, 1], ymax=ci[, 2], color=Variant, fill=Variant), alpha=.3, linetype=0) +
    geom_line() +
    geom_point() +
    ggtitle(paste0(alternatives, collapse = " vs. ")) +
    xlab("TIME") +
    ylab(sprintf("Observed frequency ratio")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks=unique(df$year))
  pp <- ggplotly(g, tooltip = c("x", "y"))
  for (i in 1:length(alternatives)) {
    pp$x$data[[2+i]]$customdata=df[df$Variant==alternatives[i],]$url
  }
  ppp <- onRender(pp, "function(el, x) { el.on('plotly_click', function(d) { var url=d.points[0].customdata; window.open(url, 'korap') })}")
  print(ppp)
  df
}

df <- alternativesOverTime(c('so "genannte.?"', '"sogenannte.?"'), (1995:2018))
