#!/usr/bin/env Rscript
#
# Plot frequency of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(htmlwidgets)

alternativesOverTime <- function(alternatives, years, kco = new("KorAPConnection", verbose=TRUE)) {
  vc = "textType = /Zeit.*/ & pubDate in"
  df <- data.frame(matrix(ncol = length(alternatives), nrow = length(years))) %>%
    setNames(alternatives) %>%
    mutate(year = years) %>%
    pivot_longer(cols = alternatives) %>%
    mutate(value = corpusQuery(kco, query=name, vc=paste(vc, year))$totalResults) %>%
    pivot_wider(id_cols= year, names_from = name) %>%
    mutate(total = rowSums(.[alternatives])) %>%
    pivot_longer(cols = alternatives) %>%
    mutate(share = value / total) %>%
    mutate(url =  corpusQuery(kco, query=name, vc=paste(vc, year))$webUIRequestUrl) %>%
    rename(Variant = name)
  df$ci <- t(sapply(Map(prop.test, df$value, df$total), "[[","conf.int"))
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
    vdata <- df[df$Variant==alternatives[i],]
    pp$x$data[[2+i]]$customdata <- vdata$url
    pp$x$data[[2+i]]$text <- sprintf("%s<br />absolute: %d / %d", pp$x$data[[2+i]]$text, vdata$value, vdata$total)
  }
  ppp <- onRender(pp, "function(el, x) { el.on('plotly_click', function(d) { var url=d.points[0].customdata; window.open(url, 'korap') })}")
  print(ppp)
  df
}

df <- alternativesOverTime(c('so "genannte.?"', '"sogenannte.?"'), (1995:2018))
