#!/usr/bin/env Rscript
#
# Plot proportions of alternative expressions or spellings variants over time
#
library(RKorAPClient)
library(ggplot2)
library(plotly)
library(htmlwidgets)

alternativesOverTime <- function(alternatives, years, kco = new("KorAPConnection", verbose=TRUE)) {
  df <- expand_grid(Variant = alternatives, year = years) %>%
    cbind(corpusQuery(kco, .$Variant, sprintf("textType = /Zeit.*/ & pubDate in %d", .$year))) %>%
    group_by(year) %>% mutate(tokens = sum(totalResults)) %>%
    ci()
  g <- ggplot(data = df, mapping = aes(x = year, y = f, color = Variant, fill = Variant)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color = Variant, fill = Variant), alpha = .3, linetype = 0) +
    geom_line() +
    geom_point() +
    ggtitle(paste0(alternatives, collapse = " vs. ")) +
    xlab("TIME") +
    ylab(sprintf("Observed frequency ratio")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = unique(df$year))
  pp <- ggplotly(g, tooltip = c("x", "y"))
  for (i in 1:length(alternatives)) {
    vdata <- df[df$Variant == alternatives[i],]
    pp$x$data[[2+i]]$customdata <- vdata$webUIRequestUrl
    pp$x$data[[2+i]]$text <- sprintf("%s<br />absolute: %d / %d", pp$x$data[[2+i]]$text, vdata$totalResults, vdata$tokens)
  }
  ppp <- onRender(pp, "function(el, x) { el.on('plotly_click', function(d) { var url=d.points[0].customdata; window.open(url, 'korap') })}")
  print(ppp)
  df
}

df <- alternativesOverTime(c('so "genannte.?"', '"sogenannte.?"'), (1995:2018))
