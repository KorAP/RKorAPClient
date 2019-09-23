#!/usr/bin/Rscript
library(RKorAPClient)
library(ggplot2)

geoDistrib <- function(query, kco = new("KorAPConnection", verbose=TRUE)) {
  map <- readRDS("examples/geo/data/map.rds")
  regions <- readRDS("examples/geo/data/regions.rds")
  regions$freq <- NA
  plot <- NULL
  vc <- ""
  for (i in 1:nrow(regions)) {
    if (!is.na(regions[i,]$query)) {
      cat(as.character(regions[i,]$land), "\n")
      regions[i,]$total <- corpusStats(kco, vc=paste0(vc, regions[i,]$query))@tokens
      if (regions[i,]$total == 0) {
        regions[i,]$afreq <- 0
        regions[i,]$freq <- NA
      } else {
        regions[i,]$afreq <- corpusQuery(kco, query, vc=paste0(vc, regions[i,]$query))@totalResults
        regions[i,]$freq <- regions[i,]$afreq / regions[i,]$total
      }
      cat(regions[i,]$afreq, regions[i,]$total, regions[i,]$freq, "\n")
      plot <- updatePlot(query, plot, map, regions)
      cat("\n\n")
    }
  }
}

updatePlot <- function(query, myPlot, map, laender) {
  map$ipm <- sapply(map$grp, function(grp) laender$freq[grp] * 10^6)
  myPlot <- ggplot(map) +
    geom_polygon(aes(x=long, y=lat, group=group, fill=ipm), colour= "black", size=.1) +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    coord_equal(ratio=1.5) +
    labs(title = sprintf("Regional distribution of “%s”", query))
  print(myPlot)
  myPlot
}

#geoDistrib("wegen dem [tt/p=NN]")
#geoDistrib("heuer")
#geoDistrib("Sonnabend")
geoDistrib("eh")
