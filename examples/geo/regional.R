#!/usr/bin/Rscript
library(RKorAPClient)
library(ggplot2)
library(raster)
library(broom)

mapfile <- "examples/geo/data/cache/map.rds"

fetchAndPrepareMap <- function(map, pick) {
  cat("Downloading GADM map data for ", map, "\n")
  sp <- readRDS(url(sprintf("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_%s_sp.rds", map)))
  if (pick > 0) {
    sp@polygons <- sp@polygons[pick]
    sp@data <- sp@data[pick,]
  }
  sp
}

fetchMaps <- function(maps, picks) {
  if (file.exists(mapfile)) {
    df <- readRDS(mapfile)
  } else {
    cat("Downloading and caching GADM map data.\nPlease note that the GADM map data is licensed for academic use and other non-commercial use, only.\nSee https://gadm.org/license.html\n")
    df <- broom::tidy(Reduce(bind, mapply(fetchAndPrepareMap, maps, picks)))
    dir.create(dirname(mapfile), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, mapfile)
  }
  df$grp <- floor(as.numeric(as.character(df$group)))
  df
}

map <- fetchMaps(c("DEU_1", "AUT_1", "CHE_1", "LUX_0", "BEL_3", "ITA_1"), c(0,0,0,0,34,17))

geoDistrib <- function(query, kco = new("KorAPConnection", verbose=TRUE)) {
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

updatePlot <- function(query, regionsPlot, map, laender) {
  map$ipm <- sapply(map$grp, function(grp) laender$freq[grp] * 10^6)
  regionsPlot <- ggplot(map) +
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
  print(regionsPlot)
  regionsPlot
}

#geoDistrib("wegen dem [tt/p=NN]")
geoDistrib("heuer")
#geoDistrib("Sonnabend")
#geoDistrib("eh")
