#!/usr/bin/Rscript
library(RKorAPClient)
library(ggplot2)
library(raster)
library(broom)
# library(R.cache)

devAskNewPage(ask = FALSE)

mapfile <- file.path(tempdir(), "map-v2.rds")

# Caching data in the user's home filespace by default
# is not allowed to package demos by CRAN policies ...
#
# mapfile <- file.path(R.cache::getCachePath(), "map-v2.rds")

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

map <- fetchMaps(c("DEU_1", "AUT_0", "CHE_0", "LUX_0", "BEL_3", "ITA_1", "LIE_0"), c(0, 0, 0, 0, 34, 17, 0))

geoDistrib <- function(query, kco = KorAPConnection(verbose=TRUE)) {
  regions <- readRDS("demo/data/regions.rds")
  regions$freq <- NA
  regions$url <- NA
  plot <- NULL
  vc <- ""
  for (i in 1:nrow(regions)) {
    if (!is.na(regions[i,]$query)) {
      cat(as.character(regions[i,]$region), "\n")
      regions[i,]$total <- corpusStats(kco, vc=paste0(vc, regions[i,]$query))@tokens
      if (regions[i,]$total == 0) {
        regions[i,]$afreq <- 0
        regions[i,]$freq <- NA
      } else {
        kqo <- corpusQuery(kco, query, vc=paste0(vc, regions[i,]$query))
        regions[i,]$afreq <- kqo@totalResults
        regions[i,]$freq <- regions[i,]$afreq / regions[i,]$total
        regions[i,]$url <- kqo@webUIRequestUrl
      }
      cat(regions[i,]$afreq, regions[i,]$total, regions[i,]$freq, "\n")
      cat("\n\n")
    }
  }
  plot <- updatePlot(query, map, regions)
  print(plot)
  plot
}

updatePlot <- function(query, map, regions) {
  map$ipm <- sapply(map$grp, function(grp) regions$freq[grp] * 10^6)
  map$region <- sapply(map$grp, function(grp) regions$region[grp])
  map$url <- sapply(map$grp, function(grp) regions$url[grp])
  regionsPlot <- ggplot(map) +
    geom_polygon(aes(x=long, y=lat, group=group, fill=ipm, hack=region), colour= "black", linewidth=.1) +
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
    labs(title = sprintf("Regional distribution of \u201c%s\u201d", query))
  print(regionsPlot)
  regionsPlot
}

#geoDistrib("wegen dem [tt/p=NN]")
geoDistrib("heuer")
#geoDistrib("Sonnabend")
#geoDistrib("eh")
