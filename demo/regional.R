#!/usr/bin/Rscript
library(RKorAPClient)
library(ggplot2)
library(sf)
# library(R.cache)

devAskNewPage(ask = FALSE)

mapfile <- file.path(tempdir(), "map-gadm41-sf-v1.rds")

# Caching data in the user's home filespace by default
# is not allowed to package demos by CRAN policies ...
#
# mapfile <- file.path(R.cache::getCachePath(), "map-v2.rds")

fetchAndPrepareMap <- function(map, pick) {
  cat("Downloading GADM 4.1 map data for ", map, "\n")
  parts <- strsplit(map, "_")[[1]]
  iso <- parts[1]
  level <- as.integer(parts[2])
  json_url <- sprintf("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_%s_%d.json", iso, level)
  sfobj <- tryCatch({
    suppressWarnings(sf::st_read(json_url, quiet = TRUE))
  }, error = function(e) {
    stop(sprintf("Failed to read %s: %s", json_url, conditionMessage(e)))
  })
  if (pick > 0) {
    sfobj <- sfobj[pick, ]
  }
  # Keep only geometry to standardize columns across layers
  sfobj <- sfobj["geometry"]
  sfobj
}

fetchMaps <- function(maps, picks) {
  if (file.exists(mapfile)) {
    df <- readRDS(mapfile)
  } else {
  cat("Downloading and caching GADM 4.1 map data.\nPlease note that the GADM map data is licensed for academic use and other non-commercial use, only.\nSee https://gadm.org/license.html\n")
    # Fetch individual sf layers and row-bind
    sflist <- mapply(fetchAndPrepareMap, maps, picks, SIMPLIFY = FALSE)
    df <- do.call(rbind, sflist)
    # Create a stable group index compatible with original regions index logic
    df$grp <- seq_len(nrow(df))
    dir.create(dirname(mapfile), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, mapfile)
  }
  # If cache is from an older version (non-sf tidy data), refresh
  if (!inherits(df, "sf")) {
    cat("Cached map is in outdated format; re-downloading as sf...\n")
    sflist <- mapply(fetchAndPrepareMap, maps, picks, SIMPLIFY = FALSE)
    df <- do.call(rbind, sflist)
    df$grp <- seq_len(nrow(df))
    dir.create(dirname(mapfile), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, mapfile)
  } else if (is.null(df$grp)) {
    df$grp <- seq_len(nrow(df))
  }
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
    geom_sf(aes(fill = ipm), colour = "black", linewidth = .1) +
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
    coord_sf() +
    labs(title = sprintf("Regional distribution of \u201c%s\u201d", query))
  print(regionsPlot)
  regionsPlot
}

#geoDistrib("wegen dem [tt/p=NN]")
geoDistrib("heuer")
#geoDistrib("Sonnabend")
#geoDistrib("eh")
