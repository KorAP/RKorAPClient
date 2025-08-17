#!/usr/bin/Rscript
library(RKorAPClient)
library(ggplot2)
library(sf)
# library(R.cache)

devAskNewPage(ask = FALSE)

# --- CRAN-compliant caching -----------------------------------------------
# Default to tempdir() during demos/tests. Allow users to opt-in to a
# persistent cache in a per-user directory via either
#   options(RKorAPClient.regional.cache = "user")
# or environment variable
#   RKORAPCLIENT_CACHE=user
# Any value among {"user","persistent","cache","true","1","yes"}
# enables persistent caching. Everything else uses tempdir().
get_cache_dir <- function() {
  mode <- tolower(getOption(
    "RKorAPClient.regional.cache",
    Sys.getenv("RKORAPCLIENT_CACHE", "temp")
  ))
  if (mode %in% c("user", "persistent", "cache", "true", "1", "yes")) {
    d <- tools::R_user_dir("RKorAPClient", which = "cache")
  } else {
    d <- tempdir()
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

mapfile <- file.path(get_cache_dir(), "map-gadm41-sf-v1.rds")

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
  cat("Using cached map from:", mapfile, "\n")
  } else {
  cat("Downloading and caching GADM 4.1 map data.\nPlease note that the GADM map data is licensed for academic use and other non-commercial use, only.\nSee https://gadm.org/license.html\n")
    # Fetch individual sf layers and row-bind
    sflist <- mapply(fetchAndPrepareMap, maps, picks, SIMPLIFY = FALSE)
    df <- do.call(rbind, sflist)
    # Create a stable group index compatible with original regions index logic
    df$grp <- seq_len(nrow(df))
    saveRDS(df, mapfile)
  cat("Saved map cache to:", mapfile, "\n")
  }
  # If cache is from an older version (non-sf tidy data), refresh
  if (!inherits(df, "sf")) {
    cat("Cached map is in outdated format; re-downloading as sf...\n")
    sflist <- mapply(fetchAndPrepareMap, maps, picks, SIMPLIFY = FALSE)
    df <- do.call(rbind, sflist)
    df$grp <- seq_len(nrow(df))
    saveRDS(df, mapfile)
  cat("Saved map cache to:", mapfile, "\n")
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

# --- Alternative: Highcharter interactive map version ---
geoDistrib_hc <- function(query, kco = KorAPConnection(verbose=TRUE)) {
  # Ensure optional dependencies are present
  for (pkg in c("highcharter", "geojsonsf", "viridisLite", "htmlwidgets")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required for the highcharter demo. Install with install.packages('%s')", pkg, pkg))
  }

  regions <- readRDS("demo/data/regions.rds")
  regions$freq <- NA
  regions$url <- NA
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
  plot <- updatePlotHC(query, map, regions)
  if (interactive()) {
    print(plot)
  } else {
    outfile <- file.path(tempdir(), "regional_map_highcharter.html")
    htmlwidgets::saveWidget(plot, outfile, selfcontained = TRUE)
    cat("Saved interactive map to:", outfile, "\n")
  }
  plot
}

updatePlotHC <- function(query, map, regions) {
  # Build data to join to map via 'grp'
  df <- data.frame(
    grp = map$grp,
    ipm = sapply(map$grp, function(grp) regions$freq[grp] * 10^6),
    region = sapply(map$grp, function(grp) regions$region[grp]),
    url = sapply(map$grp, function(grp) regions$url[grp])
  )

  gj <- jsonlite::fromJSON(geojsonsf::sf_geojson(map), simplifyVector = FALSE)

  hc <- highcharter::highchart(type = "map") |>
    highcharter::hc_add_series_map(
      map = gj,
      df = df,
      value = "ipm",
      joinBy = "grp",
      name = "ipm",
      borderColor = "#000000",
      borderWidth = 0.2,
      nullColor = "#eeeeee"
    ) |>
    highcharter::hc_colorAxis(stops = highcharter::color_stops(200, colors = viridisLite::viridis(200))) |>
    highcharter::hc_title(text = sprintf('Regional distribution of "%s"', query)) |>
    highcharter::hc_mapNavigation(enabled = TRUE) |>
    highcharter::hc_tooltip(useHTML = TRUE, pointFormat = "<b>{point.region}</b><br/>{point.value:.1f} per Mio") |>
    highcharter::hc_plotOptions(
    	series = list(
      cursor = "pointer",
      point = list(events = list(click = htmlwidgets::JS("function(){ if (this.url){ window.open(this.url, '_blank'); }}")))
    ))

  # Set the raw Highcharts map projection via options (works across versions)
  hc$x$hc_opts$mapView <- list(projection = list(name = "WebMercator"))
  hc
}

#geoDistrib("wegen dem [tt/p=NN]")
geoDistrib("heuer")
#geoDistrib("Sonnabend")
#geoDistrib("eh")
# To use the Highcharter version, call:
geoDistrib_hc("heuer")
