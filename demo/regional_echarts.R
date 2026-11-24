#!/usr/bin/Rscript
# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

library(RKorAPClient)
library(echarts4r)
## sf is used via sf::; don't attach package in demo to avoid undeclared library() calls
# library(R.cache)

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
  for (pkg in c("geojsonsf", "htmlwidgets")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required for this demo. Install with install.packages('%s')", pkg, pkg))
  }

  regions <- readRDS("demo/data/regions.rds")
  regions$ipm <- NA_real_
  regions$webUIRequestUrl <- NA_character_
  for (i in which(!is.na(regions$query))) {
    cat(regions$region[i], "\n")
    tokens <- corpusStats(kco, vc = regions$query[i])@tokens
    if (tokens > 0) {
      kqo <- corpusQuery(kco, query, vc = regions$query[i])
      regions$ipm[i] <- kqo@totalResults / tokens * 1e6
      regions$webUIRequestUrl[i] <- kqo@webUIRequestUrl
    }
  }
  plot <- updatePlot(query, map, regions)
  if (interactive()) {
    print(plot)
  } else {
    outfile <- file.path(tempdir(), "regional_map_echarts.html")
    htmlwidgets::saveWidget(plot, outfile, selfcontained = TRUE)
    cat("Saved interactive map to:", outfile, "\n")
  }
  plot
}

updatePlot <- function(query, map, regions) {
  # ECharts joins values to areas by name
  map$name <- regions$region[map$grp]
  gj <- jsonlite::fromJSON(geojsonsf::sf_geojson(map[c("name", "geometry")]), simplifyVector = FALSE)
  known <- regions[!is.na(regions$ipm), ]

  known |>
    e_charts(region, renderer = "svg") |>
    e_map_register("regions", gj) |>
    e_map(ipm, map = "regions", name = "ipm", roam = TRUE, aspectScale = 0.66,
          itemStyle = list(areaColor = "#eeeeee", borderColor = "#000000", borderWidth = 0.2)) |>
    e_add_unnested("webUIRequestUrl", webUIRequestUrl) |>
    e_visual_map(ipm, inRange = list(color = viridisLite::viridis(20))) |>
    e_title(sprintf('Regional distribution of "%s"', query)) |>
    e_tooltip(formatter = htmlwidgets::JS("function (p) {
      return '<b>' + echarts.format.encodeHTML(p.name) + '</b><br/>' +
        (isNaN(p.value) ? '\u2013' : p.value.toFixed(1) + ' per Mio'); }")) |>
    ec_add_onclick_korap_search()
}

geoDistrib("heuer")
