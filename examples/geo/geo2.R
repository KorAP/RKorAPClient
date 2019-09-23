#!/usr/bin/Rscript
library(optparse);
svg=TRUE
myalpha=0.1
library(ggplot2)
#library(gridSVG)
library(utils)
library(proto)
#library(tikzDevice)
library(grid)
#library(RJSONIO)
#library(extrafont)
#library(maptools)
library(rgdal)
library(rgeos)
library(RMariaDB)
library(raster)
library("SemiPar")
library(geonames)

options(geonamesUsername="kupietz")

basenameDB <- "basename"

convertMaps <- function(countries) {
  mergedMaps <- NULL
  allStates <- c()
  offs <- 1
  for (country in countries) {
    nc <- readOGR(sprintf("examples/geo/map/%s_adm1.shp", country))
    states <- as.character(nc@data$NAME_1)
    allStates <- append(allStates, states)
    if (is.null(mergedMaps)) {
      mergedMaps <- nc
    } else {
      mergedMaps <-union(mergedMaps, nc)
    }
  }
  simplified <- fortify(gSimplify(mergedMaps, 0.01, topologyPreserve=TRUE))
  simplified$grp <- floor(as.numeric(as.character(simplified$group)))
  saveRDS(simplified, file="map.rds")
  saveRDS(allStates, file="laender.rds")
  simplified
}

myRand <- function() {
  return(runif(1, -0.02, 0.02))
}
gpclibPermit()
#nc1 <- convertMaps(c("DEU", "AUT", "CHE", "LUX"))

par(mar=c(0,0,0,0))

getPlaceInfo <- function(city) {
  gn <- GNsearch(q=city, lang="de", maxRows=1)
  if (nrow(gn) == 0) {
    return(NULL)
  }

  data.frame(lat=as.numeric(as.character(gn$lat)), lng=as.numeric(as.character(gn$lng)), land_id=match(gn$adminName1, laender$land))
}

geoDistrib <- function(query, kco = new("KorAPConnection", verbose=TRUE)) {
#  con <- dbConnect(dbDriver("MariaDB"),dbname = "corpora", host="klinux10", user="viewer")
#  dbExecute(con, "SET NAMES 'utf8'")
  places <- NULL
#  places <- dbGetQuery(con, paste("SELECT ort, lat, lng FROM `basename`  WHERE ort!=''AND lng GROUP by ort ORDER BY ort"))
#  dbDisconnect(con)
#  places$afreq <- 0
#  places$total <- 1
  nc1 <- readRDS("map.rds")
  laender <- data.frame(land <- readRDS("laender.rds"), afreq=0, total=0, freq=0)
  kqo <- corpusQuery(kco, query, vc="textType=/Zeitung.*/")
  plot = NULL
  while (kqo@hasMoreMatches) {
    kqo <- fetchNext(kqo)
    for (i in 1:nrow(kqo@apiResponse$matches)) {
      if (!is.na(kqo@apiResponse$matches[i,]$pubPlace)) {
        pubPlace <- gsub(" / ", "", (kqo@apiResponse$matches)[i,]$pubPlace)
        if (is.null(places) || nrow(places[places$ort == pubPlace,]) == 0) {
          placeInfo <- getPlaceInfo(pubPlace)
          if (!is.null(placeInfo)) {
            placeInfo$ort <- pubPlace
            placeInfo$afreq <- 1
            placeInfo$total <- corpusStats(kco, vc=sprintf("textType=/Zeitung.*/ & pubPlace=/%s/", pubPlace))@tokens
            if (!is.null(placeInfo$land_id)) {
              laender$total[placeInfo$land_id] <- laender$total[placeInfo$land_id] + placeInfo$total
              laender$afreq[placeInfo$land_id] <- laender$afreq[placeInfo$land_id] + 1
              laender$freq[placeInfo$land_id] <- laender$afreq[placeInfo$land_id] / laender$total[placeInfo$land_id]
            }
            if (is.null(places)) {
              places <- placeInfo
            } else {
              places <- rbind(places, placeInfo)
            }
          }
        } else {
          places[places$ort==pubPlace,]$afreq <- places[places$ort==pubPlace,]$afreq + 1
          if (!is.null(placeInfo$land_id)) {
            laender$afreq[placeInfo$land_id] <- laender$afreq[placeInfo$land_id] + 1
            laender$freq[placeInfo$land_id] <- laender$afreq[placeInfo$land_id] / laender$total[placeInfo$land_id]
          }
        }
      }
    }
    plot <- updatePlot(plot, nc1, places, laender)
  }
}

updatePlot <- function(myPlot, nc1, places, laender) {
  populatedPlaces <- places[places$afreq>0,]
#  save(places, file="test.Rda")
  if(nrow(populatedPlaces) == 0) {
    return(NULL)
  }
  populatedPlaces$freq <- populatedPlaces$afreq / populatedPlaces$total
  nc1$freq <- sapply(nc1$grp, function(grp) laender$freq[grp])
  if (TRUE || is.null(myPlot)) {
    myPlot<-ggplot(data=populatedPlaces) +
    geom_polygon(data=nc1, aes(x=long, y=lat, group=group, fill=nc1$freq), colour= "#708090", size=.1) +
    scale_size(range=c(3,12), guide=FALSE) + # scale_color_manual(values=c("DeReKo-Bestand"="orange", "neu"="red")) +
    #guides(color=guide_legend(""),size=guide_legend(title="Millionen Wörter")) +
    scale_alpha_continuous(guide = FALSE) +
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
    coord_equal(ratio=1/cos(populatedPlaces$lat[1]*pi/180))	+
    coord_equal(ratio=1.5)
  }
  myPlot <- myPlot+  geom_point(data=populatedPlaces,
                aes(size=freq, x=lng+myRand(), y=lat+myRand(),alpha=myalpha))
  print(myPlot)
  return(myPlot)
}


#geoDistrib("[tt/p=ART] teilweise [tt/p=NN]")

geoDistrib("wegen dem [tt/p=NN]")

