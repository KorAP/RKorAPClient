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
library("SemiPar")
library(raster)
basenameDB <- "basename"

myRand <- function() {
  return(runif(1, -0.02, 0.02))
}
gpclibPermit()
nc1 <- readOGR("examples/geo/map/DEU_adm1.shp")
nc2 <- readOGR("examples/geo/map/AUT_adm1.shp")
nc3 <- readOGR("examples/geo/map/CHE_adm1.shp")
nc4 <- readOGR("examples/geo/map/LUX_adm1.shp")

nc1 <- fortify(gSimplify(nc1, 0.01, topologyPreserve=TRUE))
nc2 <- fortify(gSimplify(nc2, 0.01, topologyPreserve=TRUE))
nc3 <- fortify(gSimplify(nc3, 0.01, topologyPreserve=TRUE))
nc4 <- fortify(gSimplify(nc4, 0.01, topologyPreserve=TRUE))
par(mar=c(0,0,0,0))

geoDistrib <- function(query, kco = new("KorAPConnection", verbose=TRUE)){
  con <- dbConnect(dbDriver("MariaDB"),dbname = "corpora", host="klinux10", user="viewer")
  dbExecute(con, "SET NAMES 'utf8'")
  places <- dbGetQuery(con, paste("SELECT ort, lat, lng FROM `basename`  WHERE ort!=''AND lng GROUP by ort ORDER BY ort"))
#  dbDisconnect(con)
  places$afreq <- 0
  places$total <- 1
  kqo <- corpusQuery(kco, query, vc="textType=/Zeitung.*/")
  plot = NULL
  while (kqo@hasMoreMatches) {
    kqo <- fetchNext(kqo)
    for (i in 1:nrow(kqo@apiResponse$matches)) {
      if (!is.na(kqo@apiResponse$matches[i,]$pubPlace) && nrow(places[places$ort==kqo@apiResponse$matches[i,]$pubPlace,]) > 0) {
        pubPlace <- (kqo@apiResponse$matches)[i,]$pubPlace
        places[places$ort==pubPlace,]$afreq <- places[places$ort==pubPlace,]$afreq + 1
        if (places[places$ort==pubPlace,]$total == 1) {
          places[places$ort==pubPlace,]$total <- corpusStats(kco, vc=sprintf("textType=/Zeitung.*/ & pubPlace=/%s/", pubPlace))@tokens
        }
      }
    }
    plot <- updatePlot(plot, places)
  }
}

updatePlot <- function(myPlot, places) {
  populatedPlaces <- places[places$afreq>0,]
  if(nrow(populatedPlaces) == 0) {
    return(NULL)
  }
  populatedPlaces$freq <- populatedPlaces$afreq / populatedPlaces$total
  if (is.null(myPlot)) {
    myPlot<-ggplot(data=populatedPlaces, aes(x = lng, y = lat, group=1, fill = freq)) +
    geom_polygon(data=nc1, aes(x=long, y=lat, group=group), colour= "#708090", size=.1) +
    geom_polygon(data=nc2, aes(x=long, y=lat, group=group), colour= "#708090", fill="#e0ffff", size=.1) +
    geom_polygon(data=nc3, aes(x=long, y=lat, group=group), colour= "#708090", fill="#ffe0ff", size=.1) +
    geom_polygon(data=nc4, aes(x=long, y=lat, group=group), colour= "#708090", fill="#ffffe0", size=.1)  +
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

geoDistrib("[tt/p=ART] teilweise [tt/p=NN]")

#geoDistrib("wegen dem [tt/p=NN]")

