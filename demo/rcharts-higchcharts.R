library(RKorAPClient)
library(rCharts)

plotHighchart <- function(query = "Schlumpf",
                          years = c(2000:2010),
                          vc = "textType = /Zeit.*/ & pubDate in",
                          kco = new("KorAPConnection", verbose=T)) {
  h1 <- Highcharts$new()

  for(q in query) {
    dat <-
      frequencyQuery(kco, q, paste(vc, years)) %>%
      ipm() %>%
      mutate(year=as.numeric(queryStringToLabel(vc)))
    h1$series(
      name = q,
      data = toJSONArray(data.frame(x=dat$year, y=dat$ipm, click=dat$webUIRequestUrl), json = F),
      type = 'line',
      zIndex = 1
    )
    h1$series(
      name = "ci",
      data = toJSONArray2(dat[,c('year', 'conf.low', 'conf.high')], names = F, json = F),
      type = 'arearange',
      fillOpacity = 0.3,
      lineWidth = 0,
      marker = list(enabled = F),
      enableMouseTracking = F,
      linkedTo= ':previous',
      color = paste0("#! (Highcharts.getOptions().colors)[", (1+length(h1$params$series))/2-1, "] !#"),
      zIndex = 0
    )
  }
  h1$plotOptions(line = list(cursor = 'pointer', point = list(
    events = list(click = "#! function() { window.open(this.click, 'korap'); } !#")
  )))
  h1$set(
    xAxis = list(title = list(text="")),
    yAxis = list(title = list(text="ipm")),
    tooltip = list(
      crosshairs =  T,
      valueDecimals = 3,
      shared = T,
      valueSuffix = '\U2009ipm'
    )
  )
  print(h1)
  h1
}

saveHPlot <- function(h, fname) {
  cat(gsub("=//", "=https://", paste(capture.output(h$show('inline', include_assets = TRUE, cdn = TRUE)), collapse = '\n')), file=fname)
}

h1 <-plotHighchart(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"), c(1970:2018))

