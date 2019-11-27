library(RKorAPClient)
library(rCharts)

plotHighchart <- function(query = "Schlumpf",
                          years = c(2000:2010),
                          as.alternatives = length(query) > 1,
                          vc = "textType = /Zeit.*/ & pubDate in",
                          kco = new("KorAPConnection", verbose=T) ) {
  palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  h1 <- Highcharts$new()
  df <-
    frequencyQuery(kco, query, paste(vc, years), as.alternatives=as.alternatives) %>%
    { if(! as.alternatives) ipm(.) else percent(.) } %>%
    mutate(year=as.numeric(queryStringToLabel(vc)))

  ylabel = if(as.alternatives) "%" else "ipm"
  for(q in query) {
    dat <- df[df$query==q,]
    h1$series(
      marker = list(radius = 2),
      name = q,
      data = toJSONArray(data.frame(x=dat$year,
                                    y = if (as.alternatives) dat$f else dat$ipm,
                                    percentage = dat$f ,
                                    count = dat$totalResults,
                                    click=dat$webUIRequestUrl), json = F),
      type = 'line',
      color = palette[1+length(h1$params$series)/2],
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
      color = palette[1+(length(h1$params$series)-1)/2],
      zIndex = 0
    )
  }
  h1$plotOptions(line = list(cursor = 'pointer', point = list(
    events = list(click = "#! function() { window.open(this.click, 'korap'); } !#")
  )))
  h1$set(
    credits = list(text="KorAP R Client Pakckage", href="//github.com/KorAP/RKorAPClient/"),
    zoomType = "Y",
#    xAxis = list(title = list(text="year")),
    yAxis = if (as.alternatives)
      list(ceiling=100, floor=0, labels = list(format="{value}\U2009%"))
    else
      list(title = list(text=ylabel), floor=0),
    tooltip = list(
      formatter = paste0("#! function (tooltip) {
        var str = tooltip.defaultFormatter.call(this, tooltip);
        if(Array.isArray(str))  {
          str = str.join('');
        }
        for (var i = 0; i < this.points.length; i++) {
            str = str.replace(/([0-9.,]+.?)", ylabel, "/, this.points[i].point.count+' ($1@)');
        }
        return str.replace(/@/g, '", ylabel, "')
      } !#"),
      crosshairs =  T,
      valueDecimals = 2,
      shared = T,
      valueSuffix = paste0('\U2009', ylabel)
    )
  )
  print(h1)
  h1
}

saveHPlot <- function(h, fname) {
  cat(gsub("=//", "=https://", paste(capture.output(h$show('inline', include_assets = TRUE, cdn = TRUE)), collapse = '\n')), file=fname)
}

#h1 <-plotHighchart(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"), c(1980:2018))
h1 <- plotHighchart(c("Leser | Lesern | Lesers", 'Leserin | Leserinnen', 'LeserIn | LeserInnen', '"Leser[_\\*]in.*"'), c(1985:2018))
#plotHighchart(c("Tollpatsch", "Tolpatsch"), c(1991:2018))

