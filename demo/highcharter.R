library(RKorAPClient)
library(highcharter)

hc_freq_by_year_ci <- function(df, as.alternatives = F, ylabel = if(as.alternatives) "%" else "ipm") {
  title <- ""
  df <- df %>%
    { if(! as.alternatives) ipm(.) else RKorAPClient::percent(.) }

  if (!"year" %in% colnames(df)) {
    df <- df %>% mutate(year = as.integer(queryStringToLabel(df$vc, pubDateOnly = T)))
  }
  if (!"condition" %in% colnames(df)) {
    if (length(base::unique(df$query)) > 1) {
      df <- df %>% mutate(condition = query)
      if(length(base::unique(queryStringToLabel(df$vc, excludePubDate = T ))) > 1) {
        df <- df %>% mutate(condition = paste(condition, " & ",
                                              queryStringToLabel(vc, excludePubDate = T )))
      }
    } else {
      title <- base::unique(df$query)
      if(length(base::unique(queryStringToLabel(df$vc, excludePubDate = T ))) > 1) {
        df <- df %>% mutate(condition = queryStringToLabel(vc, excludePubDate = T ))
      }
    }
  }
  # use the D3 palette which provides 20 attractive and distinguishable colours
  palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5")
  highchart() %>%
    hc_title(text=title) %>%
    hc_chart(zoomType="xy") %>%
    hc_yAxis(
      title = list(text = if (as.alternatives) "" else ylabel),
      ceiling = if (as.alternatives) 100 else NULL,
      floor = 0,
      labels = if(as.alternatives) list(format = paste0("{value}\U2009", ylabel)) else NULL
    ) %>%
    hc_xAxis(allowDecimals=F) %>%
    hc_add_theme(hc_theme_google(colors=palette)) %>%
    hc_plotOptions(
      series = list(enabled = T),
      line = list(cursor = 'pointer', point = list(events = list(
      click = JS("function() { window.open(this.click, 'korap'); }")
    )))) %>%
    hc_credits(enabled = T,
               text = "KorAP R Client Pakckage",
               href = "//github.com/KorAP/RKorAPClient/") %>%
    hc_exporting(enabled = T) %>%
    hc_tooltip(
      formatter = JS(paste0("function (tooltip) {
        var str = tooltip.defaultFormatter.call(this, tooltip);
        if(Array.isArray(str))  {
          str = str.join('');
        }
       for (var i = 0; i < this.points.length; i++) {
         str = str.replace(/([0-9.,]+.?)", ylabel, "/, this.points[i].point.count+' ($1@)');
       }
       return str.replace(/@/g, '", ylabel, "')
      } ")),
      crosshairs =  T,
      valueDecimals = 2,
      shared = T,
      valueSuffix = paste0('\U2009', ylabel)
    ) %>%
    hc_add_series_korap_frequencies(df, as.alternatives)
}

hc_add_series_korap_frequencies <- function(hc, df, as.alternatives = F) {
  index <- 0
  for(q in unique(df$condition)) {
    dat <- df[df$condition==q,]
    hc <- hc %>% hc_add_series(
      marker = list(radius = 2),
      name = q,
      data = data.frame(
        year = dat$year,
        value = if (as.alternatives) dat$f else dat$ipm,
        count = dat$totalResults,
        click = dat$webUIRequestUrl
      ),
      hcaes(year, value),
      type = 'line',
      colorIndex = index,
      zIndex = 1
    ) %>%
      hc_add_series(
        name = "ci",
        data = dat[,c('year', 'conf.low', 'conf.high')],
        hcaes(x = year, low = conf.low, high = conf.high),
        type = 'arearange',
        fillOpacity = 0.3,
        lineWidth = 0,
        marker = list(enabled = F),
        enableMouseTracking = F,
        linkedTo= ':previous',
        colorIndex = index,
        zIndex = 0
      )
    index <- index+1
  }
  hc
}

plotHighchart <- function(query = "Schlumpf",
                          years = c(2000:2010),
                          as.alternatives = length(query) > 1,
                          vc = "textType = /Zeit.*/ & availability!=QAO-NC-LOC:ids & pubDate in",
                          kco = new("KorAPConnection", verbose=T) ) {
  hc <-
    frequencyQuery(kco, query, paste(vc, years), as.alternatives=as.alternatives) %>%
    hc_freq_by_year_ci(as.alternatives)
  print(hc)
  hc
}

saveHCPlot <- function(hc, fname) {
  htmlwidgets::saveWidget(hc, file=fname, selfcontained = T)
}

#h1 <-plotHighchart(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"), c(1980:2018))
h1 <- plotHighchart(c("Leser | Lesern | Lesers", 'Leserin | Leserinnen', 'LeserIn | LeserInnen', '"Leser[_\\*]in.*"'), c(1985:2018), as.alternatives = F)
#plotHighchart(c("Tollpatsch", "Tolpatsch"), c(1991:2018))

