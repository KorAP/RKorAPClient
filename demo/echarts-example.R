# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

library(RKorAPClient)
library(htmlwidgets)

plotEchart <- function(query = "Schlumpf",
                          years = c(2000:2010),
                          as.alternatives = length(query) > 1,
                          vc = "textType = /Zeit.*/ & availability!=QAO-NC-LOC:ids & pubDate in",
                          kco = KorAPConnection(verbose=TRUE) ) {
  e <-
    frequencyQuery(kco, query, paste(vc, years), as.alternatives=as.alternatives) %>%
    ec_freq_by_year_ci(as.alternatives)
  print(e)
  e
}

saveEchart <- function(e, fname) {
  htmlwidgets::saveWidget(e, file=fname, selfcontained = TRUE)
}

#e1 <- plotEchart(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"), c(1980:2018))
e1 <- plotEchart(c("Leser | Lesern | Lesers", 'Leserin | Leserinnen', 'LeserIn | LeserInnen', '"Leser[_\\*]in.*"'), c(1985:2018), as.alternatives = FALSE)
#plotEchart(c("Tollpatsch", "Tolpatsch"), c(1991:2018))

# Click the None and All buttons next to the legend to switch all curves.
# A personal theme for all charts, see ?ec_freq_by_year_ci:
# options(rkorap.chart.theme = list(color = c("#003366", "#CC6600", "#669900", "#993366")))
