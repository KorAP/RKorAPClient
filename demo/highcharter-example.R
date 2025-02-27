library(RKorAPClient)
library(htmlwidgets)

plotHighchart <- function(query = "Schlumpf",
                          years = c(2000:2010),
                          as.alternatives = length(query) > 1,
                          vc = "textType = /Zeit.*/ & availability!=QAO-NC-LOC:ids & pubDate in",
                          kco = KorAPConnection(verbose=TRUE) ) {
  hc <-
    frequencyQuery(kco, query, paste(vc, years), as.alternatives=as.alternatives) %>%
    hc_freq_by_year_ci(as.alternatives)
  print(hc)
  hc
}

saveHCPlot <- function(hc, fname) {
  htmlwidgets::saveWidget(hc, file=fname, selfcontained = TRUE)
}

#h1 <-plotHighchart(c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn"), c(1980:2018))
h1 <- plotHighchart(c("Leser | Lesern | Lesers", 'Leserin | Leserinnen', 'LeserIn | LeserInnen', '"Leser[_\\*]in.*"'), c(1985:2018), as.alternatives = FALSE)
#plotHighchart(c("Tollpatsch", "Tolpatsch"), c(1991:2018))


