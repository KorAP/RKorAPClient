# Requires RKorAPClient >= 1.5.0 or a current development build.

library(RKorAPClient)

hc <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery("sozusagen/i",
    vc = c("FOLK (gesprochen)" = "corpusSigle=FOLK",
           "DeReKo (geschrieben)" = "corpusSigle!=FOLK")) |>
  hc_freq_bar_ci(horizontal = FALSE, ylabel = "Treffer pro Million W\u00f6rter") |>
  highcharter::hc_title(text = "'sozusagen': gesprochen vs. geschrieben")

print(hc)
