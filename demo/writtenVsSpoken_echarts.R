# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

library(RKorAPClient)

e <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery("sozusagen/i",
    vc = c("FOLK (gesprochen)" = "corpusSigle=FOLK",
           "DeReKo (geschrieben)" = "corpusSigle!=FOLK")) |>
  ec_freq_bar_ci(horizontal = FALSE, ylabel = "Treffer pro Million W\u00f6rter") |>
  echarts4r::e_title("'sozusagen': gesprochen vs. geschrieben")

print(e)
