library(RKorAPClient)
library(highcharter)

KorAPConnection(verbose = TRUE) |>
  frequencyQuery("sozusagen/i", vc = c("corpusSigle=FOLK", "corpusSigle!=FOLK")) |>
  ipm() -> df

highchart() |>
  hc_xAxis(categories = c("FOLK (gesprochen)", "DeReKo (geschrieben)")) |>
  hc_yAxis(
    min = 0,
    title = list(text = "Treffer pro Million W\u00f6rter")
  ) |>
  hc_add_series(
    data = df,
    type = "column",
    hcaes(y = ipm),
    name = "sozusagen"
  ) |>
  hc_add_series(
    data = df,
    type = "errorbar",
    hcaes(low = conf.low, high = conf.high),
    name = "95%-Konfidenzintervall",
    showInLegend = FALSE
  ) |>
  hc_add_onclick_korap_search() |>
  hc_title(text = "'sozusagen': gesprochen vs. geschrieben") -> hc

print(hc)
