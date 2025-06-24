library(RKorAPClient)
library(highcharter)
library(tidyverse)

QUERIES <- c("(das|einen|den) Cola") # search in treetagger lemma annotations
COUNTRIES <- c("AT", "BE", "CH", "DE", "IT", "LU")

VCS <- sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", COUNTRIES) # limit virtual corpus to newspapers and magazines


df <- KorAPConnection(verbose=TRUE) %>%
  frequencyQuery(QUERIES, vc=VCS) %>%
  ipm() %>%
  mutate(country = rep(COUNTRIES, length(QUERIES)))


hc_add_series_with_errorbar <- function(hc, data) {
  for (g in unique(data$country)) {
    df <- data %>% filter(country == g)
    hc <-
      hc %>%
      hc_add_series(type = "column", data = df, hcaes(group = country, x = query, y = ipm)) %>%
      hc_add_series(type = "errorbar",
                    data = df,
                    stemWidth = 1,
                    whiskerWidth = 1,
                    whiskerLength = 10, hcaes(low = conf.low, high = conf.high)
      )
  }
  hc
}

hc <- highchart() %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_yAxis(title=list(text = "IPM"), type = "logarithmic") %>%
  hc_add_series_with_errorbar(data = df) %>%
  hc_add_onclick_korap_search() %>%
  hc_xAxis(categories = QUERIES) %>%
  hc_legend(enabled = TRUE) %>%
  hc_title(text = "Relative frequency by country of publication") %>%
  hc_caption(text= "Click on a bar to launch the respectively corresponding KorAP query.")


print(hc)


