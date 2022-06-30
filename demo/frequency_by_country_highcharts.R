library(RKorAPClient)
library(highcharter)

QUERIES <- c("[tt/l=verunfallen]", "Sonnabend") # in TreeTagger-Lemma-Annotationen suchen
COUNTRIES <- c("AT", "BE", "CH", "DE", "IT", "LU")

VCS <- sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", COUNTRIES) # limit virtual corpus to newspapers and magazines


df <- new("KorAPConnection", verbose=TRUE) %>%
  frequencyQuery(QUERIES, vc=VCS) %>%
  ipm() %>%
  mutate(country = rep(COUNTRIES, length(QUERIES)))


hc_add_series_with_errorbar <- function(hc, data, group, ...) {
  for (g in unique(data$country)) {
    hc <-
      hc %>%
      hc_add_series(type = "column", data = data %>% filter(.[, group] == g),  hcaes(group = eval(as.symbol(group)), x = query, y = ipm), ...) %>%
      hc_add_series(type = "errorbar",
                    data = data %>% filter(.[, group] == g),
                    stemWidth = 1,
                    whiskerWidth = 1,
                    whiskerLength = 10, ...
      )
  }
  hc
}

hc <- highchart() %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_yAxis(title=list(text = "IPM"), type = "logarithmic") %>%
  hc_add_series_with_errorbar (data = df, group="country", hcaes(group = eval(as.symbol(group)), x = query, y = ipm, low = conf.low, high = conf.high)) %>%
  hc_add_onclick_korap_search() %>%
  hc_xAxis(categories = QUERIES) %>%
  hc_legend(enabled = TRUE) %>%
  hc_title(text = "Relative frequency by country of publication") %>%
  hc_caption(text= "Click on a bar to launch the respectively corresponding KorAP query.")


print(hc)


