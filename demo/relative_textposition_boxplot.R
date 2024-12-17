library(RKorAPClient)
library(highcharter)
library(tidyverse)

kco <- new("KorAPConnection", verbose = TRUE)
set.seed(7)
get_relative_positions_sample <- function(kco, query, sampleSize = 400) {
  res <- corpusQuery(kco, query)
  res <- fetchNext(res, maxFetch = sampleSize, randomizePageOrder = TRUE)
  matches <- res@collectedMatches
  matches <- matches %>%
    mutate(
      query = query,
      vc = paste0('textSigle="', textSigle, '"'),
      textSize = corpusStats(kco, vc, as.df = TRUE)$tokens,
      relativeTextPosition = matchStart / textSize
    )
  return(matches)
}

df <- c(
  "anfangs/i",
  "zuguterletzt/i",
  "zunächst/i",
  "zuerst/i",
  "zuletzt/i",
  "schließlich/i"
) %>%
  map(~ get_relative_positions_sample(kco, .)) %>%
  bind_rows()

hc_data <- df %>%
  group_by(query) %>%
  summarise(
    min = min(relativeTextPosition),
    q1 = quantile(relativeTextPosition, 0.25),
    median = median(relativeTextPosition),
    q3 = quantile(relativeTextPosition, 0.75),
    max = max(relativeTextPosition)
  ) %>%
  mutate(data = pmap(list(min, q1, median, q3, max), c)) %>%
  select(query, data)

hc <- highchart() %>%
  hc_chart(type = "boxplot", inverted = TRUE) %>%
  hc_xAxis(categories = hc_data$query) %>%
  hc_yAxis(ceiling=1, title = list(text = "Relative position in text")) %>%
  hc_add_series(
    name = "Relative Text Position",
    data = hc_data$data,
    type = "boxplot"
  ) %>%
  hc_title(text = "Boxplot of Relative Text Positions")

print(hc)
