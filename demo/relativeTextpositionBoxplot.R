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
  cat("\n\n", query, ":\n")
  print(summary(matches$relativeTextPosition))
  cat("\n\n")
  return(matches)
}

df <- c(
  "anfangs/i",
  "zuguterletzt/i",
  "zun\u00e4chst/i", # it is still necessary to encode non ascii characters in R package demos
  "zuerst/i",
  "zuletzt/i",
  "schlie\u00dflich/i"
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
  hc_yAxis(ceiling = 1, title = list(text = "Relative position in text")) %>%
  hc_add_series(data = hc_data$data) %>%
  hc_title(text = "Relative positions of some adverbs in DeReKo texts") %>%
  hc_legend(enabled = FALSE)

print(hc)
