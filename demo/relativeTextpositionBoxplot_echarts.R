# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

library(RKorAPClient)
library(tidyverse)
library(echarts4r)

kco <- KorAPConnection(verbose = TRUE)

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
  "zuallererst/i",
  "zuerst/i",
  "zuletzt/i",
  "schlie\u00dflich/i"
) %>%
  map(~ get_relative_positions_sample(kco, .)) %>%
  bind_rows()

box_data <- df %>%
  group_by(query) %>%
  summarise(
    min = min(relativeTextPosition),
    q1 = quantile(relativeTextPosition, 0.25),
    median = median(relativeTextPosition),
    q3 = quantile(relativeTextPosition, 0.75),
    max = max(relativeTextPosition)
  )

# e_boxplot() calculates quartiles of its own, so pass the five numbers directly.
e <- e_charts() %>%
  e_list(list(
    xAxis = list(type = "value", min = 0, max = 1, name = "Relative position in text",
                 nameLocation = "middle", nameGap = 28),
    yAxis = list(type = "category", data = as.list(box_data$query), inverse = TRUE),
    grid = list(left = 110),
    series = list(list(type = "boxplot",
      data = unname(pmap(box_data[-1], function(...) list(...) %>% unname()))))
  )) %>%
  e_title("Relative positions of some adverbs in DeReKo texts", left = "center") %>%
  e_tooltip()

print(e)
