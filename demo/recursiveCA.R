library(RKorAPClient)
library(tidyverse)
library(knitr)
new("KorAPConnection", verbose = TRUE) %>%
  collocationAnalysis(
    "aufmerksam",
    leftContextSize = 1,
    rightContextSize = 1,
    exactFrequencies = TRUE,
    searchHitsSampleLimit = 1000,
    topCollocatesLimit = 10,
    withinSpan = "",
    maxRecurse=2
  ) %>%
#  mutate(LVC = sprintf("[aufmerksam %s](%s)", collocate, webUIRequestUrl)) %>%
  mutate(LVC = sprintf("[%s](%s)", example, webUIRequestUrl)) %>%
  { . ->> ca } %>%
  select(LVC, logDice, pmi, ll) %>%
  head(50) %>%
  kable(format="pipe", digits=2)  %>%
  cat(file="/tmp/aufmerksam.md", sep="\n")

rmarkdown::render("/tmp/aufmerksam.md")
browseURL("/tmp/aufmerksam.html")
