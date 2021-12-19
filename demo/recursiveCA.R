library(RKorAPClient)
library(tidyverse)
library(knitr)

nodeWordform <- 'aufmerksam'
mdFile <- tempfile(nodeWordform, fileext = ".md")

new("KorAPConnection", verbose = TRUE) %>%
  collocationAnalysis(
    nodeWordform,
    leftContextSize = 2,
    rightContextSize = 2,
    exactFrequencies = TRUE,
    searchHitsSampleLimit = 1000,
    topCollocatesLimit = 10,
    maxRecurse = 1,
    addExamples = TRUE
  ) %>%
  mutate(LVC = sprintf("[%s](%s)", example, webUIRequestUrl)) %>%
  { . ->> ca } %>%
  select(LVC, logDice, pmi, ll) %>%
  head(50) %>%
  kable(format = "pipe", digits = 2)  %>%
  cat(file = mdFile, sep = "\n")

rmarkdown::render(mdFile)
browseURL(gsub("\\.md", ".html", mdFile))
