library(RKorAPClient)
library(tidyverse)
library(knitr)
library(rmarkdown)

lvLemma <- "nehmen"

mdFile <- tempfile(lvLemma, fileext = ".md")

cat(file=mdFile, sprintf("---\ntitle: LVC analysis of %s\n---\n\n", lvLemma))

new("KorAPConnection", verbose = TRUE) %>%
  collocationAnalysis(
    sprintf("focus(in [tt/p=NN] {[tt/l=%s]})", lvLemma),
    leftContextSize = 1,
    rightContextSize = 0,
    exactFrequencies = FALSE,
    searchHitsSampleLimit = 1000,
    topCollocatesLimit = 20
  ) %>%
  mutate(LVC = sprintf("[in %s %s](%s)", collocate, lvLemma, webUIRequestUrl)) %>%
  select(LVC, logDice, pmi, ll) %>%
  slice_head(10) %>%
  kable(format = "pipe", digits = 2)  %>%
  cat(file = mdFile, sep = "\n", append = TRUE)

rmarkdown::render(mdFile)
browseURL(str_replace(mdFile, "\\.md$", ".html"))
