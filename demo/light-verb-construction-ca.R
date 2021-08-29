library(RKorAPClient)
library(knitr)
new("KorAPConnection", verbose = TRUE) %>%
  collocationAnalysis(
    "focus(in [tt/p=NN] {[tt/l=setzen]})",
    leftContextSize = 1,
    rightContextSize = 0,
    exactFrequencies = FALSE,
    searchHitsSampleLimit = 1000,
    topCollocatesLimit = 20
  ) %>%
  mutate(LVC = sprintf("[in %s setzen](%s)", collocate, webUIRequestUrl)) %>%
  select(LVC, logDice, pmi, ll) %>%
  head(10) %>%
  kable(format="pipe", digits=2)  %>%
  cat(file="/tmp/in_setzen.md", sep="\n")

#rmarkdown::render("/tmp/in_setzen.md")
#browseURL("/tmp/in_setzen.html")

