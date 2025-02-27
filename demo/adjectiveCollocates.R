library(RKorAPClient)
library(kableExtra)


KorAPConnection(verbose = TRUE) %>%
  auth() %>%
  collocationAnalysis("focus([marmot/p=ADJA] {Gendern})", leftContextSize=1, rightContextSize=0) %>%
  mutate(collocate = paste0('<a href="', webUIRequestUrl, '">', collocate, '</a>')) %>%
  select(collocate, O, pmi, mi2, mi3, logDice, ll) %>%
  kable(format = "html", escape = FALSE, caption = "Adjective collocates of 'Gendern'") %>%
  kable_styling() %>%
  print()

