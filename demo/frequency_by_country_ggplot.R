library(RKorAPClient)
library(ggplot2)

query <- "[tt/l=verunfallen]" # search TreeTagger lemma annotations
countries <- c("AT", "BE", "CH", "DE", "IT", "LU")

vcs <- sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", countries) # limit virtual corpus to newspapers and magazines

g <- KorAPConnection(verbose=TRUE) %>%
  frequencyQuery(query, vc=vcs) %>%
  ipm() %>%
  mutate(Land = countries) %>%
  ggplot(aes(x = Land, y = ipm, ymin = conf.low, ymax = conf.high))  +
  geom_col() +
  geom_errorbar(width = .3, alpha = .3) +
  ggtitle(sprintf("Relative frequency of '%s'", query))

print(g)
