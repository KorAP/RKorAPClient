# library(devtools)
# install_github("KorAP/RKorAPClient")
library(RKorAPClient)
library(ggplot2)
g <- KorAPConnection(verbose=TRUE) %>%
  frequencyQuery("sozusagen/i", vc=c("corpusSigle=FOLK", "corpusSigle!=FOLK")) %>%
  ipm() %>%
  mutate(corpus=c("FOLK", "DeReKo")) %>%
  ggplot(aes(x = corpus, y = ipm, ymin = conf.low, ymax = conf.high))  +
  geom_col() +
  geom_errorbar(width = .3, alpha = .3) +
  ggtitle("'sozusagen' in written (DeReKo) and spoken corpora (FOLK-excerpt)")
print(g)
