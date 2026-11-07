# Requires RKorAPClient >= 1.5.0 or a current development build.

library(RKorAPClient)
library(ggplot2)

g <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery("sozusagen/i",
    vc = c("FOLK (spoken)" = "corpusSigle=FOLK",
           "DeReKo (written)" = "corpusSigle!=FOLK")) |>
  ipm() |>
  ggplot(aes(x = label, y = ipm)) +
  geom_freq_bar_ci() +
  labs(x = NULL, y = "Instances per million tokens",
       title = "'sozusagen' in written and spoken corpora") +
  theme_bw()

print(g)
