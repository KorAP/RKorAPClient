# Requires RKorAPClient >= 1.5.0 or a current development build.

library(RKorAPClient)
library(ggplot2)

query <- "[tt/l=verunfallen]" # search TreeTagger lemma annotations
countries <- c("AT", "BE", "CH", "DE", "IT", "LU")
vcs <- setNames(sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", countries), countries)

g <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery(query, vc = vcs) |>
  ipm() |>
  ggplot(aes(x = label, y = ipm)) +
  geom_freq_bar_ci() +
  labs(x = "Country of publication", y = "Instances per million tokens",
       title = sprintf("Relative frequency of '%s'", query)) +
  theme_bw()

print(g)
