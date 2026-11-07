# Requires RKorAPClient >= 1.5.0 or a current development build.

library(RKorAPClient)

queries <- "(das|einen|den) Cola"
countries <- c("AT", "BE", "CH", "DE", "IT", "LU")
vcs <- setNames(sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", countries), countries)

hc <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery(queries, vc = vcs, expand = TRUE) |>
  hc_freq_bar_ci(horizontal = FALSE) |>
  highcharter::hc_title(text = "Relative frequency by country of publication")

print(hc)
