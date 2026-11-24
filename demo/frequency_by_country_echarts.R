# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

library(RKorAPClient)

queries <- "(das|einen|den) Cola"
countries <- c("AT", "BE", "CH", "DE", "IT", "LU")
vcs <- setNames(sprintf("textType=/Zeit.*/ & pubPlaceKey=%s", countries), countries)

e <- KorAPConnection(verbose = TRUE) |>
  frequencyQuery(queries, vc = vcs, expand = TRUE) |>
  ec_freq_bar_ci(horizontal = FALSE) |>
  echarts4r::e_title("Relative frequency by country of publication")

print(e)
