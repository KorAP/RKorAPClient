# Requires RKorAPClient >= 1.5.0 or a current development build.

library(RKorAPClient)

cat("Are the gender-sensitive variants Nutzer*innen and Nutzer:innen similarly dispersed\n",
    "across press sources, publication countries and publication years in 2023-2025,\n",
    "accounting for the token counts in each group?\n\n", sep = "")

kco <- KorAPConnection(verbose = TRUE)
queries <- c("Nutzer*innen" = "[orth='Nutzer*innen']",
             "Nutzer:innen" = "[orth='Nutzer:innen']")
# Temporary HAZ23-HAZ25 metadata workaround: require a publication country.
# Remove pubPlaceKey=/.+/ after the missing location metadata are repaired.
result <- corpusQuery(kco, queries,
  vc = "pubDate since 2023 & pubDate until 2025 & textType=/Zeit.*/ & pubPlaceKey=/.+/",
  fields = c("corpusTitle", "pubPlaceKey", "pubDate")) |>
  fetchSamplePages(maxHits = Inf, seed = 7) |>
  dispersionAnalysis(by = c("source", "pubPlaceKey", "year"))
print(result)
print(hc_dispersion(result, partition = "source", top = 12))
print(hc_dispersion(result, partition = "pubPlaceKey"))

# Inf fetches every hit for an exact comparison. A finite maxHits gives page
# estimates, which can be strongly biased by clustering within result pages.
# Years are calendar-year groups: their order and distances do not enter these
# measures. This compares evenness across years; it does not test a time trend.
