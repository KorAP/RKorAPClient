library(RKorAPClient)
library(httr)
library(kableExtra)
library(dplyr)

query = 'V\u00F6ner' # "Portable packages must use only ASCII characters in their demos."

korap_app <-oauth_app("test-korap-client", key = "773NHGM76N7P9b6rLfmpM4", secret = NULL)
korap_endpoint <- oauth_endpoint(NULL,
                                 "settings/oauth/authorize",
                                 "api/v1.0/oauth2/token",
                                 base_url = "https://korap.ids-mannheim.de")
token_bundle = oauth2.0_token(korap_endpoint, korap_app, scope = "search match_info", cache = FALSE)

new("KorAPConnection", verbose = TRUE, accessToken = token_bundle[["credentials"]][["access_token"]]) %>%
  corpusQuery(query, fields = c("textSigle", "pubDate", "corpusTitle", "snippet"),
              metadataOnly = FALSE) %>%
  fetchAll() %>%
  slot("collectedMatches") %>%
  dplyr::arrange(pubDate) %>%
  dplyr::rename("kwic" = "snippet") %>%
  kable(escape = FALSE, caption = paste0("Query hits for '", query, "' ordered by date of publication")) %>%
  kable_styling() %>%
  print()
