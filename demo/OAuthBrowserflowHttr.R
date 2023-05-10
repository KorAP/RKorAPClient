library(RKorAPClient)
library(httr)

korap_app <-oauth_app("test-korap-client", key = "773NHGM76N7P9b6rLfmpM4", secret = NULL)
korap_endpoint <- oauth_endpoint(NULL,
                                 "settings/oauth/authorize",
                                 "api/v1.0/oauth2/token",
                                 base_url = "https://korap.ids-mannheim.de")
token_bundle = oauth2.0_token(korap_endpoint, korap_app, scope = "search match_info", cache = FALSE)

kco <- new("KorAPConnection", verbose = T, accessToken = token_bundle[["credentials"]][["access_token"]])
q <- corpusQuery(kco, "Ameisenplage", metadataOnly = F) %>% fetchAll()
q@collectedMatches$snippet
