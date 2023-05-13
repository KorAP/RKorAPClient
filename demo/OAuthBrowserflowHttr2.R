library(RKorAPClient)
library(httr2)
library(kableExtra)

token <- oauth_client( id = "773NHGM76N7P9b6rLfmpM4",
    token_url = "https://korap.ids-mannheim.de/api/v1.0/oauth2/token") %>%
  oauth_flow_auth_code( scope = "search match_info",
    auth_url = "https://korap.ids-mannheim.de/settings/oauth/authorize")

new("KorAPConnection", verbose = TRUE, accessToken = token$access_token) %>%
  collocationAnalysis("focus([marmot/p=ADJA] {Gendern})", leftContextSize=1, rightContextSize=0) %>%
  mutate(collocate = paste0('<a href="', webUIRequestUrl, '">', collocate, '</a>')) %>%
  select(collocate, O, pmi, mi2, mi3, logDice, ll) %>%
  kable(escape = FALSE, caption = "Adjective collocates of 'Gendern'") %>%
  kable_styling() %>%
  print()

