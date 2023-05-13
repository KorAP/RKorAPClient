library(RKorAPClient)
library(httr2)
library(kableExtra)

demo_kor_app_id = "773NHGM76N7P9b6rLfmpM4"

oauthorizeDemo <- function(kco, app_id = demo_kor_app_id) {
  if (is.null(kco@accessToken) || is.null(kco@welcome)) { # if access token is not set or invalid
    kco@accessToken <- ( # request one
      oauth_client(
        id =  app_id, # for the demo application
        token_url = paste0(kco@apiUrl, "oauth2/token")
      ) %>%
        oauth_flow_auth_code(
          scope = "search match_info",
          auth_url = paste0(kco@KorAPUrl, "settings/oauth/authorize")
        )
    )$access_token
  }
  print(kco@accessToken)
  return(kco)
}

new("KorAPConnection", verbose = TRUE) %>%
  oauthorizeDemo() %>%
  collocationAnalysis("focus([marmot/p=ADJA] {Gendern})", leftContextSize=1, rightContextSize=0) %>%
  mutate(collocate = paste0('<a href="', webUIRequestUrl, '">', collocate, '</a>')) %>%
  select(collocate, O, pmi, mi2, mi3, logDice, ll) %>%
  kable(escape = FALSE, caption = "Adjective collocates of 'Gendern'") %>%
  kable_styling() %>%
  print()

