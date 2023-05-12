library(RKorAPClient)
library(tidyverse)
library(purrrlyr)
library(httr2)
library(httpuv)

demo_kor_app_id = "773NHGM76N7P9b6rLfmpM4"

# The challenge in searching gender variants with KorAP and DeReKo is that,
# firstly, some characters used for gender marking, especially punctuation marks,
# are interpreted and indexed as token boundaries and, secondly, punctuation
# marks are currently not indexed in KorAP.
#
# The former is intentional with regard to a majority of use cases and with
# regard to the reproducibility maxim (see Diewald/Kupietz/L\u00FCngen 2022).
# The latter is a shortcoming in KorAP that will be remedied sooner or later
# and that can be solved provisionally in the meantime with the help of the KorAP API.
#
# The following unravelPunctuationGenderCases function, for example, takes the
# result of a frequencyQuery for two supposedly consecutive tokens and then looks more
# closely into the KWIC snippets to see which non-indexed strings actually do appear
# between these tokens and counts the frequencies of the variants that occur.

unravelPunctuationGenderCases <- function(df, suffix = "innen", kco = new("KorAPConnection", verbose=TRUE)) {
  if ( nrow(df) > 1) {
    df %>%
      dplyr::filter(totalResults > 0 & str_detect(query, paste0(" ", suffix))) %>%
      by_row(unravelPunctuationGenderCases, .collate = "rows", .labels=FALSE) %>%
      select(-.row) %>%
      bind_rows(df %>% dplyr::filter(totalResults == 0 | ! str_detect(query, paste0(" ", suffix)))) %>%
      tidyr::complete(query, nesting(vc, total), fill = list(totalResults = 0))  %>%
      select(-f, -conf.low, -conf.high) %>%
      RKorAPClient::ci() %>%
      mutate(query = str_replace_all(query, '(^"|"$|[\\[\\]\\\\])', '')) %>%
      mutate(query = str_replace_all(query, paste0('\\(', suffix), paste0('(', suffix, ')'))) %>%
      filter(!str_detect(query, paste0("\\w ", suffix))) # remove "Nutzer innen"
  } else {
    q <- corpusQuery(kco, df$query, vc=df$vc, metadataOnly = FALSE) %>%
      fetchAll()
    cases <- q@collectedMatches$snippet %>%
      str_replace_all(paste0(".*<mark>.*\\w(\\W+)", suffix, "</mark>.*"), "\\1") %>%
      as_tibble() %>%
      group_by(value) %>%
      summarise(n = n())
    df %>% uncount(nrow(cases)) %>%
      mutate(query = str_replace(query, paste0(" (?=", suffix, ")"), cases$value), totalResults = cases$n)
  }
}

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
    kco
}

plotPluralGenderVariants <- function(word = "Nutzer",
                          years = c(1995:2022),
                          as.alternatives = FALSE,
                          vc = "referTo ratskorpus-2023-1 & pubDate in",
                          suffixes = c('Innen', '[\\*]innen"', '[_]innen"', ' innen'),
                          prefixes = c('',      '"',            '"',        ''),
                          kco = new("KorAPConnection", verbose=TRUE) %>% oauthorizeDemo()) {
  hc <-
    frequencyQuery(kco, paste0(prefixes, word, suffixes), paste(vc, years), as.alternatives=as.alternatives) %>%
    unravelPunctuationGenderCases(kco = kco) %>%
    hc_freq_by_year_ci(as.alternatives)
  print(hc)
  hc
}


hc <- plotPluralGenderVariants("Nutzer", c(1995:2022), as.alternatives = FALSE)
# htmlwidgets::saveWidget(hc, file=fname, selfcontained = TRUE)

# Diewald, Nils/Kupietz, Marc/L\u00FCngen, Harald (2022):
# Tokenizing on scale. Preprocessing large text corpora on the lexical and sentence level.
# In: Klosa-K\u00FCckelhaus, Annette/Engelberg, Stefan/M\u00F6hrs, Christine/Storjohann, Petra (eds):
# Dictionaries and Society. Proceedings of the XX EURALEX International Congress, 12-16 July 2022.
# Mannheim: IDS-Verlag, 2022: 208-221.
# <https://doi.org/10.14618/ids-pub-11146>

