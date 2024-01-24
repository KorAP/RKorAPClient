library(RKorAPClient)
library(tidyverse)
library(purrrlyr)
library(httr2)
library(httpuv)
library(RMariaDB)
library(tidyfst)

demo_kor_app_id = "773NHGM76N7P9b6rLfmpM4"
source("https://www.stgries.info/research/dispersion/dispersions.r")

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

unravelPunctuationGenderCases <- function(df, suffix = "innen", kco) {
  if ( nrow(df) > 1) {
    df %>%
      dplyr::filter(totalResults > 0 & str_detect(query, paste0(" ", suffix))) %>%
      by_row(unravelPunctuationGenderCases, kco = kco, .collate = "rows", .labels=FALSE) %>%
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

getOKKSourceTitles <- function() {
  db <- dbConnect(MariaDB(), host="klinux10.ids-mannheim.de", user="viewer", dbname="corpora")
  dbExecute(db, "SET NAMES 'utf8'")
  rs <- dbSendQuery(db, "SELECT title from basename WHERE basename.rsr")
  corpus_parts  <- dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(db)
  return(corpus_parts$title)
}

sourceDispersions <- function(word = "Bürger",
                             sourceTitles = getOKKSourceTitles(),
                             as.alternatives = FALSE,
                             vc = 'referTo ratskorpus-2023-1 & corpusTitle="',
                             suffixes = c('Innen', '[\\*]innen"', '[_]innen"', ' innen'),
                             prefixes = c('',      '"',            '"',        ''),
                             kco = new("KorAPConnection", verbose=TRUE) %>% oauthorizeDemo()) {
  df <-
    frequencyQuery(kco, paste0(prefixes, word, suffixes), paste0(vc, sourceTitles, '"'), as.alternatives=as.alternatives) %>%
    unravelPunctuationGenderCases(kco = kco) %>%
    mutate(Quelle=str_replace_all(.$vc, '(.*="|"$)', '')) %>%
    ipm() %>%
    filter(total > 0) %>%
    rename(Variante=query)

  dispersions <- df %>%
    group_by(Variante) %>%
    mutate(total_size=sum(total), rel_size=total/total_size) %>%
    group_modify(~ as_tibble(dispersions2(.x$totalResults, .x$rel_size))) %>%
    pivot_longer(cols= -1) %>%
    filter(! str_detect(name, " equally")) %>%
    mutate(name=str_replace(name, " ?\\(.*\\)", '')) %>%
    mutate_when(str_detect(name, "DPnorm"), name = "1-DP_norm", value = 1 -value) %>%
    mutate(across(where(is.double)), value = round(value, 2)) %>%
    mutate_when(str_detect(name, "(corpus|range)"), value = as.integer(value)) %>%
    group_by(name) %>%
    mutate(rank = if_else(str_detect(name, "Kullback"), rank(value), rank(-value))) %>%
    group_by(Variante) %>%
    #    mutate(rank = mean(rank, na.rm = FALSE)) %>%
    bind_rows(summarise(., name="Avg. Rank", value = mean(rank, na.rm = FALSE))) %>%
    select(-rank) %>%
    pivot_wider(names_from = Variante) %>%
    rename(measure=name)

  return(list(df, dispersions))
}

df_dispersions <- sourceDispersions("Nutzer")
View(df_dispersions[[2]])

# htmlwidgets::saveWidget(hc, file=fname, selfcontained = TRUE)

# Diewald, Nils/Kupietz, Marc/L\u00FCngen, Harald (2022):
# Tokenizing on scale. Preprocessing large text corpora on the lexical and sentence level.
# In: Klosa-K\u00FCckelhaus, Annette/Engelberg, Stefan/M\u00F6hrs, Christine/Storjohann, Petra (eds):
# Dictionaries and Society. Proceedings of the XX EURALEX International Congress, 12-16 July 2022.
# Mannheim: IDS-Verlag, 2022: 208-221.
# <https://doi.org/10.14618/ids-pub-11146>

