library(RKorAPClient)
library(kableExtra)
library(dplyr)

query = 'V\u00F6ner' # "Portable packages must use only ASCII characters in their demos."

KorAPConnection(verbose = TRUE) %>%
  auth() %>%
  corpusQuery(query, fields = c("textSigle", "pubDate", "corpusTitle", "snippet"),
              metadataOnly = FALSE) %>%
  fetchAll() %>%
  slot("collectedMatches") %>%
  dplyr::arrange(pubDate) %>%
  dplyr::rename("kwic" = "snippet") %>%
  kable(format = "html", escape = FALSE, caption = paste0("Query hits for '", query, "' ordered by date of publication")) %>%
  kable_styling() %>%
  print()
