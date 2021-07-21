library(tidyverse)
library(RKorAPClient)
library(tictoc)
library(magrittr)
kco <- new("KorAPConnection", verbose = TRUE)

snippet2FreqTable <- function(snippet,
                              minOccurr = 5,
                              leftContextSize = 5,
                              rightContextSize = 5,
                              ignoreCollocateCase = FALSE,
                              stopwords = c("der", "die", "das", "ein", "eine", "einer", "den", "dem", "des", "einen", "von", "mit", "zu", "und", "in", "am", "um"),
                              oldTable = data.frame(word = rep(NA, 1), frequency = rep(NA, 1))) {
  if (length(snippet) > 1) {
    message(paste("Joinging", length(snippet), "kwics"))
    tic()
    for (s in snippet) {
      oldTable <- snippet2FreqTable(
        s,
        leftContextSize = 5,
        rightContextSize = 5,
        oldTable = oldTable
      )
    }
    toc()
    message(paste("Aggregating", length(oldTable$word), "tokens"))
      oldTable  %>%
      group_by(word) %>%
      mutate(word = case_when(ignoreCollocateCase ~ tolower(word), TRUE ~ word)) %>%
      summarise(frequency=sum(frequency), .groups = "drop") %>%
      filter(frequency >= minOccurr) %>%
      arrange(desc(frequency))
  } else {
    stopwordsTable <- tibble(word=stopwords)
    match <-
      str_match(
        snippet,
        '<span class="context-left"><span class="more"></span>(.*[^ ]) *</span><span class="match"><mark>.*</mark></span><span class="context-right"> *([^<]*)'
      )
    left <- tail(match[1, 2], leftContextSize)
    right <- head(match[1, 3], rightContextSize)
    str_split(paste(left, right), "([! )(»«),.:?„“\'\"]+|&quot;)")[[1]] %>%
      table() %>%
      as.data.frame() %>%
      dplyr::rename(word = 1, frequency = 2) %>%
      mutate(word = as.character(word)) %>%
      filter(str_detect(word, '^[:alnum:]+[:alnum:-]$')) %>%
      anti_join(stopwordsTable, by="word")  %>%
      bind_rows(oldTable)
  }
}


collocatesQuery <-
  function(kco,
           query,
           vc = "",
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           limit = 20000,
           ignoreCollocateCase = FALSE,
           ...) {
    q <- corpusQuery(kco, query, vc, metadataOnly = F, ...) %>%
      fetchNext(maxFetch=limit, randomizePageOrder=TRUE)
    df <- snippet2FreqTable((q@collectedMatches)$snippet,
                      minOccur = minOccur,
                      leftContextSize = leftContextSize,
                      rightContextSize = leftContextSize,
                      ignoreCollocateCase = ignoreCollocateCase)
  }

collocationAnalysis <-
  function(kco,
           node,
           vc = "",
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           maxCollocates = 200,
           limit = 20000,
           ignoreCollocateCase = FALSE,
           seed = 7,
           ...) {
    set.seed(seed)
    candidates <- collocatesQuery(
      kco,
      node,
      vc = vc,
      minOccur = minOccur,
      leftContextSize = leftContextSize,
      rightContextSize = leftContextSize,
      limit = limit,
      ignoreCollocateCase = ignoreCollocateCase,
      ...
    ) %>%
      head(maxCollocates)
    print(candidates)
    collocationScoreQuery(
      kco,
      node = node,
      collocate = candidates$word,
      vc = vc,
      leftContextSize = leftContextSize,
      rightContextSize = leftContextSize,
      observed = candidates$frequency,
      ignoreCollocateCase = ignoreCollocateCase,
      ...
    ) %>% arrange(desc(logDice))
  }

kco <- new("KorAPConnection", verbose = T)
if (is.null(kco@accessToken)) {
  message()
}

#collocationAnalysis(kco,"Ameisenplage", limit=10000)
#tic()
#y<<-collocationAnalysis(kco, "Packung", vc="textDomain=Sport", limit=2000)
#toc()
