library(tidyverse)
library(RKorAPClient)
library(tictoc)
kco <- new("KorAPConnection", verbose = TRUE)

snippet2FreqTable <- function(snippet,
                              minOccurr = 5,
                              leftContextSize = 5,
                              rightContextSize = 5,
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
      filter(str_detect(word, '^[:alnum:]+[:alnum:-]$')) %>%
      anti_join(stopwordsTable, by="word")  %>%
      bind_rows(oldTable)
  }
}


collocatesQuery <-
  function(kco,
           query,
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           limit = 20000,
           ...) {
    q <- corpusQuery(kco, query, metadataOnly = F, ...) %>%
      fetchNext(maxFetch=limit)
    df <- snippet2FreqTable((q@collectedMatches)$snippet,
                      minOccur = minOccur,
                      leftContextSize = leftContextSize,
                      rightContextSize = leftContextSize)
  }

collocationAnalysis <-
  function(kco,
           query,
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           maxCollocates = 200,
           limit = 20000,
           ...) {
    candidates <- collocatesQuery(kco, query, minOccur = minOccur,
                    leftContextSize = leftContextSize,
                    rightContextSize = leftContextSize, limit, ...) %>%
                  head(maxCollocates)
    print(candidates)
    collocationScoreQuery(kco, node=query, collocate=candidates$word, leftContextSize = leftContextSize,
                          rightContextSize = leftContextSize, observed=candidates$frequency, ...) %>% arrange(desc(logDice))
    }

kco <- new("KorAPConnection", verbose = F)
if (is.null(kco@accessToken)) {
  message()
}

#collocationAnalysis(kco,"Ameisenplage", limit=10000)
tic()
x<<-collocationAnalysis(kco, "Zähne", limit=20000)
toc()
