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
                              tokenizeRegex = "([! )(»«),.:?„“\'\"]+|&quot;)",
                              oldTable = data.frame(word = rep(NA, 1), frequency = rep(NA, 1))) {
  if (length(snippet) > 1) {
    message(paste("Joinging", length(snippet), "kwics"))
    tic()
    for (s in snippet) {
      oldTable <- snippet2FreqTable(
        s,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
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
        '<span class="context-left">(<span class="more"></span>)?(.*[^ ]) *</span><span class="match"><mark>.*</mark></span><span class="context-right"> *([^<]*)'
      )

    left <- tail(unlist(str_split(match[1, 3], tokenizeRegex)), leftContextSize)
    # cat(paste("left:", left, "\n", collapse=" "))
    right <- head(unlist(str_split(match[1, 4], tokenizeRegex)), rightContextSize)
    # cat(paste("right:", right, "\n", collapse=" "))

    table(c(left, right)) %>%
      as.data.frame() %>%
      dplyr::rename(word = 1, frequency = 2) %>%
      mutate(word = as.character(word)) %>%
      filter(str_detect(word, '^[:alnum:]+-?[:alnum:]*$')) %>%
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
    snippet2FreqTable((q@collectedMatches)$snippet,
                      minOccur = minOccur,
                      leftContextSize = leftContextSize,
                      rightContextSize = rightContextSize,
                      ignoreCollocateCase = ignoreCollocateCase) %>%
      mutate(frequency = frequency * q@totalResults / limit)
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
      rightContextSize = rightContextSize,
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
      rightContextSize = rightContextSize,
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
