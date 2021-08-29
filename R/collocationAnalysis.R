setGeneric("collocationAnalysis", function(kco, ...)  standardGeneric("collocationAnalysis") )

#' Collocation analysis
#'
#' @aliases collocationAnalysis
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Performs a collocation analysis for the given node (or query)
#' in the given virtual corpus.
#'
#' @details
#' As the collocation analysis is not yet provided by the KorAP backend
#' it is very slow. Expect it to run more than 45 min. For that reason, defaults for the parameters
#' `topCollocatesLimit`,`searchHitsSampleLimit`are optimized for speed rather than accuracy.
#' In addition, currently not the tokenization provided by the backend, but a tinkered one is used.
#'
#' @family collocation analysis functions
#'
#' @param minOccur               minimum absolute number of of observed co-occurrences to consider a collocate candidate
#' @param topCollocatesLimit     limit analysis to the n most frequent collocates in the search hits sample
#' @param searchHitsSampleLimit  limit the size of the search hits sample
#' @param stopwords              vector of stopwords not to be considered as collocates
#' @param seed                   seed for random page collecting order
#' @param ...                    more arguments will be passed to \code{\link{collocationScoreQuery}}
#' @inheritParams collocationScoreQuery,KorAPConnection-method
#' @return Tibble with top collocates, association scores, corresponding URLs for web user interface queries, etc.
#'
#' @importFrom stringr str_match str_split str_detect
#' @importFrom dplyr anti_join arrange desc slice_head
#'
#' @examples
#' \dontrun{
#'  new("KorAPConnection", verbose = TRUE) %>%
#'   collocationAnalysis("Newstickeritis", vc="corpusSigle=WUD17")
#' }
#'
#' @export
setMethod("collocationAnalysis", "KorAPConnection",
          function(kco,
                   node,
                   vc = "",
                   minOccur = 5,
                   leftContextSize = 5,
                   rightContextSize = 5,
                   topCollocatesLimit = 200,
                   searchHitsSampleLimit = 20000,
                   ignoreCollocateCase = FALSE,
                   stopwords = RKorAPClient::synsemanticStopwords(),
                   seed = 7,
                   ...) {
            # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
            word <- NULL
            frequency <- NULL

            set.seed(seed)
            candidates <- collocatesQuery(
              kco,
              node,
              vc = vc,
              minOccur = minOccur,
              leftContextSize = leftContextSize,
              rightContextSize = rightContextSize,
              searchHitsSampleLimit = searchHitsSampleLimit,
              ignoreCollocateCase = ignoreCollocateCase,
              stopwords = stopwords,
              ...
            )

            if (nrow(candidates) > 0) {
              candidates <- head(candidates, topCollocatesLimit)
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
              ) %>% dplyr::arrange(dplyr::desc(logDice))
            } else {
              tibble()
            }
          }
)

#' @importFrom magrittr debug_pipe
#' @importFrom stringr str_match str_split str_detect
#' @importFrom dplyr as_tibble rename filter anti_join tibble bind_rows case_when
#'
snippet2FreqTable <- function(snippet,
                              minOccur = 5,
                              leftContextSize = 5,
                              rightContextSize = 5,
                              ignoreCollocateCase = FALSE,
                              stopwords = c(),
                              tokenizeRegex = "([! )(\uc2\uab,.:?\u201e\u201c\'\"]+|&quot;)",
                              oldTable = data.frame(word = rep(NA, 1), frequency = rep(NA, 1))) {
  word <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  frequency <- NULL

  if (length(snippet) < 1) {
    dplyr::tibble(word=c(), frequency=c())
  } else if (length(snippet) > 1) {
    message(paste("Joinging", length(snippet), "kwics"))
    for (s in snippet) {
      oldTable <- snippet2FreqTable(
        s,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        oldTable = oldTable,
        stopwords = stopwords
      )
    }
    message(paste("Aggregating", length(oldTable$word), "tokens"))
    oldTable  %>%
      group_by(word) %>%
      mutate(word = dplyr::case_when(ignoreCollocateCase ~ tolower(word), TRUE ~ word)) %>%
      summarise(frequency=sum(frequency), .groups = "drop") %>%
      filter(frequency >= minOccur) %>%
      arrange(desc(frequency))
  } else {
    stopwordsTable <- tibble(word=stopwords)
    match <-
      str_match(
        snippet,
        '<span class="context-left">(<span class="more"></span>)?(.*[^ ]) *</span><span class="match"><mark>.*</mark></span><span class="context-right"> *([^<]*)'
      )

    left <- if(leftContextSize > 0)
      tail(unlist(str_split(match[1, 3], tokenizeRegex)), leftContextSize)
    else
      ""
    cat(paste("left:", left, "\n", collapse=" "))

    right <- if(rightContextSize > 0)
      head(unlist(str_split(match[1, 4], tokenizeRegex)), rightContextSize)
    else
        ""
    cat(paste("right:", right, "\n", collapse=" "))

    if(is.na(left) || is.na(right) || length(left) + length(right) == 0) {
      oldTable
    } else {
      table(c(left, right)) %>%
        dplyr::as_tibble(.name_repair = "minimal") %>%
        dplyr::rename(word = 1, frequency = 2) %>%
        dplyr::filter(str_detect(word, '^[:alnum:]+-?[:alnum:]*$')) %>%
        dplyr::anti_join(stopwordsTable, by="word")  %>%
        dplyr::bind_rows(oldTable)
    }
  }
}

#' Preliminary synsemantic stopwords function
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Preliminary synsemantic stopwords function to be used in collocation analysis.
#'
#' @details
#' Currently only suitable for German See \link[stopwords]{stopwords} for other languages.
#'
#' @param ... future arguments for language detection
#'
#' @family collocation analysis functions
#' @return Vector of synsemantic stopwords.
#' @export
synsemanticStopwords <- function(...) {
  res <- c(
    "der",
    "die",
    "und",
    "in",
    "den",
    "von",
    "mit",
    "das",
    "zu",
    "im",
    "ist",
    "auf",
    "sich",
    "Die",
    "des",
    "dem",
    "nicht",
    "ein",
    "eine",
    "es",
    "auch",
    "an",
    "als",
    "am",
    "aus",
    "Der",
    "bei",
    "er",
    "dass",
    "sie",
    "nach",
    "um",
    "Das",
    "zum",
    "noch",
    "war",
    "einen",
    "einer",
    "wie",
    "einem",
    "vor",
    "bis",
    "\u00fcber",
    "so",
    "aber"
  )
  return(res)
}

collocatesQuery <-
  function(kco,
           query,
           vc = "",
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           searchHitsSampleLimit = 20000,
           ignoreCollocateCase = FALSE,
           stopwords = c(),
           ...) {
    frequency <- NULL
    q <- corpusQuery(kco, query, vc, metadataOnly = F, ...) %>%
      fetchNext(maxFetch=searchHitsSampleLimit, randomizePageOrder=TRUE)
    snippet2FreqTable((q@collectedMatches)$snippet,
                      minOccur = minOccur,
                      leftContextSize = leftContextSize,
                      rightContextSize = rightContextSize,
                      ignoreCollocateCase = ignoreCollocateCase,
                      stopwords = stopwords) %>%
      mutate(frequency = frequency * q@totalResults / searchHitsSampleLimit)
  }


