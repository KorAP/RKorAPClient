setGeneric("collocationScoreQuery", function(kco, ...)  standardGeneric("collocationScoreQuery") )

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' Query frequencies of a node and a collocate and calculate collocation association scores
#'
#' @aliases collocationScoreQuery
#'
#' @description
#' Computes various collocation association scores
#' based on [frequencyQuery()]s for a target word and a collocate.
#'
#' @param kco [KorAPConnection()] object (obtained e.g. from `new("KorAPConnection")`
#' @param node               target word
#' @param collocate          collocate of target word
#' @param vc                 string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param lemmatizeNodeQuery      logical, set to TRUE if node query should be lemmatized, i.e. `x -> [tt/l=x]`
#' @param lemmatizeCollocateQuery logical, set to TRUE if collocate query should be lemmatized, i.e. `x -> [tt/l=x]`
#' @param leftContextSize    size of the left context window
#' @param rightContextSize   size of the right context window
#' @param scoreFunctions     named list of score functions of the form function(O1, O2, O, N, E, window_size), see e.g. [pmi]
#' @param smoothingConstant  smoothing constant will be added to all observed values
#' @param observed           if collocation frequencies are already known (or estimated from a sample) they can be passed as a vector here, otherwise: NA
#' @param ignoreCollocateCase     logical, set to TRUE if collocate case should be ignored
#' @param withinSpan         KorAP span specification for collocations to be searched within
#'
#' @return tibble with query KorAP web request URL, all observed values and association scores
#'
#' @family collocation analysis functions
#'
#' @examples
#' \dontrun{
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#'   collocationScoreQuery("Grund", "triftiger")
#' }
#'
#' \dontrun{
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#' collocationScoreQuery("Grund", c("guter", "triftiger"),
#'    scoreFunctions = list(localMI = function(O1, O2, O, N, E, window_size) { O * log2(O/E) }) )
#' }
#'
#' \dontrun{
#'
#' library(highcharter)
#' library(tidyr)
#' new("KorAPConnection", verbose = TRUE) %>%
#'   collocationScoreQuery("Team", "agil", vc = paste("pubDate in", c(2014:2018)),
#'                         lemmatizeNodeQuery = TRUE, lemmatizeCollocateQuery = TRUE) %>%
#'                          pivot_longer(14:last_col(), names_to = "measure", values_to = "score") %>%
#'   hchart(type="spline", hcaes(label, score, group=measure)) %>%
#'   hc_add_onclick_korap_search()
#' }
#'
#' @importFrom tidyr pivot_longer
#' @export
setMethod("collocationScoreQuery", "KorAPConnection",
          function(kco,
                   node,
                   collocate,
                   vc = "",
                   lemmatizeNodeQuery = FALSE,
                   lemmatizeCollocateQuery = FALSE,
                   leftContextSize = 5,
                   rightContextSize = 5,
                   scoreFunctions = defaultAssociationScoreFunctions(),
                   smoothingConstant = .5,
                   observed = NA,
                   ignoreCollocateCase = FALSE,
                   withinSpan = "base/s=s"
          ) {
            # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
            O1 <- O2 <- O <- N <- E <- w <- 0

            query <- buildCollocationQuery(node,
                                           collocate,
                                           lemmatizeNodeQuery,
                                           lemmatizeCollocateQuery,
                                           leftContextSize,
                                           rightContextSize,
                                           ignoreCollocateCase,
                                           withinSpan)

            tibble(
              node = node,
              collocate = collocate,
              label = queryStringToLabel(vc),
              vc = vc,
              webUIRequestUrl = if (is.na(observed[1]))
                frequencyQuery(kco, query, vc)$webUIRequestUrl
              else
                buildWebUIRequestUrl(
                  kco,
                  buildCollocationQuery(
                    node,
                    collocate,
                    lemmatizeNodeQuery,
                    lemmatizeCollocateQuery,
                    leftContextSize,
                    rightContextSize,
                    ignoreCollocateCase,
                    withinSpan
                  ),
                  vc
                ),
              w = leftContextSize + rightContextSize,
              leftContextSize,
              rightContextSize,
              N  = frequencyQuery(kco, node, vc)$total + smoothingConstant,
              O = as.double( if(is.na(observed[1])) frequencyQuery(kco, query, vc)$totalResults else observed) + smoothingConstant,
              O1 = frequencyQuery(kco, node, vc)$totalResults + smoothingConstant,
              O2 = frequencyQuery(kco, collocate, vc)$totalResults + smoothingConstant,
              E = w * as.double(O1) * O2 / N
            ) %>%
              mutate(!!! lapply(scoreFunctions, mapply, .$O1, .$O2, .$O, .$N, .$E, .$w))

          })

buildCollocationQuery <- function(                   node,
                                                     collocate,
                                                     lemmatizeNodeQuery = FALSE,
                                                     lemmatizeCollocateQuery = FALSE,
                                                     leftContextSize = 5,
                                                     rightContextSize = 5,
                                                     ignoreCollocateCase = FALSE,
                                                     withinSpan = "base/s=s"
) {
  if (leftContextSize <= 0 && rightContextSize <= 0) {
    stop(sprintf("At least one of leftContextSize (=%d) and rightContextSize (=%d) must be > 0", leftContextSize, rightContextSize),
         call. = FALSE)
  }

  if (lemmatizeNodeQuery) {
    node <- lemmatizeWordQuery(node)
  }

  if (ignoreCollocateCase) {
    collocate <- ignoreCollocateCaseWordQuery(collocate)
  }

  if (lemmatizeCollocateQuery) {
    collocate <- lemmatizeWordQuery(collocate)
  }

  query <- ""

  if (leftContextSize > 0) {
    query <-
      paste0(collocate,
             if (leftContextSize > 1) paste0(" []{0,", leftContextSize - 1, "} ") else " ",
             node,
             if (rightContextSize > 0)  " | ")
  }

  if (rightContextSize > 0) {
    query <-
      paste0(query, node,
             if (rightContextSize > 1) paste0(" []{0,", rightContextSize - 1, "} ") else " ", collocate)
  }

  if(!is.null(withinSpan) && !is.na(withinSpan) && nchar(withinSpan) > 0) {
    query <- sprintf("contains(<%s>, (%s))", withinSpan, query)
  }

  query
}

ignoreCollocateCaseWordQuery <- function(w) {
  paste0(w, '/i')
}

lemmatizeWordQuery <- function(w) {
  paste0('[tt/l=', w, ']')
}
