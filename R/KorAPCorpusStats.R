#' Class KorAPCorpusStats
#'
#' `KorAPCorpusStats` objects can hold information about a corpus or virtual corpus.
#' `KorAPCorpusStats` objects can be obtained by the [corpusStats()] method.
#'
#' @include KorAPConnection.R
#'
#' @export
#' @slot vc definition of the virtual corpus
#' @slot tokens number of tokens
#' @slot documents number of documents
#' @slot sentences number of sentences
#' @slot paragraphs number of paragraphs
#' @slot webUIRequestUrl link to the web user interface with the current vc definition
setClass("KorAPCorpusStats", slots=c(vc="character", documents="numeric", tokens="numeric", sentences="numeric", paragraphs="numeric", webUIRequestUrl="character" ))

log_info <- function(v,  ...) {
  cat(ifelse(v, paste0(...), ""))
}
setGeneric("corpusStats", function(kco, ...)  standardGeneric("corpusStats") )

#' Fetch information about a (virtual) corpus
#' @param kco [KorAPConnection()] object (obtained e.g. from `new("KorAPConnection")`
#' @param vc string describing the virtual corpus. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param verbose logical. If `TRUE`, additional diagnostics are printed.
#' @param as.df return result as data frame instead of as S4 object?
#' @return `KorAPCorpusStats` object with the slots `documents`, `tokens`, `sentences`, `paragraphs`
#'
#' @examples
#'
#' \dontrun{
#'
#' kco <- new("KorAPConnection")
#' corpusStats(kco, "pubDate in 2017 & textType=/Zeitung.*/")
#' }
#'
#' @aliases corpusStats
#' @export
setMethod("corpusStats", "KorAPConnection",  function(kco,
                                                      vc = "",
                                                      verbose = kco@verbose,
                                                      as.df = FALSE) {
  if (length(vc) > 1)
    do.call(rbind, Map(function(cq)
      corpusStats(kco, cq, verbose, as.df = TRUE), vc))
  else {
    url <-
      paste0(kco@apiUrl,
             'statistics?cq=',
             URLencode(enc2utf8(vc), reserved = TRUE))
    log_info(verbose, "Getting size of virtual corpus \"", vc, "\"", sep = "")
    res <- apiCall(kco, url)
    webUIRequestUrl <- paste0(kco@KorAPUrl, sprintf("?q=Test&cq=%s",  URLencode(enc2utf8(vc))))
    if(is.null(res)) {
      res <- data.frame(documents=NA, tokens=NA, sentences=NA, paragraphs=NA)
    }
    log_info(verbose, ": ", res$tokens, " tokens\n")
    if (as.df)
      data.frame(vc = vc, webUIRequestUrl = webUIRequestUrl, res, stringsAsFactors = FALSE)
    else
      new(
        "KorAPCorpusStats",
        vc = vc,
        documents = res$documents,
        tokens = res$tokens,
        sentences = res$sentences,
        paragraphs = res$paragraphs,
        webUIRequestUrl = webUIRequestUrl
      )
  }
})

#' @rdname KorAPCorpusStats-class
#' @param object KorAPCorpusStats object
#' @export
setMethod("show", "KorAPCorpusStats", function(object) {
  cat("<KorAPCorpusStats>", "\n")
  if (object@vc == "") {
    cat("The whole corpus")
  } else {
    cat("The virtual corpus described by \"", object@vc, "\"", sep="")
  }
  cat(" contains", formatC(object@tokens, format="f", digits=0, big.mark=","), "tokens in",
      formatC(object@sentences, format="d", big.mark=","), "sentences in",
      formatC(object@documents, format="d", big.mark=","), "documents.\n")
})
