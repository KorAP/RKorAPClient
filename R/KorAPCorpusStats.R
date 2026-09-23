#' KorAPCorpusStats class (internal)
#'
#' Internal class for corpus statistics storage. Users work with `corpusStats()` function instead.
#'
#' @keywords internal
#' @include KorAPConnection.R
#' @include logging.R
#'
#' @export
setClass("KorAPCorpusStats", slots = c(vc = "character", documents = "numeric", tokens = "numeric", sentences = "numeric", paragraphs = "numeric", webUIRequestUrl = "character"))

setGeneric("corpusStats", function(kco, ...) standardGeneric("corpusStats"))

#' Get corpus size and statistics
#'
#' Retrieve information about corpus size (documents, tokens, sentences, paragraphs) 
#' for the entire corpus or a virtual corpus subset.
#'
#' @section Usage:
#' ```r
#' # Get statistics for entire corpus
#' kcon <- KorAPConnection()
#' stats <- corpusStats(kcon)
#' 
#' # Get statistics for a specific time period
#' stats <- corpusStats(kcon, "pubDate in 2020")
#' 
#' # Access the number of tokens
#' stats@tokens
#' ```
#'
#' @family corpus analysis
#' @param kco [KorAPConnection()] object (obtained e.g. from `KorAPConnection()`
#' @param vc string describing the virtual corpus. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param verbose logical. If `TRUE`, additional diagnostics are printed.
#' @param as.df return result as data frame instead of as S4 object?
#' @param cacheAs path to an RDS file to keep the result in. If the file exists and records the same call, it is read back instead of contacting the server; otherwise the query is run and its result stored there. Unlike the connection's `cache`, this file belongs to the caller, which is what keeps an analysis reproducible once the corpus has grown or the scores have changed. Defaults to \code{NULL} (no file).
#' @return Object containing corpus statistics with the following information:
#' \describe{
#'   \item{`vc`}{Virtual corpus definition used (empty string for entire corpus)}
#'   \item{`documents`}{Total number of documents in the (virtual) corpus}
#'   \item{`tokens`}{Total number of word tokens in the (virtual) corpus}
#'   \item{`sentences`}{Total number of sentences in the (virtual) corpus}
#'   \item{`paragraphs`}{Total number of paragraphs in the (virtual) corpus}
#'   \item{`webUIRequestUrl`}{URL to view this corpus subset in KorAP web interface}
#' }
#' When `as.df=TRUE`, returns a data frame with these columns. 
#' When `as.df=FALSE` (default), returns a KorAPCorpusStats object with these values as slots.
#'
#' @importFrom urltools url_encode
#' @examples
#' \dontrun{
#' 
#' kco <- KorAPConnection()
#' 
#' # Get statistics for entire corpus (returns S4 object)
#' stats <- corpusStats(kco)
#' stats@tokens  # Access number of tokens
#' 
#' # Get statistics for newspaper texts from 2017 (as data frame)
#' df <- corpusStats(kco, "pubDate in 2017 & textType=/Zeitung.*/", as.df = TRUE)
#' df$documents  # Access number of documents
#' 
#' # Compare corpus sizes across years
#' years <- 2015:2020
#' sizes <- sapply(years, function(y) {
#'   corpusStats(kco, paste("pubDate in", y))@tokens
#' })
#' }
#'
#' @aliases corpusStats

#' @export
setMethod("corpusStats", "KorAPConnection", function(kco,
                                                     vc = "",
                                                     verbose = kco@verbose,
                                                     as.df = FALSE,
                                                     cacheAs = NULL) {
  cacheRecord <- NULL
  if (!is.null(cacheAs)) {
    cacheAs <- cacheAsFileName(cacheAs)
    cacheRecord <- cacheAsRecord(environment(), NULL, kco)
    cached <- readCacheAs(cacheAs, kco, cacheRecord, "corpus statistics")
    if (!is.null(cached)) {
      return(cached)
    }
  }

  stats <- if (length(vc) > 1) {
    # the names of a named vc vector would end up as row names, which the first
    # bind_rows() drops, so they are kept as a column instead
    vcLabel <- vcLabels(vc)
    total_items <- length(vc)
    vcDisplay <- factor_common_prefix(if (!is.null(vcLabel)) vcLabel else ifelse(vc == "", "(all)", vc))
    layout <- progress_layout(total_items, vcDisplay$labels)
    start_time <- Sys.time()
    results <- list()
    individual_times <- numeric(total_items)
    statuses <- character(total_items)
    log_info(
      verbose, "Getting the size of ", total_items, " virtual corpora",
      if (nzchar(vcDisplay$prefix)) paste0(": ", vcDisplay$prefix), "\n"
    )

    for (i in seq_along(vc)) {
      progress_row_start(verbose, layout, i, vcDisplay$labels[i])
      item_start_time <- Sys.time()
      result <- corpusStats(kco, vc[i], verbose = FALSE, as.df = TRUE)
      if (!is.null(vcLabel)) {
        result <- tibble::add_column(result, label = vcLabel[i], .after = "vc")
      }
      results[[i]] <- result
      individual_times[i] <- as.numeric(difftime(Sys.time(), item_start_time, units = "secs"))

      # corpusStats() does not report cache hits, but they come back at once
      done <- individual_times[seq_len(i)]
      statuses[i] <- if (is.na(result$tokens)) "failed" else if (individual_times[i] < 0.1) "cached" else "ok"
      progress_row_end(
        verbose,
        if (statuses[i] == "failed") "failed" else paste(format_count(result$tokens), "tokens"),
        individual_times[i],
        statuses[i],
        format_remaining(done, done < 0.1, total_items),
        # DeReKo as a whole has tens of billions of tokens
        width = 21
      )
    }

    progress_summary(
      verbose, "virtual corpora", statuses,
      as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )

    stats <- do.call(rbind, results)
    rownames(stats) <- NULL
    stats
  } else {
    url <-
      paste0(
        kco@apiUrl,
        "statistics?cq=",
        URLencode(enc2utf8(vc), reserved = TRUE)
      )
    log_info(verbose, "Getting size of virtual corpus \"", vc, "\"", sep = "")
    res <- apiCall(kco, url)
    webUIRequestUrl <- paste0(kco@KorAPUrl, sprintf("?q=<base/s=t>&cq=%s", url_encode(enc2utf8(vc))))
    if (is.null(res)) {
      res <- data.frame(documents = NA, tokens = NA, sentences = NA, paragraphs = NA)
    }
    log_info(verbose, ": ", format_count(res$tokens), " tokens\n")
    if (as.df) {
      data.frame(vc = vc, webUIRequestUrl = webUIRequestUrl, res, stringsAsFactors = FALSE)
    } else {
      new(
        "KorAPCorpusStats",
        vc = vc,
        documents = ifelse(is.logical(res$documents), 0, res$documents),
        tokens = ifelse(is.logical(res$tokens), 0, res$tokens),
        sentences = ifelse(is.logical(res$documents), 0, res$sentences),
        paragraphs = ifelse(is.logical(res$paragraphs), 0, res$paragraphs),
        webUIRequestUrl = webUIRequestUrl
      )
    }
  }

  if (!is.null(cacheAs)) {
    writeCacheAs(cacheAs, kco, cacheRecord, "corpus statistics", stats)
  }
  stats
})

#' @rdname KorAPCorpusStats-class
#' @param object KorAPCorpusStats object
#' @export
setMethod("show", "KorAPCorpusStats", function(object) {
  cat("<KorAPCorpusStats>", "\n")
  if (object@vc == "") {
    cat("The whole corpus")
  } else {
    cat("The virtual corpus described by \"", object@vc, "\"", sep = "")
  }
  cat(
    " contains", formatC(object@tokens, format = "f", digits = 0, big.mark = ","), "tokens in",
    formatC(object@sentences, format = "d", big.mark = ","), "sentences in",
    formatC(object@documents, format = "d", big.mark = ","), "documents.\n"
  )
})
