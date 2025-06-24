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
#' @return Object containing corpus statistics: `documents`, `tokens`, `sentences`, `paragraphs`
#'
#' @importFrom urltools url_encode
#' @examples
#' \dontrun{
#'
#' kco <- KorAPConnection()
#' corpusStats(kco, "pubDate in 2017 & textType=/Zeitung.*/")
#' }
#'
#' @aliases corpusStats

#' @export
setMethod("corpusStats", "KorAPConnection", function(kco,
                                                     vc = "",
                                                     verbose = kco@verbose,
                                                     as.df = FALSE) {
  if (length(vc) > 1) {
    # ETA calculation for multiple virtual corpora
    total_items <- length(vc)
    start_time <- Sys.time()
    results <- list()
    individual_times <- numeric(total_items)

    for (i in seq_along(vc)) {
      current_vc <- vc[i]
      item_start_time <- Sys.time()

      # Truncate long vc strings for display
      vc_display <- if (nchar(current_vc) > 50) {
        paste0(substr(current_vc, 1, 47), "...")
      } else {
        current_vc
      }

      # Process current virtual corpus
      result <- corpusStats(kco, current_vc, verbose = FALSE, as.df = TRUE)
      results[[i]] <- result

      # Record individual processing time
      item_end_time <- Sys.time()
      individual_times[i] <- as.numeric(difftime(item_end_time, item_start_time, units = "secs"))

      # Format item number with proper alignment
      current_item_formatted <- sprintf(paste0("%", nchar(total_items), "d"), i)

      # Calculate timing and ETA after first few items, using cache-aware approach
      if (i >= 2) {
        eta_info <- calculate_sophisticated_eta(individual_times, i, total_items)
        cache_indicator <- get_cache_indicator(eta_info$is_cached)
        eta_display <- format_eta_display(eta_info$eta_seconds, eta_info$estimated_completion_time)

        log_info(verbose, sprintf(
          "Processed vc %s/%d: \"%s\" in %4.1fs%s%s\n",
          current_item_formatted,
          total_items,
          vc_display,
          individual_times[i],
          cache_indicator,
          eta_display
        ))
      } else {
        # First item, show without ETA
        cache_indicator <- get_cache_indicator(individual_times[i] < 0.1)
        log_info(verbose, sprintf(
          "Processed vc %s/%d: \"%s\" in %4.1fs%s\n",
          current_item_formatted,
          total_items,
          vc_display,
          individual_times[i],
          cache_indicator
        ))
      }
    }

    # Final timing summary with cache analysis
    if (verbose && total_items > 1) {
      total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg_time_per_item <- total_time / total_items
      cached_count <- sum(individual_times < 0.1)
      non_cached_count <- total_items - cached_count

      log_info(verbose, sprintf(
        "Completed processing %d virtual corpora in %s (avg: %4.1fs/item, %d cached, %d non-cached)\n",
        total_items,
        format_duration(total_time),
        avg_time_per_item,
        cached_count,
        non_cached_count
      ))
    }

    do.call(rbind, results)
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
    log_info(verbose, ": ", res$tokens, " tokens\n")
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
