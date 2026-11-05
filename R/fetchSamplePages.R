setGeneric("fetchSamplePages", function(kqo, ...) standardGeneric("fetchSamplePages"))

#' Fetch a reproducible sample of result pages
#'
#' Select complete result pages without replacement. This is a page (cluster)
#' sample, not a simple random sample of individual hits. Hits within a selected
#' page remain in Lucene index order. A single randomly selected page therefore
#' does not provide independently sampled hits. If all results fit within the
#' limit, all pages are fetched in index order and no sample is drawn.
#'
#' @param kqo A [corpusQuery()] result, for one or several queries.
#' @param maxHits Maximum hits to retrieve per query. Complete pages are used, so a
#'   finite limit is rounded down to a multiple of 50. At least 50 is required
#'   unless the complete result fits within `maxHits`. `Inf` fetches all results.
#' @param seed Seed for choosing pages; the caller's random state is restored.
#' @param verbose Whether to print progress.
#' @param warn Emit the page-sampling warning, or an informational message if
#'   all hits are fetched. Defaults to `TRUE`; the procedure
#'   remains recorded in the result even when this is explicitly disabled.
#' @return Updated query object(s), with hits in `collectedMatches` and page
#'   sampling provenance in the `korapSampling` attribute. [dispersionAnalysis()] uses
#'   this exact sample and its inclusion probability without drawing again.
#'   Single-valued metadata columns are atomic vectors, with `NA` for missing
#'   values; genuinely multivalued columns remain lists.
#' @details
#' Unlike taking the first hits, pages are selected from the entire result set.
#' If several pages are available, even one page can be selected at random.
#' Within-page ordering and clustering remain; this distinction matters when
#' interpreting estimates and uncertainty. The actual row count can be below
#' `maxHits`, particularly when the last, shorter page is selected.
#'
#' Call [fetchAll()] to replace a page sample with the complete result. A page
#' sample is not a sequential prefix, so [fetchNext()] and [fetchRest()] cannot
#' safely continue it. Call `fetchSamplePages()` again to draw a new sample.
#' @examples
#' \dontrun{
#' KorAPConnection() |>
#'   corpusQuery("Gendern") |>
#'   fetchSamplePages(maxHits = 2000, seed = 7) |>
#'   dispersionAnalysis(by = "source")
#' }
#' @aliases fetchSamplePages
#' @export
setMethod("fetchSamplePages", "KorAPQuery",
  function(kqo, maxHits = 2000, seed = 7, verbose = kqo@korapConnection@verbose, warn = TRUE) {
    validatePageSample(maxHits, seed, warn)
    checkDispersionQuery(kqo)
    pageSampleWarning(kqo@totalResults, maxHits, warn)
    sample <- sampleQueryPages(kqo, maxHits, seed, verbose, useExisting = FALSE)
    result <- sample$query
    result@collectedMatches <- sample$matches
    attr(result, "korapSampling") <- c(sample[c("method", "seed", "pages", "inclusionProbability")],
      list(populationSize = kqo@totalResults, sampleSize = nrow(sample$matches),
           requestedLimit = maxHits, pageSize = maxResultsPerPage, hitOrder = "index"))
    result
  })

#' @rdname fetchSamplePages-KorAPQuery-method
#' @export
setMethod("fetchSamplePages", "data.frame",
  function(kqo, maxHits = 2000, seed = 7, verbose = NULL, warn = TRUE) {
    validatePageSample(maxHits, seed, warn)
    queries <- korapQueryObjects(kqo)
    invisible(lapply(queries, checkDispersionQuery))
    pageSampleWarning(vapply(queries, function(q) q@totalResults, numeric(1)), maxHits, warn)
    queries <- lapply(queries, function(q) fetchSamplePages(q, maxHits = maxHits, seed = seed,
      verbose = if (is.null(verbose)) q@korapConnection@verbose else verbose, warn = FALSE))
    replaceKorapQueryObjects(kqo, queries)
  })

#' @rdname fetchAll-KorAPQuery-method
#' @export
setMethod("fetchAll", "data.frame", function(kqo, verbose = NULL, ...) {
  queries <- lapply(korapQueryObjects(kqo), function(q) fetchAll(q,
    verbose = if (is.null(verbose)) q@korapConnection@verbose else verbose, ...))
  replaceKorapQueryObjects(kqo, queries)
})

#' @rdname fetchNext-KorAPQuery-method
#' @export
setMethod("fetchNext", "data.frame", function(kqo, ..., verbose = NULL) {
  queries <- lapply(korapQueryObjects(kqo), function(q) fetchNext(q,
    verbose = if (is.null(verbose)) q@korapConnection@verbose else verbose, ...))
  replaceKorapQueryObjects(kqo, queries)
})

#' @rdname fetchRest-KorAPQuery-method
#' @export
setMethod("fetchRest", "data.frame", function(kqo, verbose = NULL, ...) {
  queries <- lapply(korapQueryObjects(kqo), function(q) fetchRest(q,
    verbose = if (is.null(verbose)) q@korapConnection@verbose else verbose, ...))
  replaceKorapQueryObjects(kqo, queries)
})

korapQueryObjects <- function(x) {
  queries <- attr(x, "korapQueries")
  if (!is.data.frame(x) || !is.list(queries) || !length(queries) || length(queries) != nrow(x) ||
      !all(vapply(queries, inherits, logical(1), what = "KorAPQuery"))) {
    stop("Use the unmodified multi-query result of corpusQuery(..., as.df = FALSE).", call. = FALSE)
  }
  valid <- vapply(seq_along(queries), function(i) {
    parameters <- httr2::url_parse(queries[[i]]@requestUrl)$query
    identical(unname(as.character(x$query[i])), parameters$q) &&
      identical(unname(as.character(x$vc[i])), queries[[i]]@vc)
  }, logical(1))
  if (!all(valid)) stop("The query-summary rows no longer match their query objects.", call. = FALSE)
  queries
}

replaceKorapQueryObjects <- function(x, queries) {
  attr(x, "korapQueries") <- queries
  x
}

validatePageSample <- function(maxHits, seed, warn) {
  if (!is.numeric(maxHits) || length(maxHits) != 1 || is.na(maxHits) || maxHits <= 0 ||
      (is.finite(maxHits) && maxHits != floor(maxHits))) stop("maxHits must be a positive integer or Inf.", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != floor(seed)) stop("seed must be a non-negative integer.", call. = FALSE)
  if (!is.logical(warn) || length(warn) != 1 || is.na(warn)) stop("warn must be TRUE or FALSE.", call. = FALSE)
}

pageSampleWarning <- function(populations, maxHits, warn) {
  if (!warn) return(invisible(NULL))
  if (all(populations <= maxHits)) {
    message("All query results fit within maxHits: complete results are fetched in index order; no random sample is drawn.")
  } else {
    warning("Sampling selects result pages, not independent hits. Hits within pages remain in Lucene index order; clustering can strongly bias dispersion scores. Queries fitting within maxHits are fetched completely.", call. = FALSE)
  }
}

completeDispersionMetadata <- function(query) {
  matches <- query@collectedMatches
  !identical(attr(query, "korapMetadataValues"), "joined") &&
    !is.null(matches) && nrow(matches) == query@totalResults &&
    all(query@fields %in% names(matches)) &&
    (!"matchID" %in% names(matches) || !anyDuplicated(matches$matchID))
}

sampleQueryPages <- function(query, limit, seed, verbose,
                             selectedPages = NULL, useExisting = TRUE) {
  n <- query@totalResults
  existing <- query@collectedMatches
  if (useExisting && completeDispersionMetadata(query)) {
    return(list(query = query, matches = existing, method = "existing_metadata", seed = NULL,
      pages = integer(), inclusionProbability = 1))
  }
  if (is.null(selectedPages) && n > limit && limit < maxResultsPerPage) {
    stop("Page sampling requires a hit limit >= 50 unless all results fit within the limit.", call. = FALSE)
  }
  totalPages <- ceiling(n / maxResultsPerPage)
  nPages <- if (n <= limit) totalPages else floor(limit / maxResultsPerPage)
  oldSeed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(oldSeed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", oldSeed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  pages <- if (!is.null(selectedPages)) selectedPages else if (nPages == totalPages) seq_len(totalPages) - 1 else sample.int(totalPages, nPages) - 1
  nPages <- length(pages)
  attr(query, "korapSampling") <- NULL
  query@collectedMatches <- NULL
  for (page in pages) {
    before <- if (is.null(query@collectedMatches)) 0 else nrow(query@collectedMatches)
    expected <- min(maxResultsPerPage, n - page * maxResultsPerPage)
    query <- strictDispersionRequest(fetchNext(query, offset = page * maxResultsPerPage,
      maxFetch = expected, verbose = verbose, randomizePageOrder = FALSE,
      preserveMetadataValues = TRUE))
    checkDispersionQuery(query)
    if (is.null(query@collectedMatches) || nrow(query@collectedMatches) - before != expected) {
      stop("Result retrieval was incomplete; no page sample was returned.", call. = FALSE)
    }
  }
  matches <- if (is.null(query@collectedMatches)) tibble::tibble() else query@collectedMatches
  query@collectedMatches <- matches
  list(query = query, matches = matches, method = if (nPages == totalPages) "all_metadata" else "random_pages",
    seed = seed, pages = pages, inclusionProbability = if (totalPages == 0) 1 else nPages / totalPages)
}
