#' @include KorAPQuery.R
setGeneric("textFrequencyQuery", function(kco, ...) standardGeneric("textFrequencyQuery"))

#' Query frequencies per text
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Frequencies of one or more queries in every single text of one or more virtual
#' corpora, including texts without any hit. In contrast to [frequencyQuery()],
#' which pools all texts of a virtual corpus, the result shows how texts differ,
#' e.g. with [ec_boxplot()], and allows for tests that treat texts rather than
#' tokens as independent observations.
#'
#' @details
#' Each text takes one request for its size and one per query. On corpora like
#' DeLiKo@@DNB, a request usually takes well under a second. On a corpus as large
#' as DeReKo, it can take several seconds, so that only few texts are feasible.
#' Restrict `vc` to a sample of texts then, see the section on slow queries in
#' [corpusQuery()], e.g. `"textType = /.*Krimi.*/ & textSigle = /.*5/"`.
#' Failed or incomplete requests abort the analysis rather than becoming zeros.
#' Run it again in this case: with the connection's `cache`, only the missing
#' requests are repeated.
#'
#' Texts are found with the query `<base/s=t>`.
#'
#' @aliases textFrequencyQuery
#' @inheritParams frequencyQuery,KorAPConnection-method
#' @param query string or vector of query strings. Names become the column `variant`.
#' @param vc string or vector of virtual corpus definitions. Names become the column `label`.
#' @param fields metadata fields to add to each text.
#' @param maxTexts maximum number of texts, as a safeguard against accidentally
#'   sending very many requests.
#' @param verbose print some info
#' @param ... further arguments passed to [corpusQuery()], e.g. `ql`.
#' @return Tibble with one row per query, virtual corpus and text, and the columns
#'   `query`, `vc`, `textSigle`, the `fields`, `totalResults` (hits in the text),
#'   `total` (tokens of the text), `f` (their ratio, `NA` for empty texts) and
#'   `webUIRequestUrl`, plus `variant` and `label` where `query` or `vc` are named.
#' @family frequency analysis
#' @seealso [frequencyQuery()], [ec_boxplot()]
#' @examples
#' \dontrun{
#' KorAPConnection("https://korap.dnb.de") |>
#'   textFrequencyQuery("Himmel",
#'     vc = c(Nominees = "award = buchpreis & pubDate in 2020",
#'            Others = "award != buchpreis & pubDate in 2020 & textSigle = /.*05/")) |>
#'   ec_boxplot(f * 1e6, label, pointLabel = title, ylabel = "IPM")
#' }
#' @export
setMethod(
  "textFrequencyQuery", "KorAPConnection",
  function(kco, query, vc = "", fields = c("author", "title", "pubDate"),
           maxTexts = 5000, verbose = kco@verbose, cacheAs = NULL, ...) {
    if (!is.character(query) || !length(query) || anyNA(query) || any(!nzchar(query)) || anyDuplicated(query)) {
      stop("query must contain distinct, non-empty query strings.", call. = FALSE)
    }
    if (!is.character(vc) || !length(vc) || anyNA(vc) || anyDuplicated(vc)) {
      stop("vc must contain distinct virtual corpus definitions.", call. = FALSE)
    }
    if (!is.numeric(maxTexts) || length(maxTexts) != 1L || is.na(maxTexts) || maxTexts < 1) {
      stop("maxTexts must be a positive number or Inf.", call. = FALSE)
    }
    cacheRecord <- NULL
    if (!is.null(cacheAs)) {
      cacheAs <- cacheAsFileName(cacheAs)
      cacheRecord <- cacheAsRecord(environment(), list(...), kco, ignore = "maxTexts")
      cached <- readCacheAs(cacheAs, kco, cacheRecord, "text frequency query")
      if (!is.null(cached)) return(cached)
    }

    strict <- function(expr) strictDispersionRequest(expr, "Text frequency analysis")
    fields <- unique(c("textSigle", fields))
    inventories <- lapply(unname(vc), function(one) {
      inventory <- strict(corpusQuery(kco, "<base/s=t>", vc = one, fields = fields, verbose = verbose))
      checkDispersionQuery(inventory)
      if (inventory@totalResults == 0) stop("No texts found in the virtual corpus \u201c", one, "\u201d.", call. = FALSE)
      inventory
    })
    n <- sum(vapply(inventories, function(inventory) as.numeric(inventory@totalResults), 1))
    if (n > maxTexts) {
      stop(format_count(n), " texts are more than maxTexts = ", format_count(maxTexts),
        ". Restrict vc, e.g. to a sample with & textSigle = /.*5/, or increase maxTexts.", call. = FALSE)
    }
    labels <- vcLabels(vc)
    texts <- lapply(seq_along(vc), function(i) {
      found <- strict(fetchAll(inventories[[i]], verbose = verbose))@collectedMatches
      found <- found[!duplicated(found$textSigle), intersect(fields, names(found)), drop = FALSE]
      if (nrow(found) != inventories[[i]]@totalResults) {
        stop("The list of texts in \u201c", vc[[i]], "\u201d is incomplete.", call. = FALSE)
      }
      found <- tibble::add_column(tibble::as_tibble(found), vc = unname(vc[i]), .before = 1L)
      if (!is.null(labels)) found <- tibble::add_column(found, label = labels[i], .after = "vc")
      found
    }) |> bind_rows()

    # a text can belong to several virtual corpora
    sigles <- unique(texts$textSigle)
    textVcs <- paste("textSigle =", vapply(sigles, quoteDispersionValue, ""))
    sizes <- dispersionCorpusSizes(kco, textVcs, "texts", verbose)
    counts <- strict(corpusQuery(kco, query = unname(query), vc = textVcs, metadataOnly = TRUE,
      as.df = TRUE, expand = TRUE, verbose = verbose, ...))
    if (nrow(counts) != length(query) * length(sigles) || any(!is.finite(counts$totalResults))) {
      stop("Text frequency analysis did not return a complete set of hit counts.", call. = FALSE)
    }

    result <- lapply(unname(query), function(one) {
      found <- counts[counts$query == one, ]
      at <- match(texts$textSigle, sigles)
      row <- match(textVcs[at], found$vc)
      rows <- tibble::add_column(texts, query = one, .before = 1L)
      rows$totalResults <- found$totalResults[row]
      rows$total <- sizes[at]
      rows$f <- ifelse(rows$total > 0, rows$totalResults / rows$total, NA_real_)
      rows$webUIRequestUrl <- found$webUIRequestUrl[row]
      rows
    }) |> bind_rows()
    if (!is.null(names(query))) {
      variants <- ifelse(is.na(names(query)) | !nzchar(names(query)), query, names(query))
      result <- tibble::add_column(result, variant = unname(variants[match(result$query, query)]), .after = "query")
    }

    if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, cacheRecord, "text frequency query", result)
    result
  }
)
