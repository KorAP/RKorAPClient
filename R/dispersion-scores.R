#' Dispersion measures and user-defined scores
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculate dispersion from absolute frequencies and corpus-part sizes. The
#' parts must be disjoint; include positive-sized parts without hits. Sizes are
#' token counts (or proportional sizes), not the frequencies of the query.
#' The analysis interface and result structure may change in future releases.
#'
#' @param frequencies Numeric vector of observed or estimated hit counts, or a
#'   [corpusQuery()] result; see [dispersionAnalysis,KorAPQuery-method] for the latter.
#' @param sizes Numeric vector of corresponding corpus-part sizes.
#' @param scoreFunctions Named list of functions with arguments
#'   `function(frequencies, sizes)`, each returning one numeric value. As with
#'   [defaultAssociationScoreFunctions()], replace or extend the list to use
#'   your own measures. Each function receives the absolute input values,
#'   with zero-sized, zero-hit parts removed.
#'
#' @details
#' `dispersionDP()` computes `sum(abs(frequencies / sum(frequencies) -
#' sizes / sum(sizes))) / 2`. `dispersionDPnorm()` divides DP by
#' `1 - min(sizes / sum(sizes))`, following the corrected normalization in
#' Lijffijt and Gries (2012). Smaller DP and DPnorm mean more even distribution.
#' Juilland's D, Carroll's D2 and Rosengren's S use the versions for unequal
#' part sizes described by Gries (2020); larger values mean more even
#' distribution. These are independent implementations of the published
#' formulas; Gries' companion script is not included or required.
#' `dispersionRange()`
#' counts parts with hits, not documents unless documents are the parts.
#'
#' The built-in measures other than range return `NA_real_` if there are no
#' hits or fewer than two positive-sized parts. Range is zero without hits.
#' No measure is assumed to be appropriate for every research question, and
#' the measures are not combined into a rank or adjusted frequency.
#'
#' @section Interpretation:
#' Range describes observed reach: the number of parts containing at least one
#' hit. It ignores part sizes and within-part frequencies, and depends on the
#' total number of hits. Broader reach need not mean more even distribution.
#'
#' DP compares each part's share of hits with its share of corpus tokens.
#' For example, DP = 0.20 means that 20 percent of the hit distribution would
#' have to be reallocated between parts to match the token shares, keeping the
#' total fixed. This is hypothetical hit mass, allowing fractional counts;
#' it is not a percentage of sources or speakers. Smaller is closer, and zero
#' is an exact match. DPnorm rescales DP by its maximum for the given part sizes;
#' the redistribution interpretation applies to DP itself, not directly to DPnorm.
#'
#' Juilland's D describes variation in relative hit frequencies across parts;
#' Carroll's D2 describes their entropy. Both equal one when each part has the
#' same relative frequency. Rosengren's S compares the hit and token
#' distributions, reaching one when they match. Larger D, D2 and S mean more
#' even distribution under their respective definitions. The measures emphasize
#' different aspects, so they need not rank variants in the same order.
#'
#' Compare variants within the same corpus and partition. There is no universal
#' cutoff for good dispersion, and these descriptive scores are not significance
#' tests. Averaging ranks would depend on the selected measures, give related
#' measures repeated votes and discard the size of differences. Inspect the
#' underlying part counts and token sizes, and report disagreements explicitly.
#' The leading one or two sources' shares of hits, considered alongside their
#' token shares, can help explain concentration. Concordances are needed to
#' investigate possible house styles, quotation or syndicated material.
#'
#' @return `dispersionAnalysis()` returns a one-row tibble, one column per score.
#'   `defaultDispersionFunctions()` returns a named list of functions.
#'   Individual score functions return a numeric scalar.
#' @references
#' Gries, Stefan Th. (2008). Dispersions and adjusted frequencies in corpora.
#' International Journal of Corpus Linguistics, 13(4), 403-437.
#' \doi{10.1075/ijcl.13.4.02gri}.
#'
#' Gries, Stefan Th. (2020). Analyzing dispersion. In M. Paquot & S. Th. Gries
#' (eds.), A Practical Handbook of Corpus Linguistics, 99-118.
#' \doi{10.1007/978-3-030-46216-1_5}.
#' Companion resources: \url{https://www.stgries.info/research/dispersion/links.html}.
#'
#' Lijffijt, Jefrey & Gries, Stefan Th. (2012). Correction to
#' "Dispersions and adjusted frequencies in corpora". International Journal of
#' Corpus Linguistics, 17(1), 147-149. \doi{10.1075/ijcl.17.1.08lij}.
#'
#' Juilland, Alphonse G. & Chang-Rodriguez, Eugenio (1964).
#' Frequency dictionary of Spanish words. The Hague: Mouton.
#'
#' Carroll, John B. (1970). An alternative to Juilland's usage coefficient for
#' lexical frequencies, and a proposal for a standard frequency index (SFI).
#' Computer Studies in the Humanities and Verbal Behavior, 3(2), 61-65.
#'
#' Rosengren, Inger (1971). The quantitative concept of language and its
#' relation to the structure of frequency dictionaries.
#' Etudes de linguistique appliquee (Nouvelle Serie), 1, 103-127.
#' @name dispersion-score-functions
#' @examples
#' dispersionAnalysis(c(1, 2, 3, 4, 5), c(9, 10, 10, 10, 11))
#' dispersionAnalysis(c(10, 0), c(100, 100), scoreFunctions = list(
#'   DP = dispersionDP,
#'   evenness = function(frequencies, sizes) 1 - dispersionDP(frequencies, sizes)
#' ))
NULL

#' @rdname dispersion-score-functions
#' @export
defaultDispersionFunctions <- function() {
  list(DP = dispersionDP, DPnorm = dispersionDPnorm,
       JuillandD = dispersionJuillandD, CarrollD2 = dispersionCarrollD2,
       RosengrenS = dispersionRosengrenS, range = dispersionRange)
}

setGeneric("dispersionAnalysis", function(frequencies, ...) standardGeneric("dispersionAnalysis"))

#' @rdname dispersion-score-functions
#' @aliases dispersionAnalysis
#' @export
setMethod("dispersionAnalysis", "numeric", function(frequencies, sizes,
                       scoreFunctions = defaultDispersionFunctions()) {
  input <- dispersionInput(frequencies, sizes)
  validateDispersionFunctions(scoreFunctions)
  scores <- lapply(names(scoreFunctions), function(name) {
    value <- scoreFunctions[[name]](input$frequencies, input$sizes)
    if (is.logical(value) && length(value) == 1L && is.na(value)) value <- NA_real_
    if (!is.numeric(value) || length(value) != 1L || is.nan(value) ||
        (!is.na(value) && !is.finite(value))) {
      stop("Dispersion function '", name, "' must return one finite numeric value or NA.", call. = FALSE)
    }
    as.numeric(value)
  })
  tibble::as_tibble(stats::setNames(scores, names(scoreFunctions)))
})

validateDispersionFunctions <- function(scoreFunctions) {
  labels <- names(scoreFunctions)
  if (!is.list(scoreFunctions) || !length(scoreFunctions) ||
      is.null(labels) || anyNA(labels) || any(!nzchar(labels)) ||
      anyDuplicated(labels) || !all(vapply(scoreFunctions, is.function, logical(1)))) {
    stop("scoreFunctions must be a non-empty, uniquely named list of functions.", call. = FALSE)
  }
  invisible(NULL)
}

dispersionInput <- function(frequencies, sizes) {
  if (!is.numeric(frequencies) || !is.numeric(sizes) ||
      !length(frequencies) || length(frequencies) != length(sizes) ||
      any(!is.finite(frequencies)) || any(!is.finite(sizes)) ||
      any(frequencies < 0) || any(sizes < 0) ||
      any(sizes == 0 & frequencies > 0) || !is.finite(sum(sizes)) ||
      sum(sizes) <= 0 || !is.finite(sum(frequencies))) {
    stop("frequencies and sizes must be matching, finite, non-negative numeric vectors, with positive total size and no hits in zero-sized parts.", call. = FALSE)
  }
  keep <- sizes > 0
  list(frequencies = frequencies[keep], sizes = sizes[keep])
}

dispersionProportions <- function(frequencies, sizes) {
  input <- dispersionInput(frequencies, sizes)
  if (sum(input$frequencies) == 0 || length(input$sizes) < 2) return(NULL)
  list(p = input$frequencies / sum(input$frequencies),
       s = input$sizes / sum(input$sizes))
}

#' @rdname dispersion-score-functions
#' @export
dispersionDP <- function(frequencies, sizes) {
  x <- dispersionProportions(frequencies, sizes)
  if (is.null(x)) NA_real_ else sum(abs(x$p - x$s)) / 2
}

#' @rdname dispersion-score-functions
#' @export
dispersionDPnorm <- function(frequencies, sizes) {
  x <- dispersionProportions(frequencies, sizes)
  if (is.null(x)) NA_real_ else sum(abs(x$p - x$s)) / (2 * (1 - min(x$s)))
}

#' @rdname dispersion-score-functions
#' @export
dispersionJuillandD <- function(frequencies, sizes) {
  x <- dispersionProportions(frequencies, sizes)
  if (is.null(x)) return(NA_real_)
  rates <- x$p / x$s
  1 - sqrt(mean((rates - mean(rates))^2)) / mean(rates) / sqrt(length(rates) - 1)
}

#' @rdname dispersion-score-functions
#' @export
dispersionCarrollD2 <- function(frequencies, sizes) {
  x <- dispersionProportions(frequencies, sizes)
  if (is.null(x)) return(NA_real_)
  rates <- x$p / x$s
  shares <- rates[rates > 0] / sum(rates)
  -sum(shares * log2(shares)) / log2(length(rates))
}

#' @rdname dispersion-score-functions
#' @export
dispersionRosengrenS <- function(frequencies, sizes) {
  x <- dispersionProportions(frequencies, sizes)
  if (is.null(x)) NA_real_ else sum(sqrt(x$p * x$s))^2
}

#' @rdname dispersion-score-functions
#' @export
dispersionRange <- function(frequencies, sizes) {
  x <- dispersionInput(frequencies, sizes)
  sum(x$frequencies > 0)
}
