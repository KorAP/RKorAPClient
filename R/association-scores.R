#' Association score functions
#'
#' @param O1            observed absolute frequency of node
#' @param O2            observed absolute frequency of collocate
#' @param O             observed absolute frequency of collocation
#' @param N             corpus size
#' @param E             expected absolute frequency of collocation (already adjusted to window size)
#' @param window_size   total window size around node (left neighbour count + right neighbour count)
#'
#' @return              association score
#' @name association-score-functions
#' @description
#' Functions to calculate different collocation association scores between
#' a node (target word) and words in a window around the it.
#' The functions are primarily used by \code{\link{collocationScoreQuery}}.
NULL
#' NULL

#' @rdname association-score-functions
#'
#' @export
#'
#' @examples
#' \donttest{
#' new("KorAPConnection", verbose = TRUE) %>%
#' collocationScoreQuery("Perlen", c("verziertes", "Säue"),
#'   scoreFunctions = append(defaultAssociationScoreFunctions(),
#'      list(localMI = function(O1, O2, O, N, E, window_size) {
#'                        O * log2(O/E)
#'                     })))
#' }
#'
defaultAssociationScoreFunctions <- function() {
  list(pmi=pmi, mi2=mi2, mi3=mi3, logDice=logDice, ll=ll)
}

#' @rdname association-score-functions
#'
#' @export
#'
pmi <- function(O1, O2, O, N, E, window_size) {
  log2(O / E)
}

#' @rdname association-score-functions
#'
#' @export
#'
mi2 <- function(O1, O2, O, N, E, window_size) {
  log2(O ^ 2 / E)
}

#' @rdname association-score-functions
#' @family association-score-functions
#'
#'
#' @export
#'
#' @references
#' Daille, B. (1994): Approche mixte pour l’extraction automatique de terminologie: statistiques lexicales et filtres linguistiques. PhD thesis, Université Paris 7.
#'
#' Thanopoulos, A., Fakotakis, N., Kokkinakis, G. (2002): Comparative evaluation of collocation extraction metrics. In: Proc. of LREC 2002: 620–625.
#'
mi3 <- function(O1, O2, O, N, E, window_size) {
  log2(O ^ 3 / E)
}

#' @rdname association-score-functions
#' @family association-score-functions
#' @export
#'
#' @references
#' Rychlý, Pavel (2008):  A lexicographer-friendly association score. In Proceedings of Recent Advances in Slavonic Natural Language Processing, RASLAN, 6–9. <http://www.fi.muni.cz/usr/sojka/download/raslan2008/13.pdf>.
#'

logDice <-  function(O1, O2, O, N, E, window_size) {
  14 + log2(2 * O / (window_size * O2 + O1))
}


#' Log likelihood
#'
#' @rdname association-score-functions
#' @family association-score-functions
#'
#' @export
#'
#' @importFrom dplyr if_else
#'
#' @references
#' Dunning, T. (1993): Accurate methods for the statistics of surprise and coincidence. Comput. Linguist. 19, 1 (March 1993), 61-74.
#'
#' Evert, Stefan (2004): The Statistics of Word Cooccurrences: Word Pairs and Collocations. PhD dissertation, IMS, University of Stuttgart. Published in 2005, URN urn:nbn:de:bsz:93-opus-23714.
#' Free PDF available from <http://purl.org/stefan.evert/PUB/Evert2004phd.pdf>
#'
ll <- function(O1, O2, O, N, E, window_size) {
  r1 = as.double(O1) * window_size
  r2 = as.double(N) - r1
  c1 = O2
  c2 = N - c1
  o11 = O
  o12 = r1 - o11
  o21 = c1 - O
  o22 = r2 - o21
  e11 = r1 * c1 / N
  e12 = r1 * c2 / N
  e21 = r2 * c1 / N
  e22 = r2 * c2 / N
  2 * ( dplyr::if_else(o11>0, o11 * log(o11/e11), 0)
        + dplyr::if_else(o12>0, o12 * log(o12/e12), 0)
        + dplyr::if_else(o21>0, o21 * log(o21/e21), 0)
        + dplyr::if_else(o22>0, o22 * log(o22/e22), 0))
}
