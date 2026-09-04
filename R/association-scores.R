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
#'
#' @description
#' Functions to calculate different collocation association scores between
#' a node (target word) and words in a window around the it.
#' The functions are primarily used by [collocationScoreQuery()].
#'
#' @section Salience versus surprise:
#'
#' `logDice` is the only one of these scores that does not compare the observed
#' co-occurrence frequency to an expected one. As it merely puts `O` into the
#' numerator of the Dice coefficient, the difference between the score of a pair
#' and the score that same pair would get if it co-occurred exactly as often as
#' expected is precisely the pointwise mutual information:
#'
#' `logDice(O) - logDice(E) = log2(O / E) = pmi`
#'
#' The level a pair starts from therefore depends on the marginal frequencies
#' alone, and it is high whenever both words are frequent. Collocates of *Grund*
#' in DeReKo, in a window of five words to each side, illustrate this:
#'
#' * *triftiger* is rare, so it starts from a logDice of -4.60 and reaches 3.96,
#'   exceeding what is expected by a `pmi` of 8.57
#' * *Berlin* is frequent, so it starts from 5.58 and still reaches 3.84, while
#'   co-occurring 1.74 bits *less* often than expected
#'
#' A frequent collocate can thus reach a respectable logDice although the node
#' does not attract it at all. logDice measures how salient a pair is, given how
#' often its words occur, rather than how surprising it is. That is what it was
#' designed for (Rychlý 2008), and it is why its values do not depend on the
#' corpus size and are comparable across corpora.
#'
#' Since [collocationAnalysis()] ranks and thresholds by logDice, it therefore
#' drops collocates occurring less often than expected by default. Its
#' `minObservedExpectedRatio` parameter controls this: raise it to demand a
#' stronger contrast, or set it to 0 to see the unfiltered ranking, for instance
#' to study repulsion. [collocationScoreQuery()] does not filter, as there the
#' pairs to score are given explicitly. For results obtained otherwise,
#' `dplyr::filter(O > E)` or a minimum `pmi` or `ll` has the same effect.
NULL
#' NULL

#' @rdname association-score-functions
#'
#' @family collocation analysis functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' KorAPConnection(verbose = TRUE) %>%
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
#' @description
#' **pmi**: pointwise mutual information
#'
#' @export
#'
pmi <- function(O1, O2, O, N, E, window_size) {
  log2(O / E)
}

#' @rdname association-score-functions
#'
#' @description
#' **mi2**: pointwise mutual information squared (Daille 1994), also referred to as mutual dependency
#' (Thanopoulos et al. 2002)
#' @export
#'
mi2 <- function(O1, O2, O, N, E, window_size) {
  log2(O ^ 2 / E)
}

#' @rdname association-score-functions
#' @family association-score-functions
#'
#' @description
#' **mi3**: pointwise mutual information cubed (Daille 1994), also referred to as log-frequency biased mutual dependency)
#' (Thanopoulos et al. 2002)
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
#'
#' @description
#' **logDice**: log-Dice coefficient, a heuristic measure that is popular in lexicography (Rychlý 2008)
#'
#' @details
#' `logDice` is computed as defined by Rychlý (2008), that is from the plain
#' marginal frequencies of node and collocate, so that its values are
#' comparable to those of other tools, such as Sketch Engine. Unlike the
#' expectation based scores, it does not take the window size into account: the
#' Dice coefficient relates the co-occurrence frequency to how often the two
#' words occur at all, and `O1` and `O2` count word tokens, while a window size
#' factor would count window positions.
#'
#' @export
#'
#' @references
#' Rychlý, Pavel (2008):  A lexicographer-friendly association score. In Proceedings of Recent Advances in Slavonic Natural Language Processing, RASLAN, 6–9. <https://www.fi.muni.cz/usr/sojka/download/raslan2008/13.pdf>.
#'

logDice <-  function(O1, O2, O, N, E, window_size) {
  14 + log2(2 * O / (O1 + O2))
}


#' Log likelihood
#'
#' @rdname association-score-functions
#' @description
#' **ll**: log-likelihood (Dunning 1993) using Stefan Evert's (2004) simplified implementation
#'
#' @export
#'
#' @importFrom dplyr if_else
#'
#' @references
#' Dunning, T. (1993): Accurate methods for the statistics of surprise and coincidence. Comput. Linguist. 19, 1 (March 1993), 61-74.
#'
#' Evert, Stefan (2004): The Statistics of Word Cooccurrences: Word Pairs and Collocations. PhD dissertation, IMS, University of Stuttgart. Published in 2005, URN urn:nbn:de:bsz:93-opus-23714.
#' Free PDF available from <https://purl.org/stefan.evert/PUB/Evert2004phd.pdf>
#'
ll <- function(O1, O2, O, N, E, window_size) {
  # The contingency table classifies co-occurrence tokens, not corpus tokens:
  # with a window of `window_size` positions, every occurrence of a word takes
  # part in that many pairs, so all of the sample size and both marginals scale
  # with it. Scaling only the row, as this did before, leaves a table whose
  # cells do not add up to one sample, and lets `N - window_size * O1` turn
  # negative for a frequent node in a wide window. The expected co-occurrence
  # frequency is unaffected, being `window_size * O1 * O2 / N` either way, which
  # is why the other scores do not depend on this.
  total = as.double(N) * window_size
  r1 = as.double(O1) * window_size
  r2 = total - r1
  c1 = as.double(O2) * window_size
  c2 = total - c1
  o11 = O
  o12 = r1 - o11
  o21 = c1 - o11
  o22 = r2 - o21
  e11 = r1 * c1 / total
  e12 = r1 * c2 / total
  e21 = r2 * c1 / total
  e22 = r2 * c2 / total
  2 * ( dplyr::if_else(o11>0, o11 * log(o11/e11), 0)
        + dplyr::if_else(o12>0, o12 * log(o12/e12), 0)
        + dplyr::if_else(o21>0, o21 * log(o21/e21), 0)
        + dplyr::if_else(o22>0, o22 * log(o22/e22), 0))
}
