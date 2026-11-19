#' R Client for KorAP Corpus Analysis Platform
#'
#' @description
#' RKorAPClient provides programmatic access to KorAP corpus analysis platform instances,
#' enabling corpus linguistic research on large corpora like DeReKo, CoRoLa, DeLiKo@@DNB.
#'
#' @section Main Functions:
#' \describe{
#'   \item{Connection}{`KorAPConnection()`, `auth()`, `persistAccessToken()`}
#'   \item{Search}{`corpusQuery()`, `fetchAll()`, `fetchNext()`}
#'   \item{Analysis}{`corpusStats()`, `frequencyQuery()`, `collocationAnalysis()`}
#' }
#'
#' @section Quick Start:
#' ```r
#' library(RKorAPClient)
#' # Connect and search
#' kcon <- KorAPConnection()
#' query <- corpusQuery(kcon, "Ameisenplage")
#' results <- fetchAll(query)
#'
#' # Access results
#' results@collectedMatches
#' results@totalResults
#' ```
#'
#' @section Common Workflows:
#'
#' **Basic Search:**
#' ```r
#' kcon <- KorAPConnection()
#' kcon |> corpusQuery("search term") |> fetchAll()
#' ```
#'
#' **Frequency Analysis:**
#' ```r
#' frequencyQuery(kcon, c("term1", "term2"), vc="pubDate in 2020")
#' ```
#'
#' **Corpus Statistics:**
#' ```r
#' corpusStats(kcon, vc="textType=Zeitung", as.df=TRUE)
#' ```
#'
#' @references
#' Kupietz, M., Diewald, N., and Margaretha, E. (2020).
#' [RKorAPClient: An R Package for Accessing the German Reference Corpus DeReKo
#' via KorAP](https://aclanthology.org/2020.lrec-1.867/).
#' Proceedings of LREC 2020, pp. 7015–7021.
#'
#' Kupietz, M., Diewald, N., and Margaretha, E. (2022).
#' Building Paths to Corpus Data: A Multi-Level Least Effort and Maximum Return
#' Approach.
#' In D. Fišer and A. Witt (eds.), CLARIN: The Infrastructure for Language
#' Resources, pp. 163–189. De Gruyter.
#' \doi{10.1515/9783110767377-007}.
#'
#' @keywords internal
"_PACKAGE"
#' [1] "_PACKAGE"
#'
