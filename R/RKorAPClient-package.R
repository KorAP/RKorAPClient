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
#' Kupietz, Marc / Diewald, Nils / Margaretha, Eliza (2020):
#' RKorAPClient: An R package for accessing the German Reference Corpus DeReKo
#' via KorAP. In: Calzolari, Nicoletta, Frédéric Béchet, Philippe Blache,
#' Khalid Choukri, Christopher Cieri,  Thierry Declerck, Sara Goggi,
#' Hitoshi Isahara, Bente Maegaard, Joseph Mariani, Hélène Mazo,
#' Asuncion Moreno, Jan Odijk, Stelios Piperidis (eds.):
#' Proceedings of The 12th Language Resources and Evaluation Conference (LREC 2020)
#' Marseille: European Language Resources Association (ELRA), 7017-7023.
#' \url{http://www.lrec-conf.org/proceedings/lrec2020/pdf/2020.lrec-1.867.pdf}
#'
#' @keywords internal
"_PACKAGE"
#' [1] "_PACKAGE"
#'
