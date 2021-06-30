#' Class KorAPQuery
#'
#' This class provides methods to perform different kinds of queries on the KorAP API server.
#' \code{KorAPQuery} objects, which are typically created by the \code{\link{corpusQuery}} method,
#' represent the current state of a query to a KorAP server.
#'
#' @include KorAPConnection.R
#' @import httr
#'
#' @include RKorAPClient-package.R

#' @export
KorAPQuery <- setClass("KorAPQuery", slots = c(
  "korapConnection",
  "request",
  "vc",
  "totalResults",
  "nextStartIndex",
  "fields",
  "requestUrl",
  "webUIRequestUrl",
  "apiResponse",
  "collectedMatches",
  "hasMoreMatches"
))

#' Method initialize
#'
#' @rdname KorAPQuery-class
#' @param .Object …
#' @param korapConnection KorAPConnection object
#' @param request query part of the request URL
#' @param vc definition of a virtual corpus
#' @param totalResults number of hits the query has yielded
#' @param nextStartIndex at what index to start the next fetch of query results
#' @param fields what data / metadata fields should be collected
#' @param requestUrl complete URL of the API request
#' @param webUIRequestUrl URL of a web frontend request corresponding to the API request
#' @param apiResponse data-frame representation of the JSON response of the API request
#' @param hasMoreMatches logical that signals if more query results can be fetched
#' @param collectedMatches matches already fetched from the KorAP-API-server
#'
#' @importFrom tibble tibble
#' @export
setMethod("initialize", "KorAPQuery",
          function(.Object, korapConnection = NULL, request = NULL, vc="", totalResults=0, nextStartIndex=0, fields=c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
                                                                              "availability", "textClass", "snippet"),
                   requestUrl="", webUIRequestUrl = "", apiResponse = NULL, hasMoreMatches= FALSE, collectedMatches = NULL) {
            .Object <- callNextMethod()
            .Object@korapConnection = korapConnection
            .Object@request = request
            .Object@vc = vc
            .Object@totalResults = totalResults
            .Object@nextStartIndex = nextStartIndex
            .Object@fields = fields
            .Object@requestUrl = requestUrl
            .Object@webUIRequestUrl = webUIRequestUrl
            .Object@apiResponse = apiResponse
            .Object@hasMoreMatches = hasMoreMatches
            .Object@collectedMatches = collectedMatches
            .Object
          })

setGeneric("corpusQuery", function(kco, ...)  standardGeneric("corpusQuery") )
setGeneric("fetchAll", function(kqo, ...)  standardGeneric("fetchAll") )
setGeneric("fetchNext", function(kqo, ...)  standardGeneric("fetchNext") )
setGeneric("fetchRest", function(kqo, ...)  standardGeneric("fetchRest") )
setGeneric("frequencyQuery", function(kco, ...)  standardGeneric("frequencyQuery") )
setGeneric("collocationScoreQuery", function(kco, ...)  standardGeneric("collocationScoreQuery") )

maxResultsPerPage <- 50

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' \bold{\code{corpusQuery}} performs a corpus query via a connection to a KorAP-API-server
#'
#' @param kco \code{\link{KorAPConnection}} object (obtained e.g. from \code{new("KorAPConnection")}
#' @param query string that contains the corpus query. The query language depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}.
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @param metadataOnly logical that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE, unless the connection is authorized (currently not possible).
#' @param ql string to choose the query language (see \href{https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters}{section on Query Parameters} in the Kustvakt-Wiki for possible values.
#' @param fields (meta)data fields that will be fetched for every match.
#' @param accessRewriteFatal abort if query or given vc had to be rewritten due to insufficient rights (not yet implemented).
#' @param verbose print some info
#' @param as.df return result as data frame instead of as S4 object?
#' @param expand logical that decides if \code{query} and \code{vc} parameters are expanded to all of their combinations
#' @return Depending on the \code{as.df} parameter, a table or a \code{\link{KorAPQuery}} object that, among other information, contains the total number of results in \code{@totalResults}. The resulting object can be used to fetch all query results (with \code{\link{fetchAll}}) or the next page of results (with \code{\link{fetchNext}}).
#' A corresponding URL to be used within a web browser is contained in \code{@webUIRequestUrl}
#' Please make sure to check \code{$collection$rewrites} to see if any unforeseen access rewrites of the query's virtual corpus had to be performed.
#'
#' @examples
#' # Fetch metadata of every query hit for "Ameisenplage" and show a summary
#' \donttest{
#' new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchAll()
#' }
#'
#' # Use the copy of a KorAP-web-frontend URL for an API query of "Ameise" in a virtual corpus
#' # and show the number of query hits (but don't fetch them).
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#'  corpusQuery(KorAPUrl =
#'    "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp")
#'
#' # Plot the time/frequency curve of "Ameisenplage"
#' \donttest{
#' new("KorAPConnection", verbose=TRUE) %>%
#'   { . ->> kco } %>%
#'   corpusQuery("Ameisenplage") %>%
#'   fetchAll() %>%
#'   slot("collectedMatches") %>%
#'   mutate(year = lubridate::year(pubDate)) %>%
#'   dplyr::select(year) %>%
#'   group_by(year) %>%
#'   summarise(Count = dplyr::n()) %>%
#'   mutate(Freq = mapply(function(f, y)
#'     f / corpusStats(kco, paste("pubDate in", y))@tokens, Count, year)) %>%
#'   dplyr::select(-Count) %>%
#'   complete(year = min(year):max(year), fill = list(Freq = 0)) %>%
#'   plot(type = "l")
#' }
#' @seealso \code{\link{KorAPConnection}}, \code{\link{fetchNext}}, \code{\link{fetchRest}}, \code{\link{fetchAll}}, \code{\link{corpusStats}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @aliases corpusQuery
#' @export
setMethod("corpusQuery", "KorAPConnection",
          function(kco,
                   query = if (missing(KorAPUrl))
                     stop("At least one of the parameters query and KorAPUrl must be specified.", call. = FALSE)
                   else
                     httr::parse_url(KorAPUrl)$query$q,
                   vc = if (missing(KorAPUrl)) "" else httr::parse_url(KorAPUrl)$query$cq,
                   KorAPUrl,
                   metadataOnly = TRUE,
                   ql = if (missing(KorAPUrl)) "poliqarp" else httr::parse_url(KorAPUrl)$query$ql,
                   fields = c(
                     "corpusSigle",
                     "textSigle",
                     "pubDate",
                     "pubPlace",
                     "availability",
                     "textClass",
                     "snippet"
                   ),
                   accessRewriteFatal = TRUE,
                   verbose = kco@verbose,
                   expand = length(vc) != length(query),
          as.df = FALSE) {
  if (length(query) > 1 || length(vc) > 1) {

    grid <- {
      if (expand)
        expand_grid(query=query, vc=vc) else tibble(query=query, vc=vc) }
        return(
             do.call(rbind,
                     Map(function(q, cq) corpusQuery(kco, query=q, vc=cq, ql=ql,
                                                     verbose=verbose, as.df = TRUE), grid$query, grid$vc)) %>%
               remove_rownames()
           )
    } else {
      contentFields <- c("snippet")
      if (metadataOnly) {
        fields <- fields[!fields %in% contentFields]
      }
      request <-
        paste0('?q=',
               URLencode(query, reserved = TRUE),
               if (vc != '') paste0('&cq=', URLencode(vc, reserved = TRUE)) else '', '&ql=', ql)
      webUIRequestUrl <- paste0(kco@KorAPUrl, request)
      requestUrl <- paste0(
        kco@apiUrl,
        'search',
        request,
        '&fields=',
        paste(fields, collapse = ","),
        if (metadataOnly) '&access-rewrite-disabled=true' else ''
      )
      log.info(verbose, "Searching \"", query, "\" in \"", vc, "\"", sep =
                 "")
      res = apiCall(kco, paste0(requestUrl, '&count=0'))
      if(!is.null(res$meta$cached))
        log.info(verbose, " [cached]\n")
      else
        log.info(verbose, " took ", res$meta$benchmark, "\n", sep = "")
      if (as.df)
        data.frame(
          query = query,
          totalResults = res$meta$totalResults,
          vc = vc,
          webUIRequestUrl = webUIRequestUrl,
          stringsAsFactors = FALSE
        )
      else
        KorAPQuery(
          korapConnection = kco,
          nextStartIndex = 0,
          fields = fields,
          requestUrl = requestUrl,
          request = request,
          totalResults = res$meta$totalResults,
          vc = vc,
          apiResponse = res,
          webUIRequestUrl = webUIRequestUrl,
          hasMoreMatches = (res$meta$totalResults > 0),
        )
    }
  })

#' Fetch the next bunch of results of a KorAP query.
#'
#' \bold{\code{fetchNext}} fetches the next bunch of results of a KorAP query.
#'
#' @param kqo object obtained from \code{\link{corpusQuery}}
#' @param offset start offset for query results to fetch
#' @param maxFetch maximum number of query results to fetch
#' @param verbose print progress information if true
#' @return The \code{kqo} input object with updated slots \code{collectedMatches}, \code{apiResponse}, \code{nextStartIndex}, \code{hasMoreMatches}
#'
#' @examples
#' \donttest{q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchNext()
#' q@collectedMatches
#' }
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @aliases fetchNext
#' @rdname KorAPQuery-class
#' @importFrom dplyr rowwise bind_rows select summarise n
#' @export
setMethod("fetchNext", "KorAPQuery", function(kqo, offset = kqo@nextStartIndex, maxFetch = maxResultsPerPage, verbose = kqo@korapConnection@verbose) {
  if (kqo@totalResults == 0 || offset >= kqo@totalResults) {
    return(kqo)
  }

  page <- kqo@nextStartIndex / maxResultsPerPage + 1
  results <- 0
  pubDate <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  collectedMatches <- kqo@collectedMatches

  repeat {
    query <- paste0(kqo@requestUrl, '&count=', min(if (!is.na(maxFetch)) maxFetch - results else maxResultsPerPage, maxResultsPerPage) ,'&offset=', offset + results, '&cutoff=true')
    res <- apiCall(kqo@korapConnection, query)
    if (length(res$matches) == 0) {
      break
    }

    for (field in kqo@fields) {
      if (!field %in% colnames(res$matches)) {
        res$matches[, field] <- NA
      }
    }
    currentMatches <-
      res$matches %>%
      dplyr::select(kqo@fields)
    if ("pubDate" %in% kqo@fields) {
      currentMatches$pubDate <-  currentMatches$pubDate %>% as.Date(format = "%Y-%m-%d")
      factorCols <- currentMatches %>% select(-pubDate) %>% colnames()
    } else {
      factorCols <- colnames(currentMatches)
    }
    currentMatches[factorCols] <- lapply(currentMatches[factorCols], factor)
    if (!is.list(collectedMatches)) {
      collectedMatches <- currentMatches
    } else {
      collectedMatches <- rbind(collectedMatches, currentMatches)
    }
    if (verbose) {
      cat(paste0("Retrieved page ", page, "/", ceiling((kqo@totalResults) / res$meta$itemsPerPage), ' in ', res$meta$benchmark, '\n'))
    }
    page <- page + 1
    results <- results + res$meta$itemsPerPage
    if (offset + results >= kqo@totalResults || (!is.na(maxFetch) && results >= maxFetch)) {
      break
    }
  }
  nextStartIndex <- min(res$meta$startIndex + res$meta$itemsPerPage, kqo@totalResults)
  KorAPQuery(nextStartIndex = nextStartIndex,
    korapConnection = kqo@korapConnection,
    fields = kqo@fields,
    requestUrl = kqo@requestUrl,
    request = kqo@request,
    totalResults = kqo@totalResults,
    vc = kqo@vc,
    webUIRequestUrl = kqo@webUIRequestUrl,
    hasMoreMatches = (kqo@totalResults > nextStartIndex),
    apiResponse = res,
    collectedMatches = collectedMatches)
})

#' Fetch all results of a KorAP query.
#'
#' \bold{\code{fetchAll}} fetches all results of a KorAP query.
#'
#' @examples
#' \donttest{
#' q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchAll()
#' q@collectedMatches
#' }
#'
#' @aliases fetchAll
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchAll", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose) {
  return(fetchNext(kqo, offset = 0, maxFetch = NA, verbose = verbose))
})

#' Fetches the remaining results of a KorAP query.
#'
#' @examples
#' \donttest{
#' q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchRest()
#' q@collectedMatches
#' }
#'
#' @aliases fetchRest
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchRest", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose) {
  return(fetchNext(kqo, maxFetch = NA, verbose = verbose))
})

#' Query relative frequency of search term(s)
#'
#' \bold{\code{frequencyQuery}} combines \code{\link{corpusQuery}}, \code{\link{corpusStats}} and
#' \code{\link{ci}} to compute a table with the relative frequencies and
#' confidence intervals of one ore multiple search terms across one or multiple
#' virtual corpora.
#'
#' @aliases frequencyQuery
#' @rdname KorAPQuery-class
#' @examples
#' \donttest{
#' new("KorAPConnection", verbose = TRUE) %>%
#'   frequencyQuery(c("Mücke", "Schnake"), paste0("pubDate in ", 2000:2003))
#' }
#'
#' @param kco \code{\link{KorAPConnection}} object (obtained e.g. from \code{new("KorAPConnection")}
#' @param query string that contains the corpus query. The query language depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}.
#' @param conf.level confidence level of the returned confidence interval (passed through \code{\link{ci}}  to \code{\link{prop.test}}).
#' @param as.alternatives LOGICAL that specifies if the query terms should be treated as alternatives. If \code{as.alternatives} is TRUE, the sum over all query hits, instead of the respective vc token sizes is used as total for the calculation of relative frequencies.
#' @export
setMethod("frequencyQuery", "KorAPConnection",
  function(kco, query, vc = "", conf.level = 0.95, as.alternatives = FALSE, ...) {
      (if (as.alternatives) {
        corpusQuery(kco, query, vc, metadataOnly = TRUE, as.df = TRUE, ...) %>%
        group_by(vc) %>%
        mutate(total = sum(totalResults))
      } else {
        corpusQuery(kco, query, vc, metadataOnly = TRUE, as.df = TRUE, ...) %>%
        mutate(total = corpusStats(kco, vc=vc, as.df=TRUE)$tokens)
      } ) %>%
      ci(conf.level = conf.level)
})

#´ format()
#' @rdname KorAPQuery-class
#' @param x KorAPQuery object
#' @param ... further arguments passed to or from other methods
#' @export
format.KorAPQuery <- function(x, ...) {
  cat("<KorAPQuery>\n")
  q <- x
  aurl = parse_url(q@request)
  cat("           Query: ", aurl$query$q, "\n")
  if (!is.null(aurl$query$cq) && aurl$query$cq != "") {
    cat("  Virtual corpus: ", aurl$query$cq, "\n")
  }
  if (!is.null(q@collectedMatches)) {
    cat("==============================================================================================================", "\n")
    print(summary(q@collectedMatches))
    cat("==============================================================================================================", "\n")
  }
  cat("   Total results: ", q@totalResults, "\n")
  cat(" Fetched results: ", q@nextStartIndex, "\n")
}

#' show()
#'
#' @rdname KorAPQuery-class
#' @param object KorAPQuery object
#' @export
setMethod("show", "KorAPQuery", function(object) {
  format(object)
})



lemmatizeWordQuery <- function(w) {
  paste0('[tt/l=', w, ']')
}

#' Query frequencies of a node and a collocate and calculate collocation association scores
#'
#' \bold{\code{collocationScoreQuery}} computes various collocation association scores
#' based on \code{\link{frequencyQuery}}s for a target word and a collocate.
#'
#' @aliases collocationScoreQuery
#' @rdname KorAPQuery-class
#'
#' @param kco \code{\link{KorAPConnection}} object (obtained e.g. from \code{new("KorAPConnection")}
#' @param node               target word
#' @param collocate          collocate of target word
#' @param vc                 string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param lemmatizeNodeQuery      logical, set to TRUE if node query should be lemmatized, i.e. x -> [tt/l=x]
#' @param lemmatizeCollocateQuery logical, set to TRUE if collocate query should be lemmatized, i.e. x -> [tt/l=x]
#' @param leftContextSize    size of the left context window
#' @param rightContextSize   size of the right context window
#' @param scoreFunctions     named list of score functions of the form function(O1, O2, O, N, E, window_size), see e.g. \link{pmi}
#' @param smoothingConstant  smoothing constant will be added to all observed values
#'
#' @return tibble with query KorAP web request URL, all observed values and association scores
#'
#' @examples
#' \donttest{
#' new("KorAPConnection", verbose = TRUE) %>%
#'   collocationScoreQuery("Grund", "triftiger")
#' }
#'
#' \donttest{
#' new("KorAPConnection", verbose = TRUE) %>%
#' collocationScoreQuery("Grund", c("guter", "triftiger"),
#'    scoreFunctions = list(localMI = function(O1, O2, O, N, E, window_size) { O * log2(O/E) }) )
#' }
#'
#' \donttest{
#' library(highcharter)
#' library(tidyr)
#' new("KorAPConnection", verbose = TRUE) %>%
#'   collocationScoreQuery("Team", "agil", vc = paste("pubDate in", c(2014:2018)),
#'                         lemmatizeNodeQuery = TRUE, lemmatizeCollocateQuery = TRUE) %>%
#'                          pivot_longer(14:last_col(), names_to = "measure", values_to = "score") %>%
#'   hchart(type="spline", hcaes(label, score, group=measure)) %>%
#'   hc_add_onclick_korap_search()
#' }
#'
#' @importFrom tidyr pivot_longer
#' @export
setMethod("collocationScoreQuery", "KorAPConnection",
          function(kco,
                   node,
                   collocate,
                   vc = "",
                   lemmatizeNodeQuery = FALSE,
                   lemmatizeCollocateQuery = FALSE,
                   leftContextSize = 5,
                   rightContextSize = 5,
                   scoreFunctions = defaultAssociationScoreFunctions(),
                   smoothingConstant = .5
                   ) {
            # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
            O1 <- O2 <- O <- N <- E <- w <- 0

            if (leftContextSize <= 0 && rightContextSize <= 0) {
              stop("At least one of leftContextSize and rightContextSize must be > 0",
                   call. = FALSE)
            }

            if (lemmatizeNodeQuery) {
              node <- lemmatizeWordQuery(node)
            }

            if (lemmatizeCollocateQuery) {
              collocate <- lemmatizeWordQuery(collocate)
            }

            query <- ""

            if (leftContextSize > 0) {
              query <-
                paste0(collocate,
                       if (leftContextSize > 1) paste0(" []{0,", leftContextSize - 1, "} ") else " ",
                       node,
                       if (rightContextSize > 0)  " | ")
            }

            if (rightContextSize > 0) {
              query <-
                paste0(query, node,
                       if (rightContextSize > 1) paste0(" []{0,", rightContextSize - 1, "} ") else " ", collocate)
            }


            tibble(
              node = node,
              collocate = collocate,
              label = queryStringToLabel(vc),
              vc = vc,
              webUIRequestUrl = frequencyQuery(kco, query, vc)$webUIRequestUrl,
              w = leftContextSize + rightContextSize,
              leftContextSize,
              rightContextSize,
              N  = frequencyQuery(kco, node, vc)$total + smoothingConstant,
              O = as.double(frequencyQuery(kco, query, vc)$totalResults) + smoothingConstant,
              O1 = frequencyQuery(kco, node, vc)$totalResults + smoothingConstant,
              O2 = frequencyQuery(kco, collocate, vc)$totalResults + smoothingConstant,
              E = w * as.double(O1) * O2 / N
            ) %>%
              mutate(!!! lapply(scoreFunctions, mapply, .$O1, .$O2, .$O, .$N, .$E, .$w))

          })
