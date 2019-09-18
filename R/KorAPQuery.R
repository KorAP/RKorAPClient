#' Class KorAPQuery
#'
#' \code{KorAPQuery} objetcs represent the current state of a query to a KorAP server.
#' New \code{KorAPQuery} objects are typically created by the \code{\link{corpusQuery}} method.
#'
#' @include KorAPConnection.R
#' @import jsonlite
#' @import httr
#'
#'

#' @export
KorAPQuery <- setClass("KorAPQuery", slots = c(
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
#' @param request query part of the request URL
#' @param vc definition of a virtual corpus
#' @param totalResults number of hits the query has yielded
#' @param nextStartIndex at what index to start the next fetch of query results
#' @param fields what data / metadata fields should be collected
#' @param requestUrl complete URL of the API request
#' @param webUIRequestUrl URL of a web frontend request corresponding to the API request
#' @param apiResponse data-frame representation of the JSON response of the API request
#' @param hasMoreMatches boolean that signals if more query results can be fetched
#' @param collectedMatches matches already fetched from the KorAP-API-server
#' @export
setMethod("initialize", "KorAPQuery",
          function(.Object, request = NULL, vc="", totalResults=0, nextStartIndex=0, fields=c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
                                                                              "availability", "textClass", "snippet"),
                   requestUrl="", webUIRequestUrl = "", apiResponse = NULL, hasMoreMatches= FALSE, collectedMatches = NULL) {
            .Object <- callNextMethod()
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

maxResultsPerPage <- 50

QueryParameterFromUrl <- function(url, parameter) {
  regex <- paste0(".*[?&]", parameter, "=([^&]*).*")
  if (grepl(regex, url)) {
    return(gsub(regex, '\\1', url, perl = TRUE))
  } else {
    return("")
  }
}

KorAPQueryStringFromUrl <- function(KorAPUrl) {
  return(URLdecode(gsub(".*[?&]q=([^&]*).*", '\\1', KorAPUrl, perl = TRUE)))
}

#' Method corpusQuery
#'
#' Perform a corpus query via a connection to a KorAP-API-server.
#'
#' @param kco \code{\link{KorAPConnection}} object (obtained e.g. from \code{new("KorAPConnection")}
#' @param query string that contains the corpus query. The query langauge depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @param metadataOnly boolean that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE, unless the connection is authorized (currently not possible).
#' @param ql string to choose the query language (see \href{https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters}{section on Query Parameters} in the Kustvakt-Wiki for possible values.
#' @param fields (meta)data fields that will be fetched for every match
#' @param accessRewriteFatal abort if query or given vc had to be rewritten due to insufficent rights (not yet implemented)
#' @param verbose print some info
#' @return A \code{\link{KorAPQuery}} object that, among other information, contains the total number of results in \code{@totalResults}. The resulting object can be used to fetch all query results (with \code{\link{fetchAll}}) or the next page of results (with \code{\link{fetchNext}}).
#' A corresponding URL to be used within a web browser is contained in \code{@webUIRequestUrl}
#' Please make sure to check \code{$collection$rewrites} to see if any unforseen access rewrites of the query's virtual corpus had to be performed.
#'
#' @examples
#' # Fetch metadata of every query hit for "Ameisenplage" and show a summary
#' kco <- new("KorAPConnection")
#' kqo <- corpusQuery(kco, "Ameisenplage")
#' kqo <- fetchAll(kqo)
#' kqo
#'
#' # Use the copy of a KorAP-web-frontend URL for an API query of "Ameise" in a virtual corpus
#' # and show the number of query hits (but don't fetch them).
#' kco <- new("KorAPConnection")
#' kqo <- corpusQuery(kco,
#'        KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp")
#' kqo
#'
#' # Plot the time/frequency curve of "Ameisenplage"
#' kco <- new("KorAPConnection")
#' q <- corpusQuery(kco, "Ameisenplage")
#' q <- fetchAll(q, verbose=TRUE)
#' tokensPerYear <- function(year) { return(corpusStats(kco, paste("pubDate in", year))@tokens) }
#' df <- as.data.frame(table(as.numeric(format(q@collectedMatches$pubDate,"%Y")), dnn="year"),
#'                     stringsAsFactors = FALSE)
#' df$ipm <- 1000000 * df$Freq / tokensPerYear(df$year)
#' plot(df$year, df$ipm, type="l")
#'
#' @seealso \code{\link{KorAPConnection}}, \code{\link{fetchNext}}, \code{\link{fetchRest}}, \code{\link{fetchAll}}, \code{\link{corpusStats}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @aliases corpusQuery
#' @export
setMethod("corpusQuery", "KorAPConnection",
          function(kco, query, vc="", KorAPUrl, metadataOnly = TRUE, ql = "poliqarp", fields = defaultFields,
                   accessRewriteFatal = TRUE, verbose=FALSE) {
            defaultFields <- c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
                               "availability", "textClass", "snippet")
            contentFields <- c("snippet")

            if (missing(query) && missing(KorAPUrl) ||  ! (missing(query) || missing(KorAPUrl))) {
              stop("Exactly one of the parameters query and KorAPUrl must be specified.")
            }
            if (missing(query)) {
              query <- QueryParameterFromUrl(KorAPUrl, "q")
              vc <- QueryParameterFromUrl(KorAPUrl, "vc")
              ql <- QueryParameterFromUrl(KorAPUrl, "ql")
            }
            request <- paste0('?q=', URLencode(query, reserved=TRUE),
                              ifelse(vc != '', paste0('&cq=', URLencode(vc, reserved=TRUE)), ''), '&ql=', ql)
            webUIRequestUrl <- paste0(kco@KorAPUrl, request)
            requestUrl <- paste0(kco@apiUrl, 'search', request,
                                 '&fields=', paste(defaultFields, collapse = ","),
                                 ifelse(metadataOnly, '&access-rewrite-disabled=true', ''))
            if (verbose) {
              cat("Searching \"", query, "\" in \"", vc, "\"", sep="")
            }
            res = fromJSON(paste0(requestUrl, '&count=1'))
            if (verbose) {
              cat(" took ", res$meta$benchmark, "\n", sep="")
            }
            KorAPQuery(
              nextStartIndex = 0,
              fields = fields[!fields %in% contentFields],
              requestUrl = requestUrl,
              request = request,
              totalResults = res$meta$totalResults,
              vc = vc,
              apiResponse = res,
              webUIRequestUrl = webUIRequestUrl,
              hasMoreMatches = (res$meta$totalResults > 0),
            )
          })

#' Fetch the next bunch of results of a KorAP query.
#'
#' @param kqo object obtained from \code{\link{corpusQuery}}
#' @param offset start offset for query results to fetch
#' @param maxFetch maximum number of query results to fetch
#' @param verbose print progress information if true
#' @return The \code{kqo} input object with updated slots \code{collectedMatches}, \code{apiResponse}, \code{nextStartIndex}, \code{hasMoreMatches}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @aliases fetchNext
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchNext", "KorAPQuery", function(kqo, offset = kqo@nextStartIndex, maxFetch = maxResultsPerPage, verbose = FALSE) {
  if (kqo@totalResults == 0 || offset >= kqo@totalResults) {
    return(kqo)
  }

  page <- 1
  results <- 0
  pubDate <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  collectedMatches <- kqo@collectedMatches

  repeat {
    res <- fromJSON(paste0(kqo@requestUrl, '&count=', min(ifelse(!is.na(maxFetch), maxFetch - results, maxResultsPerPage), maxResultsPerPage) ,'&offset=', offset + results))
    if (res$meta$totalResults == 0) { return(kqo) }
    for (field in kqo@fields) {
      if (!field %in% colnames(res$matches)) {
        res$matches[, field] <- NA
      }
    }
    currentMatches <- res$matches[kqo@fields]
    factorCols <- colnames(subset(currentMatches, select=-c(pubDate)))
    currentMatches[factorCols] <- lapply(currentMatches[factorCols], factor)
    currentMatches$pubDate = as.Date(currentMatches$pubDate, format = "%Y-%m-%d")
    if (!is.list(collectedMatches)) {
      collectedMatches <- currentMatches
    } else {
      collectedMatches <- rbind(collectedMatches, currentMatches)
    }
    if (verbose) {
      cat(paste0("Retrieved page: ", page, "/", ceiling((res$meta$totalResults) / res$meta$itemsPerPage), ': ', res$meta$benchmark, '\n'))
    }
    page <- page + 1
    results <- results + res$meta$itemsPerPage
    if (offset + results >= res$meta$totalResults || (!is.na(maxFetch) && results >= maxFetch)) {
      break
    }
  }
  nextStartIndex <- min(res$meta$startIndex + res$meta$itemsPerPage, res$meta$totalResults)
  KorAPQuery(nextStartIndex = nextStartIndex,
    fields = kqo@fields,
    requestUrl = kqo@requestUrl,
    request = kqo@request,
    totalResults = res$meta$totalResults,
    vc = kqo@vc,
    webUIRequestUrl = kqo@webUIRequestUrl,
    hasMoreMatches = (res$meta$totalResults > nextStartIndex),
    apiResponse = res,
    collectedMatches = collectedMatches)
})

#' Fetch all results of a KorAP query.
#'
#' @examples
#' q <- fetchAll(corpusQuery(new("KorAPConnection"), "Ameisenplage"))
#' q@collectedMatches
#'
#' @aliases fetchAll
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchAll", "KorAPQuery", function(kqo, verbose = FALSE) {
  return(fetchNext(kqo, offset = 0, maxFetch = NA, verbose = verbose))
})

#' Fetches the remaining results of a KorAP query.
#'
#' @examples
#' q <- fetchRest(fetchNext(corpusQuery(new("KorAPConnection"), "Ameisenplage")))
#' q@collectedMatches
#'
#' @aliases fetchRest
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchRest", "KorAPQuery", function(kqo, verbose = FALSE) {
  return(fetchNext(kqo, maxFetch = NA, verbose = verbose))
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
  cat("         Query: ", aurl$query$q, "\n")
  if (!is.null(aurl$query$vc) && aurl$query$vc != "") {
    cat("Virtual corpus: ", aurl$query$vc, "\n")
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
