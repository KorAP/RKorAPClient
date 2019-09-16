#' @import jsonlite
#' @import curl
#' @import utils

defaultFields <- c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
            "availability", "textClass", "snippet")

contentFields <- c("snippet")

maxResultsPerPage <- 50;

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

#' Send a query to a KorAP connection.
#' @param con object obtained from \code{\link{KorAPConnection}}, that contains all necessary connection information
#' @param query string that contains the corpus query. The query langauge depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @param metadataOnly boolean that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE, unless the connection is authorized (currently not possible).
#' @param ql string to choose the query language (see \href{https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters}{section on Query Parameters} in the Kustvakt-Wiki for possible values.
#' @param fields (meta)data fields that will be fetched for every match
#' @param accessRewriteFatal abort if query or given vc had to be rewritten due to insufficent rights (not yet implemented)
#' @param verbose print some info
#' @return A KorAP query object that, among other information, contains the total number of results in \code{$meta$totalResults}. The resulting object can be used to fetch all query results (with \code{\link{KorAPFetchAll}}) or the next page of results (with \code{\link{KorAPFetchNext}}).
#' A correspunding URL to be used within a web browser is contained in \code{$webUIRequestUrl}
#' Please make sure to check \code{$collection$rewrites} to see if any unforseen access rewrites of the query's virtual corpus had to be performed.
#'
#' @examples
#' con <- KorAPConnection()
#' q <- KorAPQuery(con, "Ameisenplage")
#' q <- KorAPFetchAll(q)
#' summary(q$collectedMatches)
#'
#' q <- KorAPQuery(con,
#'        KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp")
#' q$meta$totalResults
#'
#' q <- KorAPQuery(con, "Ameisenplage")
#' q <- KorAPFetchAll(q, verbose=TRUE)
#' tokensPerYear <- function(year) { return(KorAPCorpusStats(con, paste("pubDate in", year))$tokens) }
#' df <- as.data.frame(table(as.numeric(format(q$collectedMatches$pubDate,"%Y")), dnn="year"),
#'                     stringsAsFactors = FALSE)
#' df$ipm <- 1000000 * df$Freq / tokensPerYear(df$year)
#' plot(df$year, df$ipm, type="l")
#'
#' @seealso \code{\link{KorAPConnection}}, \code{\link{KorAPFetchNext}}, \code{\link{KorAPFetchRest}}, \code{\link{KorAPFetchAll}}, \code{\link{KorAPCorpusStats}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @export
KorAPQuery <- function(con, query, vc="", KorAPUrl, metadataOnly = TRUE, ql = "poliqarp", fields = defaultFields,
                       accessRewriteFatal = TRUE, verbose=FALSE) {
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
  webUIRequestUrl <- paste0(con$KorAPUrl, request)
  requestUrl <- paste0(con$apiUrl, 'search', request,
                       '&fields=', paste(defaultFields, collapse = ","),
                       ifelse(metadataOnly, '&access-rewrite-disabled=true', ''))
  if (verbose) {
    cat(paste0(webUIRequestUrl, "\n"))
  }
  result <- fromJSON(paste0(requestUrl, '&count=1'))
  result$fields <- fields[!fields %in% contentFields]
  result$requestUrl <- requestUrl
  result$request <- request
  result$vc <- vc
  result$webUIRequestUrl <- webUIRequestUrl
  result$nextStartIndex <- 0
  result$hasMoreMatches <- (result$meta$totalResults > 0)
  return(result)
}

#' Fetch the next bunch of results of a KorAP query.
#' @param queryObject object obtained from \code{\link{KorAPQuery}}
#' @param offset start offset for query results to fetch
#' @param maxFetch maximum number of query results to fetch
#' @param verbose print progress information if true
#' @return The \code{queryObject} input parameter with updated fields \code{$collectedMatches}, \code{$matches} (latest bunch only), \code{$nextStartIndex}, , \code{$hasMoreMatches}
#'
#' @examples
#' q <- KorAPFetchNext(KorAPQuery(KorAPConnection(), "Ameisenplage"))
#'
#' @seealso \code{\link{KorAPFetchRest}}, \code{\link{KorAPFetchAll}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @export
KorAPFetchNext <- function(queryObject, offset = queryObject$nextStartIndex, maxFetch = maxResultsPerPage, verbose = FALSE) {
  if (queryObject$meta$totalResults == 0 || offset >= queryObject$meta$totalResults) {
    return(queryObject)
  }

  page <- 1
  results <- 0
  pubDate <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  collectedMatches <- queryObject$collectedMatches

  repeat {
    res <- fromJSON(paste0(queryObject$requestUrl, '&count=', min(ifelse(!is.na(maxFetch), maxFetch - results, maxResultsPerPage), maxResultsPerPage) ,'&offset=', offset + results))
    if (res$meta$totalResults == 0) { return(data.frame()) }
    for (field in queryObject$fields) {
      if (!field %in% colnames(res$matches)) {
        res$matches[, field] <- NA
      }
    }
    currentMatches <- res$matches[queryObject$fields]
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
  res$nextStartIndex <- res$meta$startIndex + res$meta$itemsPerPage
  res$fields <- queryObject$fields
  res$requestUrl <- queryObject$requestUrl
  res$request <- queryObject$request
  res$webUIRequestUrl <- queryObject$webUIRequestUrl
  res$hasMoreMatches <- (res$meta$totalResults > res$nextStartIndex)
  res$collectedMatches <- collectedMatches
  return(res)
}

#' Fetch all results of a KorAP query.
#' @param queryObject object obtained from \code{\link{KorAPQuery}}
#' @param verbose print progress information if true
#' @return The \code{queryObject} input parameter with updated fields \code{$collectedMatches}, \code{$matches} (latest bunch only), \code{$nextStartIndex}, \code{$hasMoreMatches}
#'
#' @examples
#' q <- KorAPFetchAll(KorAPQuery(KorAPConnection(), "Ameisenplage"))
#' q$collectedMatches
#'
#' @seealso \code{\link{KorAPFetchRest}}, \code{\link{KorAPFetchNext}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @export
KorAPFetchAll <- function(queryObject, verbose = FALSE) {
  return(KorAPFetchNext(queryObject, offset = 0, maxFetch = NA, verbose = verbose))
}

#' Fetches all remaining results of a KorAP query.
#' @param queryObject object obtained from \code{\link{KorAPQuery}}
#' @param verbose print progress information if true
#' @return The \code{queryObject} input parameter with updated fields \code{$collectedMatches}, \code{$matches} (latest bunch only), \code{$nextStartIndex}, \code{$hasMoreMatches}
#'
#' @examples
#' q <- KorAPFetchRest(KorAPFetchNext(KorAPQuery(KorAPConnection(), "Ameisenplage")))
#' q$collectedMatches
#'
#' @seealso \code{\link{KorAPFetchAll}}, \code{\link{KorAPFetchNext}}
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @export
KorAPFetchRest <- function(queryObject, verbose = FALSE) {
  return(KorAPFetchNext(queryObject, maxFetch = NA, verbose = verbose))
}
