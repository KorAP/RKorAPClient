#' @import jsonlite
#' @import curl

defaultFields <- c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
            "availability", "textClass", "snippet")

contentFields <- c("snippet")

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

#' \code{KorAPQuery} perform a query on the KorAP server.
#' @param con object obtained from \code{\link{KorAPConnection}}, that contains all necessary connection information
#' @param query string that contains the corpus query. The query langauge depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @param metaDataOnly boolean that determines wether queries should return only metadata without any snippets. This can also be useful to prevent query rewrites.
#' @param ql string to choose the query language
#' @param fields (meta)data fields that will be fetch for every matcch
#'
#' @return A KorAP query object that, among other information, contains the total number of results in \code{$meta$totalResults}. The resulting object can be used to fetch all (\code{\link{KorAPFetchAll}) or the next page of results (\code{\link{KorAPFetchNext}}). Please make sure to check \code{$collection$rewrites} to see if any unforseen rewrites of the query had to be performed.
#'
#' @examples
#' q <- KorAPQuery(con, "Ameisenplage")
#' q <- KorAPQuery(KorAPConnection(), "Ameisenplage")
#' q <- KorAPQuery(con, KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp&cutoff=1")
#'
#' @references
#' \url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
#'
#' @export
KorAPQuery <- function(con, query = NA, vc=NA, KorAPUrl = NA, metadataOnly=FALSE, ql="poliqarp", fields=defaultFields) {
  if (is.na(query) && is.na(KorAPUrl) ||  ! (is.na(query) || is.na(KorAPUrl))) {
    stop("Exaclty one of the parameters query and KorAPUrl must be specified.")
  }
  if (is.na(query)) {
    query <- QueryParameterFromUrl(KorAPUrl, "q")
    vc <- QueryParameterFromUrl(KorAPUrl, "vc")
    ql <- QueryParameterFromUrl(KorAPUrl, "ql")
  }
  request <- paste0('?q=', URLencode(query, reserved=TRUE),
                    ifelse(vc != '', paste0('&vc=', URLencode(vc, reserved=TRUE)), ''),
                    '&ql=', ql);
  webUIRequestUrl <- paste0(con$KorAPUrl, request)
  if (is.na(vc)) {
    vc <-""
  }
  requestUrl <- paste0(con$apiUrl, 'search', request,
                       '&fields=', paste(defaultFields, collapse = ","),
                       ifelse(metadataOnly, '&access-rewrite-disabled=true', ''))
  result <- fromJSON(paste0(requestUrl, '&count=1'))

  result$fields <- fields[!metadataOnly || !fields %in% contentFields]
  result$requestUrl <- requestUrl
  result$request <- request
  result$vc <- vc
  result$webUIRequestUrl <- webUIRequestUrl
  result$nextStartIndex <- 0
  result$hasMoreMatches <- (result$meta$totalResults > 0)
  return(result)
}

#' @export
KorAPFetchAll <- function(query, verbose=FALSE) {
  if (query$meta$totalResults == 0) { return(data.frame()) }

  page <- 1
  results <- 0

  repeat {
    res <- fromJSON(paste0(query$requestUrl, '&count=50&offset=', results))
    if (res$meta$totalResults == 0) { return(data.frame()) }
    for (field in query$fields) {
      if (!field %in% colnames(res$matches)) {
        res$matches[, field] <- NA
      }
    }
    currentMatches <- res$matches[query$fields]
    factorCols <- colnames(subset(currentMatches, select=-c(pubDate)))
    currentMatches[factorCols] <- lapply(currentMatches[factorCols], factor)
    currentMatches$pubDate = as.Date(currentMatches$pubDate, format = "%Y-%m-%d")
    if (results == 0) {
      allMatches <- currentMatches
      expectedResults <- res$meta$totalResults
    } else {
      allMatches <- rbind(allMatches, currentMatches)
    }
    if (verbose) {
      cat(paste0("Retrieved page: ", page, "/", ceiling(expectedResults / res$meta$itemsPerPage), ': ', res$meta$benchmark, '\n'))
    }
    page <- page + 1
    results <- results + res$meta$itemsPerPage
    if (results >= expectedResults) {
      break
    }
  }
  return(allMatches)
}

#' @export
KorAPFetchNext <- function(query, offset=query$nextStartIndex, verbose=FALSE) {
  if (query$nextStartIndex >= query$meta$totalResults) {
    query$hasMoreMatches <- FALSE
    return(query)
  }

  res <- fromJSON(paste0(query$requestUrl, '&count=50&offset=', offset))
  for (field in query$fields) {
    if (!field %in% colnames(res$matches)) {
      res$matches[, field] <- NA
    }
  }
  currentMatches <- res$matches[query$fields]
  factorCols <- colnames(subset(currentMatches, select=-c(pubDate)))
  currentMatches[factorCols] <- lapply(currentMatches[factorCols], factor)
  currentMatches$pubDate = as.Date(currentMatches$pubDate, format = "%Y-%m-%d")
  if (offset == 0) {
    res$collectedMatches <- currentMatches
  } else {
    res$collectedMatches <- rbind(query$collectedMatches, currentMatches)
  }
  if (verbose) {
    cat(paste0("Retrieved page in ", res$meta$benchmark, '\n'))
  }
  res$nextStartIndex <- res$meta$startIndex + res$meta$itemsPerPage
  res$fields <- query$fields
  res$requestUrl <- query$requestUrl
  res$request <- query$request
  res$webUIRequestUrl <- query$webUIRequestUrl
  res$hasMoreMatches <- (res$meta$totalResults > res$nextStartIndex)

  return(res)
}
