#' @import jsonlite
#' @import curl

library(jsonlite)

defaultFields <- c("corpusSigle", "textSigle", "pubDate",  "pubPlace",
            "availability", "textClass")

derekoStats <- function(vc='') {
  return(fromJSON(paste0(apiurl, 'statistics?cq=',
                         URLencode(vc, reserved=TRUE))))
}

KorAPQuery <- function(con, query, vc="", ql="poliqarp", fields=defaultFields) {
  request <- paste0('?q=', URLencode(query, reserved=TRUE),
                    ifelse(vc != '', paste0('&cq=', URLencode(vc, reserved=TRUE)), ''),
                    '&ql=', ql);
  webUIRequestUrl <- paste0(con$KorAPUrl, request)
  requestUrl <- paste0(con$apiUrl, 'search', request,
                       '&fields=', paste(defaultFields, collapse = ","),
                       '&access-rewrite-disabled=true')
  result <- fromJSON(paste0(requestUrl, '&count=1'))

  result$fields <- fields
  result$requestUrl <- requestUrl
  result$request <- request
  result$webUIRequestUrl <- webUIRequestUrl
  result$nextStartIndex <- 0
  result$hasMoreMatches <- (result$meta$totalResults > 0)
  return(result)
}

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
