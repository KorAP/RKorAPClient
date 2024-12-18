#' Class KorAPQuery
#'
#' This class provides methods to perform different kinds of queries on the KorAP API server.
#' `KorAPQuery` objects, which are typically created by the [corpusQuery()] method,
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
                                                                              "availability", "textClass", "snippet", "tokens"),
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

maxResultsPerPage <- 50

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Corpus query
#'
#' **`corpusQuery`** performs a corpus query via a connection to a KorAP-API-server
#'
#' @rdname KorAPQuery-class
#' @aliases corpusQuery
#'
#' @importFrom urltools url_encode
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows
#'
#' @param kco [KorAPConnection()] object (obtained e.g. from `new("KorAPConnection")`
#' @param query string that contains the corpus query. The query language depends on the `ql` parameter. Either `query` must be provided or `KorAPUrl`.
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in `KorAPConnection`) to provide all necessary information for the query.
#' @param metadataOnly logical that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE.
#'    If you want your corpus queries to return not only metadata, but also KWICS, you need to authorize
#'    your RKorAPClient application as explained in the
#'   [authorization section](https://github.com/KorAP/RKorAPClient#authorization)
#'   of the RKorAPClient Readme on GitHub and set the `metadataOnly` parameter to
#'   `FALSE`.
#' @param ql string to choose the query language (see [section on Query Parameters](https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters) in the Kustvakt-Wiki for possible values.
#' @param fields (meta)data fields that will be fetched for every match.
#' @param accessRewriteFatal abort if query or given vc had to be rewritten due to insufficient rights (not yet implemented).
#' @param verbose print some info
#' @param as.df return result as data frame instead of as S4 object?
#' @param expand logical that decides if `query` and `vc` parameters are expanded to all of their combinations
#' @param context string that specifies the size of the left and the right context returned in `snippet`
#'        (provided that `metadataOnly` is set to `false` and that the necessary access right are  met).
#'        The format of the context size specifcation (e.g. `3-token,3-token`) is described in the [Service: Search GET documentation of the Kustvakt Wiki](https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET).
#'        If the parameter is not set, the default context size secification of the KorAP server instance will be used.
#'        Note that you cannot overrule the maximum context size set in the KorAP server instance,
#'        as this is typically legally motivated.
#' @return Depending on the `as.df` parameter, a table or a [KorAPQuery()] object that, among other information, contains the total number of results in `@totalResults`. The resulting object can be used to fetch all query results (with [fetchAll()]) or the next page of results (with [fetchNext()]).
#' A corresponding URL to be used within a web browser is contained in `@webUIRequestUrl`
#' Please make sure to check `$collection$rewrites` to see if any unforeseen access rewrites of the query's virtual corpus had to be performed.
#'
#' @examples
#' \dontrun{
#'
#' # Fetch metadata of every query hit for "Ameisenplage" and show a summary
#' new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchAll()
#' }
#'
#' \dontrun{
#'
#' # Use the copy of a KorAP-web-frontend URL for an API query of "Ameise" in a virtual corpus
#' # and show the number of query hits (but don't fetch them).
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#'  corpusQuery(KorAPUrl =
#'    "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp")
#' }
#'
#' \dontrun{
#'
#' # Plot the time/frequency curve of "Ameisenplage"
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
#' @seealso [KorAPConnection()], [fetchNext()], [fetchRest()], [fetchAll()], [corpusStats()]
#'
#' @references
#' <https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026>
#'
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
                     "snippet",
                     "tokens"
                   ),
                   accessRewriteFatal = TRUE,
                   verbose = kco@verbose,
                   expand = length(vc) != length(query),
          as.df = FALSE,
          context = NULL) {
  if (length(query) > 1 || length(vc) > 1) {
    grid <- if (expand) expand_grid(query=query, vc=vc) else tibble(query=query, vc=vc)
    purrr::pmap(grid, function(query, vc, ...)
      corpusQuery(kco, query=query, vc=vc, ql=ql, verbose=verbose, as.df = TRUE)) %>%
      bind_rows()
  } else {
      contentFields <- c("snippet", "tokens")
      if (metadataOnly) {
        fields <- fields[!fields %in% contentFields]
      }
      request <-
        paste0('?q=',
               url_encode(enc2utf8(query)),
               ifelse (!metadataOnly && ! is.null(context) && context !=  '', paste0('&context=', url_encode(enc2utf8(context))), ''),
               ifelse (vc != '', paste0('&cq=', url_encode(enc2utf8(vc))), ''),
               ifelse (!metadataOnly, '&show-tokens=true', ''),
               '&ql=', ql)
      webUIRequestUrl <- paste0(kco@KorAPUrl, request)
      requestUrl <- paste0(
        kco@apiUrl,
        'search',
        request,
        '&fields=',
        paste(fields, collapse = ","),
        if (metadataOnly) '&access-rewrite-disabled=true' else ''
      )
      log_info(verbose, "Searching \"", query, "\" in \"", vc, "\"", sep =
                 "")
      res = apiCall(kco, paste0(requestUrl, '&count=0'))
      if (is.null(res)) {
        log_info(verbose, " [failed]\n")
        message("API call failed.")
        totalResults <- 0
      } else {
        totalResults <-as.integer(res$meta$totalResults)
        log_info(verbose, ": ", totalResults, " hits")
        if(!is.null(res$meta$cached))
          log_info(verbose, " [cached]\n")
        else
          log_info(verbose, ", took ", res$meta$benchmark, "\n", sep = "")
      }
      if (as.df)
        data.frame(
          query = query,
          totalResults = totalResults,
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
          totalResults = totalResults,
          vc = vc,
          apiResponse = res,
          webUIRequestUrl = webUIRequestUrl,
          hasMoreMatches = (totalResults > 0),
        )
    }
  })

#' @importFrom purrr map
repair_data_strcuture <- function(x) {
  if (is.list(x))
    as.character (purrr::map(x, ~ if (length(.x) > 1) {
      paste(.x, collapse = " ")
    } else {
      .x
    }))
  else
    ifelse(is.na(x), "", x)
}

#' Fetch the next bunch of results of a KorAP query.
#'
#' **`fetchNext`** fetches the next bunch of results of a KorAP query.
#'
#' @param kqo object obtained from [corpusQuery()]
#' @param offset start offset for query results to fetch
#' @param maxFetch maximum number of query results to fetch
#' @param verbose print progress information if true
#' @param randomizePageOrder fetch result pages in pseudo random order if true. Use [set.seed()] to set seed for reproducible results.
#' @return The `kqo` input object with updated slots `collectedMatches`, `apiResponse`, `nextStartIndex`, `hasMoreMatches`
#'
#' @examples
#' \dontrun{
#'
#' q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchNext()
#' q@collectedMatches
#' }
#'
#' @references
#' <https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026>
#'
#' @aliases fetchNext
#' @rdname KorAPQuery-class
#' @importFrom dplyr rowwise mutate bind_rows select summarise n select
#' @importFrom tibble enframe add_column
#' @importFrom stringr word
#' @importFrom tidyr unnest unchop pivot_wider
#' @importFrom purrr map
#' @export
setMethod("fetchNext", "KorAPQuery", function(kqo,
                                              offset = kqo@nextStartIndex,
                                              maxFetch = maxResultsPerPage,
                                              verbose = kqo@korapConnection@verbose,
                                              randomizePageOrder = FALSE) {
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  results <- key <- name <- pubDate <- tmp_positions <- 0

  if (kqo@totalResults == 0 || offset >= kqo@totalResults) {
    return(kqo)
  }
  use_korap_api <- Sys.getenv("USE_KORAP_API", unset = NA)
  page <- kqo@nextStartIndex / maxResultsPerPage + 1
  collectedMatches <- kqo@collectedMatches

  if (randomizePageOrder) {
    pages <- head(sample.int(ceiling(kqo@totalResults / maxResultsPerPage)), maxFetch) - 1
  }

  if(is.null(collectedMatches)) {
    collectedMatches <- data.frame()
  }
  repeat {
    page = nrow(collectedMatches) %/% maxResultsPerPage + 1
    currentOffset = ifelse(randomizePageOrder, pages[page],  page - 1) * maxResultsPerPage
    query <- paste0(kqo@requestUrl, '&count=', min(if (!is.na(maxFetch)) maxFetch - results else maxResultsPerPage, maxResultsPerPage) ,'&offset=', currentOffset, '&cutoff=true')
    res <- apiCall(kqo@korapConnection, query)
    if (length(res$matches) == 0) {
      break
    }

    if ("fields" %in% colnames(res$matches) && (is.na(use_korap_api) || as.numeric(use_korap_api) >= 1.0)) {
      if (verbose) cat("Using fields API: ")
      currentMatches <- res$matches$fields %>%
        purrr::map(~ mutate(.x, value = repair_data_strcuture(value))) %>%
        tibble::enframe() %>%
        tidyr::unnest(cols = value) %>%
        tidyr::pivot_wider(names_from = key, id_cols = name, names_repair = "unique") %>%
        dplyr::select(-name)
      if("snippet" %in% colnames(res$matches)) {
        currentMatches$snippet <- res$matches$snippet
      }
    } else {
      currentMatches <- res$matches
    }

    for (field in kqo@fields) {
      if (!field %in% colnames(currentMatches)) {
        currentMatches[, field] <- NA
      }
    }
    currentMatches <- currentMatches %>%
      select(kqo@fields) %>%
      mutate(
        tmp_positions = gsub(".*-p(\\d+)-(\\d+)", "\\1 \\2", res$matches$matchID),
        matchStart = as.integer(stringr::word(tmp_positions, 1)),
        matchEnd = as.integer(stringr::word(tmp_positions, 2)) - 1
      ) %>%
      select(-tmp_positions)

    if (!is.list(collectedMatches)) {
      collectedMatches <- currentMatches
    } else {
      collectedMatches <- bind_rows(collectedMatches, currentMatches)
    }
    if (verbose) {
      cat(paste0(
        "Retrieved page ",
        ceiling(nrow(collectedMatches) / res$meta$itemsPerPage),
        "/",
        if (!is.na(maxFetch) && maxFetch < kqo@totalResults)
          sprintf("%d (%d)", ceiling(maxFetch / res$meta$itemsPerPage), ceiling(kqo@totalResults / res$meta$itemsPerPage))
        else
          sprintf("%d", ceiling(kqo@totalResults / res$meta$itemsPerPage)),
        ' in ',
        res$meta$benchmark,
        '\n'
      ))
    }
    page <- page + 1
    results <- results + res$meta$itemsPerPage
    if (nrow(collectedMatches) >= kqo@totalResults || (!is.na(maxFetch) && results >= maxFetch)) {
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
#' **`fetchAll`** fetches all results of a KorAP query.
#'
#' @examples
#' \dontrun{
#'
#' q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchAll()
#' q@collectedMatches
#' }
#'
#' @aliases fetchAll
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchAll", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose, ...) {
  return(fetchNext(kqo, offset = 0, maxFetch = NA, verbose = verbose, ...))
})

#' Fetches the remaining results of a KorAP query.
#'
#' @examples
#' \dontrun{
#'
#' q <- new("KorAPConnection") %>% corpusQuery("Ameisenplage") %>% fetchRest()
#' q@collectedMatches
#' }
#'
#' @aliases fetchRest
#' @rdname KorAPQuery-class
#' @export
setMethod("fetchRest", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose, ...) {
  return(fetchNext(kqo, maxFetch = NA, verbose = verbose, ...))
})

#' Query relative frequency of search term(s)
#'
#' **`frequencyQuery`** combines [corpusQuery()], [corpusStats()] and
#' [ci()] to compute a table with the relative frequencies and
#' confidence intervals of one ore multiple search terms across one or multiple
#' virtual corpora.
#'
#' @aliases frequencyQuery
#' @rdname KorAPQuery-class
#' @examples
#' \dontrun{
#'
#' new("KorAPConnection", verbose = TRUE) %>%
#'   frequencyQuery(c("Mücke", "Schnake"), paste0("pubDate in ", 2000:2003))
#' }
#'
#' @param kco [KorAPConnection()] object (obtained e.g. from `new("KorAPConnection")`
#' @param query string that contains the corpus query. The query language depends on the `ql` parameter. Either `query` must be provided or `KorAPUrl`.
#' @param conf.level confidence level of the returned confidence interval (passed through [ci()]  to [prop.test()]).
#' @param as.alternatives LOGICAL that specifies if the query terms should be treated as alternatives. If `as.alternatives` is TRUE, the sum over all query hits, instead of the respective vc token sizes is used as total for the calculation of relative frequencies.
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

#' buildWebUIRequestUrlFromString
#'
#' @rdname KorAPQuery-class
#' @importFrom urltools url_encode
#' @export
buildWebUIRequestUrlFromString <- function(KorAPUrl,
                                 query,
                                 vc = "",
                                 ql = "poliqarp"
) {
  if ("KorAPConnection" %in% class(KorAPUrl)) {
    KorAPUrl <- KorAPUrl@KorAPUrl
  }

  request <-
    paste0(
      '?q=',
      urltools::url_encode(enc2utf8(as.character(query))),
      ifelse(vc != '',
             paste0('&cq=', urltools::url_encode(enc2utf8(vc))),
             ''),
      '&ql=',
      ql
    )
  paste0(KorAPUrl, request)
}

#' buildWebUIRequestUrl
#'
#' @rdname KorAPQuery-class
#' @importFrom httr parse_url
#' @export
buildWebUIRequestUrl <- function(kco,
                                 query = if (missing(KorAPUrl))
                                   stop("At least one of the parameters query and KorAPUrl must be specified.", call. = FALSE)
                                 else
                                   httr::parse_url(KorAPUrl)$query$q,
                                 vc = if (missing(KorAPUrl)) "" else httr::parse_url(KorAPUrl)$query$cq,
                                 KorAPUrl,
                                 ql = if (missing(KorAPUrl)) "poliqarp" else httr::parse_url(KorAPUrl)$query$ql) {

  buildWebUIRequestUrlFromString(kco@KorAPUrl, query, vc, ql)
}

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

