################################################################################
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
################################################################################
setClassUnion("characterOrNULL", c("character", "NULL"))

#' Class KorAPConnection
#'
#' `KorAPConnection` objects represent the connection to a KorAP server.
#' New `KorAPConnection` objects can be created by `new("KorAPConnection")`.
#'
#' @import R.cache
#' @import utils
#' @import methods
#' @export
KorAPConnection <- setClass("KorAPConnection", slots=c(KorAPUrl="character", apiVersion="character", indexRevision="characterOrNULL", apiUrl="character", accessToken="characterOrNULL", userAgent="character", timeout="numeric", verbose="logical", cache="logical"))

#' @param .Object KorAPConnection object
#' @param KorAPUrl the URL of the KorAP server instance you want to access.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @param accessToken OAuth2 access token. To use authorization based on an access token
#'   in subsequent queries, initialize your KorAP connection with
#'   `kco <- new("KorAPConnection", accessToken="<access token>")`.
#'   In order to make the API
#'   token persistent for the currently used `KorAPUrl` (you can have one
#'   token per KorAPUrl / KorAP server instance), use
#'   `persistAccessToken(kco)`. This will store it in your keyring using the
#'   [keyring()] package. Subsequent new("KorAPConnection") calls will
#'   then automatically retrieve the token from your keying. To stop using a
#'   persisted token, call `clearAccessToken(kco)`. Please note that for
#'   DeReKo, authorized queries will behave differently inside and outside the
#'   IDS, because of the special license situation. This concerns also cached
#'   results which do not take into account from where a request was issued. If
#'   you experience problems or unexpected results, please try `kco <-
#'   new("KorAPConnection", cache=FALSE)` or use
#'   [clearCache()] to clear the cache completely.
#' @param userAgent user agent string.
#' @param timeout time out in seconds.
#' @param verbose logical. Decides whether following operations will default to
#'   be verbose.
#' @param cache logical. Decides if API calls are cached locally. You can clear
#'   the cache with [clearCache()].
#' @return [KorAPConnection()] object that can be used e.g. with
#'   [corpusQuery()]
#'
#' @examples
#' \donttest{
#' kcon <- new("KorAPConnection", verbose = TRUE)
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq)
#' }
#'
#' \dontrun{
#' kcon <- new("KorAPConnection", verbose = TRUE, accessToken="e739u6eOzkwADQPdVChxFg")
#' kq <- corpusQuery(kcon, "Ameisenplage", metadataOnly=FALSE)
#' kq <- fetchAll(kq)
#' kq@collectedMatches$snippet
#' }
#'
#' @rdname KorAPConnection-class
#' @export
setMethod("initialize", "KorAPConnection",
          function(.Object, KorAPUrl = "https://korap.ids-mannheim.de/", apiVersion = 'v1.0', apiUrl, accessToken = getAccessToken(KorAPUrl), userAgent = "R-KorAP-Client", timeout=110, verbose = FALSE, cache = TRUE) {
            .Object <- callNextMethod()
            m <- regexpr("https?://[^?]+", KorAPUrl, perl = TRUE)
            .Object@KorAPUrl <- regmatches(KorAPUrl, m)
            if (!endsWith(.Object@KorAPUrl, '/')) {
              .Object@KorAPUrl <- paste0(.Object@KorAPUrl, "/")
            }
            if (missing(apiUrl)) {
              .Object@apiUrl = paste0(.Object@KorAPUrl, 'api/', apiVersion, '/')
            } else {
              .Object@apiUrl = apiUrl
            }
            .Object@accessToken = accessToken
            .Object@apiVersion = apiVersion
            .Object@userAgent = userAgent
            .Object@timeout = timeout
            .Object@verbose = verbose
            .Object@cache = cache
            welcome <- apiCall(.Object, .Object@apiUrl, json = FALSE, cache = FALSE, getHeaders = TRUE)
            message(welcome[[2]])
            .Object@indexRevision <- welcome[[1]][["x-index-revision"]]
            .Object
          })


accessTokenServiceName <- "RKorAPClientAccessToken"

setGeneric("persistAccessToken", function(kco, ...) standardGeneric("persistAccessToken") )

#' @aliases persistAccessToken
#' @rdname KorAPConnection-class
#' @import keyring
#' @export
#' @examples
#' \dontrun{
#' kco <- new("KorAPConnection", accessToken="e739u6eOzkwADQPdVChxFg")
#' persistAccessToken(kco)
#' }
#'
setMethod("persistAccessToken", "KorAPConnection",  function(kco, accessToken = kco@accessToken) {
  if (is.null(accessToken))
    stop("It seems that you have not supplied any access token that could be persisted.", call. = FALSE)

  kco@accessToken <- accessToken
  key_set_with_value(accessTokenServiceName, kco@KorAPUrl, accessToken)
})

setGeneric("clearAccessToken", function(kco) standardGeneric("clearAccessToken") )

#' @aliases clearAccessToken
#' @rdname KorAPConnection-class
#' @import keyring
#' @export
#' @examples
#' \dontrun{
#' kco <- new("KorAPConnection")
#' clearAccessToken(kco)
#' }
#'
setMethod("clearAccessToken", "KorAPConnection",  function(kco) {
  key_delete(accessTokenServiceName, kco@KorAPUrl)
})

#' @import keyring
getAccessToken <- function(KorAPUrl) {
    keyList <- tryCatch(withCallingHandlers(key_list(service = accessTokenServiceName),
                                   warning = function(w) invokeRestart("muffleWarning"),
                                   error = function(e) return(NULL)),
                          error = function(e) { })
  if (KorAPUrl %in% keyList$username)
    key_get(accessTokenServiceName, KorAPUrl)
  else
    NULL
}


warnIfNoAccessToken <- function(kco) {
  if (is.null(kco@accessToken)) {
    warning(
      paste0(
        "In order to receive KWICSs also from corpora with restricted licenses, you need an access token.\n",
        "To generate an access token, login to KorAP and navigite to KorAP's OAuth settings <",
        kco@KorAPUrl,
        "settings/oauth#page-top>"
      )
    )
  }
}

KorAPCacheSubDir <- function() {
  paste0("RKorAPClient_",
         gsub(
           "^([0-9]+\\.[0-9]+).*",
           "\\1",
           packageVersion("RKorAPClient"),
           perl = TRUE
         ))
}

setGeneric("apiCall", function(kco, ...)  standardGeneric("apiCall") )

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @aliases apiCall
#' @rdname KorAPConnection-class
#' @param kco KorAPConnection object
#' @param url request url
#' @param json logical that determines if json result is expected
#' @param getHeaders logical that determines if headers and content should be returned (as a list)
#' @importFrom jsonlite fromJSON
#' @export
setMethod("apiCall", "KorAPConnection",  function(kco, url, json = TRUE, getHeaders = FALSE, cache = kco@cache) {
  result <- ""
  if (cache) {
    result <- R.cache::loadCache(dir=KorAPCacheSubDir(), key=list(url, kco@accessToken, kco@indexRevision))
    if (!is.null(result)) {
      if (!is.null(result$meta))
        result$meta$cached <- "local"
      return(result)
    }
  }
  if (!is.null(kco@accessToken))
    resp <- GET(url, user_agent(kco@userAgent), timeout(kco@timeout), add_headers(Authorization = paste("Bearer", kco@accessToken)))
  else
    resp <- GET(url, user_agent(kco@userAgent), timeout(kco@timeout))
  if (json || status_code(resp) != 200) {
    if (json && !http_type(resp) %in% c("application/json", "application/ld+json")) {
      stop("API did not return json", call. = FALSE)
    }
    result <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"))
    if (!is.null(result$warnings)) {
      message <- if (nrow(result$warnings) > 1)
        sapply(result$warnings, function(warning) paste(sprintf("%s: %s", warning[1], warning[2]), sep="\n"))
      else
        sprintf("%s: %s", result$warnings[1], result$warnings[2])
      warning(message, call. = FALSE)
    }
  }
  if (status_code(resp) != 200) {
    if (kco@verbose) {
      cat("\n")
    }
    message <- sprintf("%s KorAP API request failed", status_code(resp))
    if (!is.null(result$errors)) {
      message <- sprintf("%s - %s %s", message, result$errors[1], result$errors[2])
    }
    stop(message, call. = FALSE)
  }
  if (!json) {
    result <- content(resp, "text", encoding = "UTF-8")
  }
  if (cache) {
    R.cache::saveCache(result, key = list(url, kco@accessToken, kco@indexRevision), dir = KorAPCacheSubDir(), compress = TRUE)
  }
  if (getHeaders) {
    list(httr::headers(resp), result)
  } else {
    result
  }
})

setGeneric("clearCache", function(kco)  standardGeneric("clearCache") )

#' @aliases clearCache
#' @rdname KorAPConnection-class
#' @export
setMethod("clearCache", "KorAPConnection",  function(kco) {
  R.cache::clearCache(dir=KorAPCacheSubDir())
})

#' @rdname KorAPConnection-class
#' @param object KorAPConnection object
#' @export
setMethod("show", "KorAPConnection", function(object) {
  cat("<KorAPConnection>", "\n")
  cat("apiUrl: ", object@apiUrl, "\n")
})

##' Funtion KorAPConnection()
##'
##' Wrappper function for new("KorAPConnection")
##'
##' @rdname KorAPConnection-constructor
##' @name KorAPConnection-constructor
##' @export
## XKorAPConnection <- function(...) new("KorAPConnection", ...)
