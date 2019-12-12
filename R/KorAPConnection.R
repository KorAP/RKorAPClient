################################################################################
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
################################################################################
setClassUnion("characterOrNULL", c("character", "NULL"))

#' Class KorAPConnection
#'
#' \code{KorAPConnection} objects represent the connection to a KorAP server.
#' New \code{KorAPConnection} objects can be created by \code{new("KorAPConnection")}.
#'
#' @import R.cache
#' @import utils
#' @import methods
#' @export
KorAPConnection <- setClass("KorAPConnection", slots=c(KorAPUrl="character", apiVersion="character", apiUrl="character", accessToken="characterOrNULL", userAgent="character", timeout="numeric", verbose="logical", cache="logical"))

#' @param .Object KorAPConnection object
#' @param KorAPUrl the URL of the KorAP server instance you want to access.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @param accessToken OAuth2 access token. To use authorization based on an access token
#'   in subsequent queries, intialize your KorAP connection with
#'   \code{kco <- new("KorAPConnection", accessToken="<access token>")}.
#'   In order to make the API
#'   token persistent for the currently used \code{KorAPUrl} (you can have one
#'   token per KorAPUrl / KorAP server instance), use
#'   \code{persistAccessToken(kco)}. This will store it in your keyring using the
#'   \code{\link{keyring}} package. Subsequent new("KorAPConnection") calls will
#'   then automatically retrieve the token from your keying. To stop using a
#'   persisted token, call \code{clearAccessToken(kco)}. Please note that for
#'   DeReKo, authorized queries will behave differently inside and outside the
#'   IDS, because of the special license situation. This concerns also cached
#'   results which do not take into account from where a request was issued. If
#'   you experience problems or unexpected results, please try \code{kco <-
#'   new("KorAPConnection", cache=FALSE)} or use
#'   \code{\link{clearCache}} to clear the cache completely.
#' @param userAgent user agent string.
#' @param timeout time out in seconds.
#' @param verbose logical. Decides whether following operations will default to
#'   be verbose.
#' @param cache logical. Decides if API calls are cached locally. You can clear
#'   the cache with \code{\link{clearCache}()}.
#' @return \code{\link{KorAPConnection}} object that can be used e.g. with
#'   \code{\link{corpusQuery}}
#'
#' @examples
#' kcon <- new("KorAPConnection", verbose = TRUE)
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq)
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
  if (KorAPUrl %in% keyList)
    key_get(accessTokenServiceName, KorAPUrl)
  else
    NULL
}

KorAPCacheSubDir <- function() {
  paste0("RKorAPClient_", packageVersion("RKorAPClient"))
}

setGeneric("apiCall", function(kco, ...)  standardGeneric("apiCall") )

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @aliases apiCall
#' @rdname KorAPConnection-class
#' @param kco KorAPConnection object
#' @param url request url
#' @importFrom jsonlite fromJSON
#' @export
setMethod("apiCall", "KorAPConnection",  function(kco, url) {
  if (kco@cache) {
    parsed <- R.cache::loadCache(dir=KorAPCacheSubDir(), key=list(url, kco@accessToken))
    if (!is.null(parsed)) {
      return(parsed)
    }
  }
  if (!is.null(kco@accessToken))
    resp <- GET(url, user_agent(kco@userAgent), timeout(kco@timeout), add_headers(Authorization = paste("Bearer", kco@accessToken)))
  else
    resp <- GET(url, user_agent(kco@userAgent), timeout(kco@timeout))
  if (!http_type(resp) %in% c("application/json", "application/ld+json")) {
    stop("API did not return json", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(content(resp, "text"))
  if (!is.null(parsed$warnings)) {
    message <- if (nrow(parsed$warnings) > 1)
      sapply(parsed$warnings, function(warning) paste(sprintf("%s: %s", warning[1], warning[2]), sep="\n"))
    else
      sprintf("%s: %s", parsed$warnings[1], parsed$warnings[2])
    warning(message, call. = FALSE)
  }
  if (status_code(resp) != 200) {
    message <- if (!is.null(parsed$errors))
                 sapply(parsed$errors, function(error) paste0(sprintf("\n%s: KorAP API request failed: %s", error[1], error[2])))
               else
                 message <- sprintf("%s: KorAP API request failed.", status_code(resp))
    stop(message, call. = FALSE)
  }
  if (kco@cache) {
    R.cache::saveCache(parsed, key = list(url, kco@accessToken), dir = KorAPCacheSubDir(), compress = TRUE)
  }
  parsed
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
