#' Class KorAPConnection
#'
#' \code{KorAPConnection} objects represent the connection to a KorAP server.
#' New \code{KorAPConnection} objects can be created by \code{KorAPConnection()}.
#'
#' @import jsonlite
#' @import utils
#' @import methods
#' @import dplyr
#' @import purrr
#' @import tidyr
#'

#' @export
KorAPConnection <- setClass("KorAPConnection", slots=c(KorAPUrl="character", apiVersion="character", apiUrl="character", userAgent="character", timeout="numeric", verbose="logical"))

#' @param .Object KorAPConnection object
#' @param KorAPUrl the URL of the KorAP server instance you want to access.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @param userAgent user agent string.
#' @param timeout time out in seconds.
#' @param verbose logical. Decides whether following operations will default to be verbose.
#' @return \code{\link{KorAPConnection}} object that can be used e.g. with \code{\link{corpusQuery}}
#'
#' @examples
#' kcon <- new("KorAPConnection", verbose = TRUE)
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq)
#'
#' @note Currently it is not possible to authenticate the client
#'
#' @rdname KorAPConnection-class
#' @export
setMethod("initialize", "KorAPConnection",
          function(.Object, KorAPUrl = "https://korap.ids-mannheim.de/", apiVersion = 'v1.0', apiUrl, userAgent = "R-KorAP-Client", timeout=10, verbose = FALSE) {
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
            .Object@apiVersion = apiVersion
            .Object@userAgent = userAgent
            .Object@timeout = timeout
            .Object@verbose = verbose
            .Object
          })

setGeneric("apiCall", function(kco, ...)  standardGeneric("apiCall") )

#' @aliases apiCall
#' @rdname KorAPConnection-class
#' @param kco KorAPConnection object
#' @param url request url
#' @export
setMethod("apiCall", "KorAPConnection",  function(kco, url) {
  resp <- GET(url, user_agent(kco@userAgent), timeout(kco@timeout))
  if (!http_type(resp) %in% c("application/json", "application/ld+json")) {
    stop("API did not return json", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(content(resp, "text"))
  if (!is.null(parsed$warnings)) {
    message <- ifelse (nrow(parsed$warnings) > 1,
                       sapply(parsed$warnings, function(warning) paste(sprintf("%s: %s", warning[1], warning[2]), sep="\n")),
                       sprintf("%s: %s", parsed$warnings[1], parsed$warnings[2]))
    warning(message, call. = FALSE)
  }
  if (status_code(resp) != 200) {
    message <- ifelse (!is.null(parsed$errors),
                       sapply(parsed$errors, function(error) paste0(sprintf("\n%s: KorAP API request failed: %s", error[1], error[2]))),
                       message <- sprintf("%s: KorAP API request failed.", status_code(resp)))
    stop(message, call. = FALSE)
  }
  parsed
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
