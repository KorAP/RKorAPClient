#' Class KorAPConnection
#'
#' \code{KorAPConnection} objetcs represent the connection to a KorAP server.
#' New \code{KorAPConnection} objects can be created by \code{KorAPConnection()}
#'
#' @import jsonlite
#' @import utils
#' @import methods
#'
#'

#' @export
KorAPConnection <- setClass("KorAPConnection", slots=c(KorAPUrl="character", apiVersion="character", apiUrl="character"))

#' @param .Object KorAPConnection object
#' @param KorAPUrl the URL of the KorAP server instance you want to access.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @return \code{\link{KorAPConnection}} object that can be used e.g. with \code{\link{corpusQuery}}
#'
#' @examples
#' kcon <- new("KorAPConnection")
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq, verbose=TRUE)
#'
#' @note Currently it is not possible to authenticate the client
#'
#' @rdname KorAPConnection-class
#' @export
setMethod("initialize", "KorAPConnection",
          function(.Object, KorAPUrl = "https://korap.ids-mannheim.de/instance/test/", apiVersion = 'v1.0', apiUrl) {
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
            .Object
          })

#' @rdname KorAPConnection-class
#' @param object KorAPConnection object
#' @export
setMethod("show", "KorAPConnection", function(object) {
  cat("<KorAPConnection>", "\n")
  cat("apiUrl: ", object@apiUrl, "\n")
})

#' Funtion KorAPConnection()
#'
#' Wrappper function for new("KorAPConnection")
#'
#' @rdname KorAPConnection-constructor
#' @name KorAPConnection-constructor
#' @export
# KorAPConnection <- function(...) new("KorAPConnection", ...)

