#' @import jsonlite
#' @import curl
#' @import utils

defaultKorAPUrl <- "https://korap.ids-mannheim.de/"

#' Connect to a KorAP server.
#'
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @param apiVersion which version of KorAP's API you want to connect to
#' @param apiUrl url of the KorAP web service
#' @return object that contains all connection information and can be used with \code{\link{KorAPQuery}}
#'
#' @note Currently it is not possible to authenticate the client
#'
#' @export
KorAPConnection <- function(KorAPUrl=defaultKorAPUrl, apiVersion='v1.0', apiUrl) {
  m <-regexpr("https?://[^?]+", KorAPUrl, perl = TRUE)
  KorAPUrl <- regmatches(KorAPUrl, m)
  if (!endsWith(KorAPUrl, '/')) {
    KorAPUrl <- paste0(KorAPUrl, "/")
  }
  if (missing(apiUrl)) {
    apiUrl = paste0(KorAPUrl, 'api/', apiVersion, '/')
  }
  con <- data.frame(apiUrl, KorAPUrl, apiVersion)
  return(con)
}
