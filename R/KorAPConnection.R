#' @import jsonlite
#' @import curl

defaultKorAPUrl <- "https://korap.ids-mannheim.de/"

#' \code{KorAPQuery} initiates a connect to some KorAP server.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.
#' @return object that contains all necessary connection information and can be used on \code{\link{KorAPQuery}}
#' @export
KorAPConnection <- function(KorAPUrl=defaultKorAPUrl, apiVersion='v1.0', apiUrl = NA) {
  m <-regexpr("https?://[^?]+", KorAPUrl, perl = TRUE)
  KorAPUrl <- regmatches(KorAPUrl, m)
  if (!endsWith(KorAPUrl, '/')) {
    KorAPUrl <- paste0(KorAPUrl, "/")
  }
  if (is.na(apiUrl)) {
    apiUrl = paste0(KorAPUrl, 'api/', apiVersion, '/')
  }
  con <- data.frame(apiUrl, KorAPUrl, apiVersion)
  return(con)
}
