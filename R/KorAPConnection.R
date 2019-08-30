#' @import jsonlite
#' @import curl

library(jsonlite)

defaultKorAPUrl <- "https://korap.ids-mannheim.de/"

KorAPConnection <- function(KorAPUrl=defaultKorAPUrl, apiVersion='v1.0', apiUrl = paste0(KorAPUrl, 'api/' ,apiVersion, '/')) {
  con <- data.frame(apiUrl, KorAPUrl, apiVersion)
  return(con)
}
