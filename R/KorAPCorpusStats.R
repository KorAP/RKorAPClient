#' @import jsonlite
#' @import curl

#' @export
KorAPCorpusStats <- function(con, vc = NA, query = NA) {
  if ((is.na(query) && is.na(vc)) || !(is.na(query) || is.na(vc))) {
    stop("Exaclty one of the parameters query and vc must be specified.")
  }
  if (is.na(vc)) {
    vc = query$vc
  }
  url <- paste0(con$apiUrl, 'statistics?cq=', URLencode(vc, reserved=TRUE))
  return(fromJSON(url))
}
