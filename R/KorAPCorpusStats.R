#' @import jsonlite
#' @import curl

dummy <- NA # without this roxygen2 fails

#' Fetch information about a (virtual) corpus
#' @param con object obtained from \code{\link{KorAPConnection}}
#' @param query object returned from \code{\link{KorAPQuery}}
#' @param vc string describing the virtual corpus. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @export
#' @return object with the fields \code{$documents}, \code{$tokens}, \code{$sentences}, \code{$paragraphs}
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
