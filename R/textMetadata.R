setGeneric("textMetadata", function(kco, ...)  standardGeneric("textMetadata") )

#' Retrieve metadata for a text, identified by its sigle (id)
#'
#' @aliases textMetadata
#'
#' @description
#' Retrieves metadata for a text, identified by its sigle (id) using the corresponding KorAP API
#' (see [Kustvakt Wiki](https://github.com/KorAP/Kustvakt/wiki/Service:-Metadata-Retrieval)).
#'
#'
#' @param kco [KorAPConnection()] object (obtained e.g. from `new("KorAPConnection")`)
#' @param textSigle unique text id (concatenation of corpus, document and text ids, separated by `/`, e.g. ) or vector thereof
#' @param verbose logical. If `TRUE`, additional diagnostics are printed. Defaults to `kco@verbose`.
#'
#' @return Tibble with columns for every metadata property. In case of errors, like non-existing texts/sigles, the tibble will also contain a row called `errors`.
#'
#' @importFrom urltools url_encode
#' @importFrom dplyr bind_rows relocate mutate
#'
#' @examples
#' \dontrun{
#' new("KorAPConnection") %>% textMetadata(c("WUD17/A97/08542", "WUD17/B96/57558", "WUD17/A97/08541"))
#' }
#'
#' @export
setMethod("textMetadata", "KorAPConnection",
 function(kco, textSigle, verbose = kco@verbose) {
  if (length(textSigle) > 1)
    do.call(bind_rows, Map(function(atomicSigle)
      textMetadata(kco, atomicSigle), textSigle))
  else {
    url <-
      paste0(kco@apiUrl, 'corpus/',
             URLencode(enc2utf8(textSigle), reserved = TRUE))
    log_info(verbose, "Getting metadata for ", textSigle, sep = "")
    res <- apiCall(kco, url)
    log_info(verbose, ifelse(is.null(res) || "errors" %in% names(res), " [error]\n",  "\n"))

    if(is.null(res)) {
      res <- tibble(errors="API request failed")
    } else {
      res <- lapply(res, function(x) paste0(x, collapse = "\\t")) # flatten list
      res <- as_tibble(res) %>%
        head(n=1) %>%
        mutate(
          requestUrl = url,
          textSigle = textSigle,
          webUIRequestUrl = paste0(kco@KorAPUrl, sprintf('?q=<base/s=t>&cq=textSigle+%%3D+"%s"', url_encode(enc2utf8(textSigle))))) %>%
        relocate(textSigle)
    }
    res
  }
})


