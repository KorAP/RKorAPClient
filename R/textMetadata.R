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
#' @return Tibble with columns for each metadata property. In case of errors, such as non-existing texts/sigles, the tibble will also contain a column called `errors`.
#' If there are metadata columns you cannot make sense of, please ignore them. The function simply returns all the metadata it gets from the server.
#'
#' @importFrom urltools url_encode
#' @importFrom dplyr bind_rows relocate mutate
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
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
      if ("document" %in% names(res) & "fields" %in% names(res$document) && length(res$document$fields) > 0) {
        res <- as_tibble(res$document$fields) %>%
          select(key, value) %>%
          tidyr::pivot_wider(names_from = key, values_from = value, names_repair = "unique") %>%
          mutate(
            textSigle = as.character(textSigle),
            requestUrl = url,
            webUIRequestUrl = paste0(kco@KorAPUrl, sprintf('?q=<base/s=t>&cq=textSigle+%%3D+"%s"', url_encode(enc2utf8(textSigle))))) %>%
        relocate(textSigle)
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
    }
    res
  }
})


