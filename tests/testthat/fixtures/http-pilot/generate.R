# Run from the repository root: Rscript tests/testthat/fixtures/http-pilot/generate.R
# Synthetic API-shaped data; no real requests, credentials or corpus texts.
directory <- "tests/testthat/fixtures/http-pilot"
api <- "https://korap.example.invalid/api/v1.0/"
interactions <- list()
writeFixture <- function(url, body, contentType = "application/json", headers = list()) {
  text <- if (identical(contentType, "application/json")) {
    as.character(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
  } else body
  interaction <- list(
    request = list(method = "GET", uri = url),
    response = list(status = 200L,
      headers = c(list(`content-type` = contentType), headers), body = list(string = text)),
    recorded_at = "2020-01-01 00:00:00")
  interactions[[length(interactions) + 1L]] <<- interaction
}
writeFixture(api, "KorAP offline fixture", contentType = "text/plain",
  headers = list(`x-index-revision` = "fixture-index-1"))
auth <- "https://korap.example.invalid/settings/oauth/authorize"
writeFixture(auth, "", contentType = "text/html")

url <- paste0(api, "search?q=", urltools::url_encode("[orth='Nutzer*innen']"),
  "&cq=", urltools::url_encode("pubDate since 2023 & pubDate until 2025"),
  "&ql=poliqarp&fields=textSigle,corpusTitle,pubDate&access-rewrite-disabled=true")
countUrl <- paste0(url, "&count=0")
pageUrls <- character()
writeFixture(countUrl, list(meta = list(totalResults = 51L), matches = list()))
for (offset in c(0L, 50L)) {
  indices <- seq.int(offset + 1L, min(offset + 50L, 51L))
  matches <- lapply(indices, function(i) {
    sigle <- sprintf("TEST/2023/%05d", i)
    field <- function(key, value) list(key = key, type = "type:string", value = list(value))
    list(matchID = sprintf("match-%s-p%d-%d", sigle, i * 10L, i * 10L + 2L),
      fields = list(field("textSigle", sigle), field("corpusTitle", "Test corpus 2023"),
                    field("pubDate", "2023-01-01")))
  })
  parameters <- httr2::url_parse(url)$query
  parameters$count <- 50L
  parameters$offset <- offset
  page <- httr2::url_modify(url, query = parameters)
  pageUrls <- c(pageUrls, page)
  writeFixture(page, list(meta = list(totalResults = 51L, startIndex = offset,
    itemsPerPage = length(indices)), matches = matches))
  if (offset == 50L) {
    parameters$count <- 1L
    partialPage <- httr2::url_modify(url, query = parameters)
    writeFixture(partialPage,
      list(meta = list(totalResults = 51L, startIndex = offset, itemsPerPage = 1L),
        matches = matches))
  }
}
emptyUrl <- paste0(api, "search?q=absent&ql=poliqarp&fields=textSigle&access-rewrite-disabled=true&count=0")
writeFixture(emptyUrl,
  list(meta = list(totalResults = 0L), matches = list()))
for (vc in c("", "pubDate since 2020 & pubDate until 2021")) {
  size <- if (nzchar(vc)) 1L else 10L
  writeFixture(paste0(api, "statistics?cq=", URLencode(vc, reserved = TRUE)),
    list(documents = 100L * size, tokens = 25000L * size,
      sentences = 1200L * size, paragraphs = 400L * size))
}
writeResponse <- function(path, status, text, contentType) {
  entry <- list(request = list(method = "GET", uri = paste0(api, path)),
    response = list(status = status, headers = list(`content-type` = contentType),
      body = list(string = text)), recorded_at = "2020-01-01 00:00:00")
  interactions[[length(interactions) + 1L]] <<- entry
}
writeResponse("unauthorized", 401L, '{"errors":[[401,"Invalid access token"]]}', "application/json")
writeResponse("bad-gateway", 502L, "Bad Gateway", "text/plain")
writeResponse("missing", 404L, "Not Found", "text/plain")
writeResponse("html", 200L, "<html>Not JSON</html>", "text/html")
writeResponse("broken-json", 200L, "{broken JSON", "application/json")
writeFixture(paste0(api, "ok"), list(meta = list(totalResults = 2L)))
writeFixture(paste0(api, "warning"), list(meta = list(totalResults = 2L),
  warnings = list(list(682L, "Synthetic timeout warning"))))
# One cassette per scenario keeps vcr's sequential request matching cheap and
# makes the required order and number of requests explicit.
uris <- vapply(interactions, function(x) x$request$uri, character(1))
saveCassette <- function(name, requests) {
  indices <- match(requests, uris)
  stopifnot(!anyNA(indices))
  jsonlite::write_json(list(http_interactions = interactions[indices],
    recorded_with = "Synthetic KorAP fixtures"), file.path(directory, paste0(name, ".json")),
    auto_unbox = TRUE, pretty = TRUE)
}
common <- c(api, auth)
stats <- paste0(api, "statistics?cq=", URLencode("pubDate since 2020 & pubDate until 2021", reserved = TRUE))
saveCassette("connection", rep(common, 2))
saveCassette("count", c(common, countUrl, emptyUrl))
saveCassette("fetch", c(common, countUrl, rep(pageUrls, 2)))
saveCassette("cache", c(common, countUrl))
saveCassette("statistics", c(common, stats, paste0(api, "statistics?cq=")))
saveCassette("workflow", c(common, countUrl, pageUrls[1], partialPage, stats, stats))
saveCassette("connection-init-errors", paste0(api, c("unauthorized", "bad-gateway")))
saveCassette("api-errors", c(common, paste0(api, c("missing", "html", "broken-json"))))
saveCassette("connection-cache", c(common, rep(paste0(api, "ok"), 2)))
saveCassette("connection-warning", c(common, rep(paste0(api, "warning"), 2)))
