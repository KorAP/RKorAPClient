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

# Request/response cases for the former live corpusQuery regressions.
defaultFields <- c("corpusSigle", "corpusTitle", "textSigle", "pubDate", "pubPlace",
  "availability", "textClass", "dmozDomain", "wikiDomain", "snippet", "tokens")
searchUrl <- function(query, vc = "", fields = defaultFields, metadataOnly = TRUE,
                      count = 0L, offset = NULL, base = api) {
  if (metadataOnly) fields <- setdiff(fields, c("snippet", "tokens"))
  if (!"textSigle" %in% fields) fields <- c(fields, "textSigle")
  result <- paste0(base, "search?q=", urltools::url_encode(enc2utf8(query)),
    if (nzchar(vc)) paste0("&cq=", urltools::url_encode(enc2utf8(vc))) else "",
    if (!metadataOnly) "&show-tokens=true" else "", "&ql=poliqarp&fields=",
    paste(fields, collapse = ","), if (metadataOnly) "&access-rewrite-disabled=true" else "")
  if (is.null(offset)) paste0(result, "&count=", count) else {
    parameters <- httr2::url_parse(result)$query
    parameters$count <- count
    parameters$offset <- offset
    httr2::url_modify(result, query = parameters)
  }
}
countFixture <- function(query, vc = "", total = 3L, ...) {
  url <- searchUrl(query, vc, ...)
  writeFixture(url, list(meta = list(totalResults = total), matches = list()))
  url
}
artificialMatches <- function(n, offset = 0L, tokens = FALSE, extended = FALSE) {
  lapply(seq_len(n), function(i) {
    sigle <- sprintf("TEST/2023/%05d", offset + i)
    values <- list(corpusSigle = "TEST", corpusTitle = "Synthetic corpus", textSigle = sigle,
      pubDate = "2023-01-01", pubPlace = "N\u00fcrnberg", availability = "CC-BY",
      textClass = "wissenschaft", dmozDomain = "wissenschaft populaerwissenschaft", wikiDomain = "")
    if (extended) values <- c(values, list(KED.nToks = "300", KED.nSent = "12",
      KED.rcpnt = "Synthetic recipients"))
    fields <- lapply(names(values), function(key) list(key = key, type = "type:string", value = list(values[[key]])))
    position <- 2000L + i
    result <- list(matchID = sprintf("match-%s-p%d-%d", sigle, position, position + 1L), fields = fields)
    if (tokens) {
      result$tokens <- list(left = list("Eine", "reine"), match = list("Newstickeritis"),
        right = list("ist", "bekannt"))
      result$snippet <- "Eine reine <mark>Newstickeritis</mark> ist bekannt."
    }
    result
  })
}
pageFixture <- function(query, vc = "", n = 3L, total = n, offset = 0L,
                        tokens = FALSE, extended = FALSE, count = 50L, ...) {
  url <- searchUrl(query, vc, count = count, offset = offset, ...)
  writeFixture(url, list(meta = list(totalResults = total, startIndex = offset, itemsPerPage = n),
    matches = artificialMatches(n, offset, tokens, extended)))
  url
}
queryVc <- "pubDate since 2014"
basicCount <- countFixture("Ameisenplage", queryVc)
basicPage <- pageFixture("Ameisenplage", queryVc)
sigleCount <- countFixture("Ameisenplage", queryVc, fields = "textSigle")
siglePage <- pageFixture("Ameisenplage", queryVc, fields = "textSigle")
frequencyStats <- paste0(api, "statistics?cq=", URLencode(queryVc, reserved = TRUE))
writeFixture(frequencyStats, list(documents = 1000L, tokens = 100000L, sentences = 4000L, paragraphs = 2000L))
umlautCount <- countFixture("Ameisenb\u00e4r", "pubPlace=N\u00fcrnberg", total = 7L)
noHits <- countFixture("Xmeisenplagx", total = 0L)
emptyPrint <- countFixture("Xmeisenplagx", "pubDate in 2012", total = 0L)
largeCount <- countFixture("<base/s=t>", total = 1000050L)
largePage <- pageFixture("<base/s=t>", offset = 1000000L, n = 50L, total = 1000050L)
badQuery <- searchUrl("[[xx")
writeFixture(badQuery, list(errors = list(list(302L, "unbalanced brackets"))))
interactions[[length(interactions)]]$response$status <- 400L
tokenQuery <- "focus([tt/p=ADJA] {Newstickeritis})"
tokenVc <- "corpusSigle=/W.D17/"
tokenCount <- countFixture(tokenQuery, tokenVc, total = 12L, metadataOnly = FALSE)
tokenPage <- pageFixture(tokenQuery, tokenVc, n = 12L, tokens = TRUE, metadataOnly = FALSE)
selectedTokenCount <- countFixture(tokenQuery, tokenVc, total = 12L, fields = "tokens", metadataOnly = FALSE)
selectedTokenPage <- pageFixture(tokenQuery, tokenVc, n = 12L, tokens = TRUE, fields = "tokens", metadataOnly = FALSE)
kedRoot <- "https://korap.example.invalid/instance/ked/"
kedApi <- paste0(kedRoot, "api/v1.0/")
kedAuth <- paste0(kedRoot, "settings/oauth/authorize")
writeFixture(kedApi, "Synthetic KED instance", contentType = "text/plain",
  headers = list(`x-index-revision` = "fixture-ked-1"))
writeFixture(kedAuth, "", contentType = "text/html")
kedFields <- c("textSigle", "pubDate", "pubPlace", "availability", "textClass", "snippet", "tokens",
  "KED.cover1Herder", "KED.cover2Herder", "KED.cover3Herder", "KED.cover4Herder", "KED.cover5Herder",
  "KED.nPara", "KED.nPunct1kTks", "KED.nSent", "KED.nToks", "KED.nToksSentMd", "KED.nTyps",
  "KED.rcpnt", "KED.rcpntLabel", "KED.strtgy", "KED.strtgyLabel", "KED.topic", "KED.topicLabel",
  "KED.txttyp", "KED.txttypLabel")
kedCount <- countFixture("einfache", fields = kedFields, base = kedApi)
kedPage <- pageFixture("einfache", fields = kedFields, base = kedApi, extended = TRUE)
alternativeVc <- function(year) paste("textType = /Zeit.*/ & pubDate in", year)
alternativeOne <- "macht []{0,3} Sinn"
alternativeTwo <- "ergibt []{0,3} Sinn"
alternativeCounts <- c(countFixture(alternativeOne, alternativeVc(1999), total = 100L),
  countFixture(alternativeTwo, alternativeVc(1999), total = 200L),
  countFixture(alternativeTwo, alternativeVc(2000), total = 150L))
annualCounts <- vapply(seq_len(4), function(i) countFixture(c("macht", "ergibt", "ist", "wird")[i],
  alternativeVc(1998 + i), total = 100L + i * 10L), character(1))
nodeCount <- countFixture("Ameisenplage", total = 200L)
collocateCount <- countFixture("heimgesucht", total = 1000L)
pairCount <- countFixture("contains(<base/s=s>, (Ameisenplage heimgesucht))", total = 50L)
randomCount <- countFixture("Test", queryVc, total = 500L, fields = "textSigle")
randomPages <- withr::with_seed(7, c(sample(0:9, 4), sample(0:9, 1)))
randomRequests <- vapply(seq_along(randomPages), function(i) {
  count <- if (i == 4L) 25L else 50L
  pageFixture("Test", queryVc, n = count, total = 500L, offset = randomPages[i] * 50L,
    count = count, fields = "textSigle")
}, character(1))
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
saveCassette("query-frequency-cache", c(common, basicCount, frequencyStats))
saveCassette("query-index-cache", c(common, rep(c(basicCount, frequencyStats), 2)))
saveCassette("query-collocation", c(common, nodeCount, paste0(api, "statistics?cq="), collocateCount, pairCount))
saveCassette("query-portable", c(common, rep(umlautCount, 3)))
saveCassette("query-metadata", c(common, basicCount, basicPage))
saveCassette("query-empty", c(common, noHits))
saveCassette("query-print-empty", c(common, emptyPrint))
saveCassette("query-print-matches", c(common, sigleCount, siglePage))
saveCassette("query-offset", c(common, largeCount, largePage))
saveCassette("query-invalid", c(common, badQuery))
saveCassette("query-tokens", c(common, tokenCount, tokenPage))
saveCassette("query-selected-tokens", c(common, selectedTokenCount, selectedTokenPage))
saveCassette("query-ked", c(kedApi, kedAuth, kedCount, kedPage))
saveCassette("query-alternatives", c(common, alternativeCounts[1:2], alternativeCounts[c(1, 3)], annualCounts))
saveCassette("fetch-random", c(common, randomCount, randomRequests))
