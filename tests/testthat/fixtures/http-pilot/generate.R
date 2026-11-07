# Run from the repository root: Rscript tests/testthat/fixtures/http-pilot/generate.R
# Synthetic API-shaped data; no real requests, credentials or corpus texts.
directory <- "tests/testthat/fixtures/http-pilot"
api <- "https://korap.example.invalid/api/v1.0/"
interactions <- list()
writeFixture <- function(url, body, contentType = "application/json", headers = list(), copies = 1L) {
  text <- if (identical(contentType, "application/json")) {
    as.character(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
  } else body
  interaction <- list(
    request = list(method = "GET", uri = url),
    response = list(status = 200L,
      headers = c(list(`content-type` = contentType), headers), body = list(string = text)),
    recorded_at = "2020-01-01 00:00:00")
  for (i in seq_len(copies)) interactions[[length(interactions) + 1L]] <<- interaction
}
writeFixture(api, "KorAP offline fixture", contentType = "text/plain",
  headers = list(`x-index-revision` = "fixture-index-1"))
auth <- "https://korap.example.invalid/settings/oauth/authorize"
writeFixture(auth, "", contentType = "text/html")

url <- paste0(api, "search?q=", urltools::url_encode("[orth='Nutzer*innen']"),
  "&cq=", urltools::url_encode("pubDate in 2023"),
  "&ql=poliqarp&fields=textSigle,corpusTitle,pubDate&access-rewrite-disabled=true")
writeFixture(paste0(url, "&count=0"), list(meta = list(totalResults = 51L), matches = list()))
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
  # A continuation followed by fetchAll() requests both pages twice.
  writeFixture(page, list(meta = list(totalResults = 51L, startIndex = offset,
    itemsPerPage = length(indices)), matches = matches), copies = 2L)
}
writeFixture(paste0(api, "search?q=absent&ql=poliqarp&fields=textSigle&access-rewrite-disabled=true&count=0"),
  list(meta = list(totalResults = 0L), matches = list()))
for (vc in c("", "pubDate since 2020 & pubDate until 2021")) {
  size <- if (nzchar(vc)) 1L else 10L
  writeFixture(paste0(api, "statistics?cq=", URLencode(vc, reserved = TRUE)),
    list(documents = 100L * size, tokens = 25000L * size,
      sentences = 1200L * size, paragraphs = 400L * size))
}
jsonlite::write_json(list(http_interactions = interactions,
  recorded_with = "Synthetic KorAP fixtures"), file.path(directory, "http-pilot.json"),
  auto_unbox = TRUE, pretty = TRUE)
