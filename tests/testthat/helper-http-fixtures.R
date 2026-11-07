# Scope all replacements to the callback. API parsing, URL construction and
# pagination run normally; only connectivity probing and transport are isolated.
withKorapFixtureApi <- function(code) {
  testthat::skip_if_not_installed("vcr", minimum_version = "2.0.0")
  state <- new.env(parent = emptyenv())
  state$requests <- character()
  state$missing <- character()
  state$transfers <- 0L
  testthat::local_mocked_bindings(
    has_internet = function() TRUE,
    curl_fetch_memory = function(...) {
      state$transfers <- state$transfers + 1L
      stop("Real HTTP transfers are disabled in fixture tests.")
    },
    .package = "curl"
  )
  previousMock <- getOption("httr2_mock")
  on.exit(options(httr2_mock = previousMock), add = TRUE)
  vcr::local_vcr_configure(ignore_hosts = NULL, ignore_localhost = FALSE,
    turned_off = FALSE, re_record_interval = NULL)
  on.exit({
    # apiCall() intentionally handles HTTP failures, so a missing fixture must
    # also fail here instead of becoming an apparently legitimate NULL result.
    testthat::expect_equal(length(state$missing), 0L, info = paste(state$missing, collapse = "\n"))
    testthat::expect_identical(state$transfers, 0L)
  }, add = TRUE)
  vcr::use_cassette("http-pilot", {
    mock <- getOption("httr2_mock")
    options(httr2_mock = function(req) {
      state$requests <- c(state$requests, req$url)
      tryCatch(mock(req), error = function(e) {
        state$missing <- c(state$missing, req$url)
        stop(e)
      })
    })
    result <- code(state)
  }, dir = testthat::test_path("fixtures", "http-pilot"), record = "none",
    match_requests_on = c("method", "uri"), serialize_with = "json")
  result
}

fixtureKorapConnection <- function(cache = FALSE, verbose = FALSE) {
  KorAPConnection(KorAPUrl = "https://korap.example.invalid/", accessToken = NULL,
    cache = cache, verbose = verbose)
}

fixtureKorapQuery <- function(kco, ...) {
  corpusQuery(kco, "[orth='Nutzer*innen']", vc = "pubDate in 2023",
    fields = c("textSigle", "corpusTitle", "pubDate"), ...)
}
