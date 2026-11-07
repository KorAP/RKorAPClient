# Scope all replacements to the callback. API parsing, URL construction and
# pagination run normally; only connectivity probing and transport are isolated.
withKorapFixtureApi <- function(code, cassette = "connection") {
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
  vcr::use_cassette(cassette, {
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

korapHttpTestQuery <- function(kco, ...) {
  corpusQuery(kco, "[orth='Nutzer*innen']", vc = "pubDate since 2023 & pubDate until 2025",
    fields = c("textSigle", "corpusTitle", "pubDate"), ...)
}

# The same server-independent assertions run against synthetic cassettes and,
# when explicitly enabled, KorAP. Exact fixture counts are checked separately.
checkKorapHttpWorkflow <- function(kco) {
  testthat::expect_s4_class(kco, "KorAPConnection")
  testthat::expect_false(is.null(kco@welcome))
  testthat::expect_true(nzchar(kco@indexRevision))
  q <- korapHttpTestQuery(kco)
  testthat::expect_s4_class(q, "KorAPQuery")
  testthat::expect_true(is.finite(q@totalResults) && q@totalResults > 0)
  testthat::expect_false(isTRUE(q@apiResponse$meta$timeExceeded))
  testthat::expect_null(q@collectedMatches)
  result <- fetchNext(q, maxFetch = 51)
  matches <- result@collectedMatches
  testthat::expect_equal(nrow(matches), min(51, q@totalResults))
  testthat::expect_true(all(c("textSigle", "corpusTitle", "pubDate", "matchID",
    "matchStart", "matchEnd") %in% names(matches)))
  testthat::expect_equal(length(unique(matches$matchID)), nrow(matches))
  testthat::expect_true(all(nzchar(matches$textSigle)))
  testthat::expect_true(all(is.finite(matches$matchStart) & matches$matchStart <= matches$matchEnd))
  vc <- "pubDate since 2020 & pubDate until 2021"
  stats <- corpusStats(kco, vc)
  df <- corpusStats(kco, vc, as.df = TRUE)
  testthat::expect_s4_class(stats, "KorAPCorpusStats")
  testthat::expect_s3_class(df, "data.frame")
  sizes <- c(stats@documents, stats@tokens, stats@sentences, stats@paragraphs)
  testthat::expect_true(all(is.finite(sizes) & sizes > 0))
  testthat::expect_equal(unlist(df[c("documents", "tokens", "sentences", "paragraphs")],
    use.names = FALSE), sizes)
  testthat::expect_identical(httr2::url_parse(df$webUIRequestUrl)$query$cq, vc)
  invisible(result)
}
