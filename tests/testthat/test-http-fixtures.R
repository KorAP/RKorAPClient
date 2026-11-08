test_that("a connection uses fixture welcome headers and the authorization probe", {
  withKorapFixtureApi(function(state) {
    expect_message(kco <- fixtureKorapConnection(), "KorAP offline fixture")
    expect_s4_class(kco, "KorAPConnection")
    expect_identical(kco@apiUrl, "https://korap.example.invalid/api/v1.0/")
    expect_identical(kco@indexRevision, "fixture-index-1")
    expect_true(kco@authorizationSupported)
    expect_identical(state$requests, c(kco@apiUrl,
      "https://korap.example.invalid/settings/oauth/authorize"))
  })
})

test_that("count queries use actual request encoding and JSON parsing offline", {
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    q <- korapHttpTestQuery(kco)
    expect_s4_class(q, "KorAPQuery")
    expect_equal(q@totalResults, 51)
    expect_true(q@hasMoreMatches)
    expect_null(q@collectedMatches)
    query <- httr2::url_parse(tail(state$requests, 1))$query
    expect_identical(query$q, "[orth='Nutzer*innen']")
    expect_identical(query$cq, "pubDate since 2023 & pubDate until 2025")
    expect_identical(query$fields, "textSigle,corpusTitle,pubDate")
    expect_identical(query$`access-rewrite-disabled`, "true")
    expect_identical(query$count, "0")
    expect_identical(query$ql, "poliqarp")
    empty <- corpusQuery(kco, "absent", fields = "textSigle")
    expect_equal(empty@totalResults, 0)
    expect_false(empty@hasMoreMatches)
  }, cassette = "count")
})

test_that("the shared live-server contract also holds for synthetic responses", {
  withKorapFixtureApi(function(state) {
    checkKorapHttpWorkflow(suppressMessages(fixtureKorapConnection()))
  }, cassette = "workflow")
})

test_that("a two-page fetch parses fields, match positions and continuation offline", {
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    q <- korapHttpTestQuery(kco)
    first <- fetchNext(q)
    expect_equal(nrow(first@collectedMatches), 50)
    expect_equal(first@nextStartIndex, 50)
    expect_true(first@hasMoreMatches)
    result <- fetchRest(first)
    expect_equal(nrow(result@collectedMatches), 51)
    expect_equal(result@nextStartIndex, 51)
    expect_false(result@hasMoreMatches)
    expect_equal(length(unique(result@collectedMatches$matchID)), 51)
    expect_identical(result@collectedMatches$textSigle[51], "TEST/2023/00051")
    expect_identical(result@collectedMatches$corpusTitle[51], "Test corpus 2023")
    expect_equal(result@collectedMatches$matchStart, 10L * seq_len(51))
    expect_equal(result@collectedMatches$matchEnd, 10L * seq_len(51) + 1L)
    pages <- lapply(tail(state$requests, 2), function(url) httr2::url_parse(url)$query)
    expect_equal(vapply(pages, function(p) as.numeric(p$offset), numeric(1)), c(0, 50))
    expect_equal(vapply(pages, function(p) as.numeric(p$count), numeric(1)), c(50, 50))
    # fetchAll() takes the same pages and replaces a previously fetched prefix.
    all <- fetchAll(first)
    expect_equal(all@collectedMatches, result@collectedMatches)
  }, cassette = "fetch")
})

test_that("fixture-based requests can use a fresh isolated real response cache", {
  localKorapTestCache("http-fixture-cache-")
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection(cache = TRUE))
    first <- korapHttpTestQuery(kco)
    before <- length(state$requests)
    cached <- korapHttpTestQuery(kco)
    expect_equal(cached@totalResults, first@totalResults)
    expect_identical(cached@apiResponse$meta$cached, "local")
    expect_length(state$requests, before)
  }, cassette = "cache")
})

test_that("an unknown request never falls back to a real server", {
  skip_if_not_installed("vcr", minimum_version = "2.0.0")
  httr2::with_mocked_responses(NULL, {
    vcr::local_vcr_configure(ignore_hosts = NULL, ignore_localhost = FALSE,
      turned_off = FALSE, re_record_interval = NULL)
    vcr::use_cassette("connection", {
      expect_error(httr2::req_perform(httr2::request("https://korap.example.invalid/missing")),
        "Failed to find matching request", class = "vcr_unhandled")
    }, dir = test_path("fixtures", "http-pilot"), record = "none",
      match_requests_on = c("method", "uri"), serialize_with = "json")
  })
})

test_that("fixture contexts restore the previous HTTP mock and vcr configuration", {
  skip_if_not_installed("vcr", minimum_version = "2.0.0")
  sentinel <- function(req) stop("Previous mock restored")
  request <- httr2::request("https://korap.example.invalid/probe")
  previousConfig <- vcr::vcr_configuration()
  httr2::with_mocked_responses(sentinel, {
    withKorapFixtureApi(function(state) suppressMessages(fixtureKorapConnection()))
    expect_error(httr2::req_perform(request), "Previous mock restored")
    expect_identical(vcr::vcr_configuration(), previousConfig)
    expect_error(withKorapFixtureApi(function(state) stop("Fixture cleanup probe")),
      "Fixture cleanup probe")
    expect_error(httr2::req_perform(request), "Previous mock restored")
    expect_identical(vcr::vcr_configuration(), previousConfig)
  })
})

test_that("missing fixtures fail even when apiCall handles the request error", {
  expect_error(suppressMessages(withKorapFixtureApi(function(state) {
    kco <- fixtureKorapConnection()
    apiCall(kco, paste0(kco@apiUrl, "unrecorded"), cache = FALSE)
  })), "Unmatched fixture request")
})

test_that("the real response cache stays under the temporary suite root", {
  root <- getOption("R.cache.rootPath")
  expect_true(startsWith(root, tempdir()))
  path <- withWorkflowFixture("cache", function(state) {
    subdirectory <- RKorAPClient:::KorAPCacheSubDir()
    expect_identical(subdirectory, basename(subdirectory))
    kco <- suppressMessages(fixtureKorapConnection(cache = TRUE))
    query <- korapHttpTestQuery(kco)
    path <- R.cache::getCachePath(dirs = subdirectory)
    expect_identical(normalizePath(dirname(path), winslash = "/"),
      normalizePath(root, winslash = "/"))
    expect_true(length(list.files(path, recursive = TRUE)) > 0)
    expect_equal(query@totalResults, 51)
    path
  })
  expect_false(dir.exists(path))
})

test_that("default metadata include current topic-domain fields", {
  withKorapFixtureApi(function(state) {
    checkKorapDefaultMetadata(suppressMessages(fixtureKorapConnection()))
  }, cassette = "default-metadata")
})
