test_that("real HTTP retries recover from a local rate limit without long waits", {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  skip_if_not_installed("callr")
  app <- webfakes::new_app()
  app$locals$attempts <- 0L
  app$get("/api/v1.0/", function(req, res) {
    res$set_header("x-index-revision", "local-index-1")
    res$send("KorAP local HTTP fixture")
  })
  app$get("/settings/oauth/authorize", function(req, res) res$send("Authorization supported"))
  app$get("/api/v1.0/search", function(req, res) {
    app$locals$attempts <- app$locals$attempts + 1L
    if (app$locals$attempts < 3L) {
      res$set_status(429L)
      res$set_header("Retry-After", "0")
      res$send("Rate limit exceeded")
    } else {
      res$send_json(list(meta = list(totalResults = 7L, attempts = app$locals$attempts)))
    }
  })
  server <- webfakes::local_app_process(app)
  # No DNS reachability probe or dependency on the real internet is needed.
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  # Exercise httr2's actual retry loop, but use no wall-clock backoff in tests.
  local_mocked_bindings(retryAfterSeconds = function(...) 0, .package = "RKorAPClient")
  kco <- suppressMessages(KorAPConnection(KorAPUrl = server$url(), accessToken = NULL,
    cache = FALSE, verbose = FALSE))
  expect_true(kco@authorizationSupported)
  q <- corpusQuery(kco, "word")
  expect_equal(q@totalResults, 7)
  expect_equal(q@apiResponse$meta$attempts, 3)
})

test_that("a local slow response triggers the actual request timeout gracefully", {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  skip_if_not_installed("callr")
  skip_if_not_installed("vcr", minimum_version = "2.0.0")
  app <- webfakes::new_app()
  app$get("/slow", function(req, res) {
    Sys.sleep(2)
    res$send("too late")
  })
  server <- webfakes::local_app_process(app)
  # Only the initial connection comes from files; the slow transfer below uses
  # real httr2/curl against the local server, after the mock context has ended.
  kco <- withKorapFixtureApi(function(state) suppressMessages(fixtureKorapConnection()))
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  expect_message(result <- apiCall(kco, server$url("/slow"), cache = FALSE, timeout = .1),
    "Timeout|timed out", ignore.case = TRUE)
  expect_null(result)
})
