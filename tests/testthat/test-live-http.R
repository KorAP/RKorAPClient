test_that("an explicitly enabled live KorAP workflow remains compatible", {
  skip_if_not(identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true"),
    "Set RKORAP_LIVE_TESTS=true to run real KorAP integration tests")
  skip_on_cran()
  skip_if_offline()
  kco <- suppressMessages(KorAPConnection(accessToken = NULL, cache = FALSE, timeout = 20))
  skip_if(is.null(kco@welcome), "The live KorAP server is unavailable")
  checkKorapHttpWorkflow(kco)
})

# Keep the former live maxFetch regression explicitly available while its
# partial-page scenarios await the next fixture-migration step.
test_that("an explicitly enabled live fetch respects successive maxFetch limits", {
  skip_if_not(identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true"),
    "Set RKORAP_LIVE_TESTS=true to run real KorAP integration tests")
  skip_on_cran()
  skip_if_offline()
  kco <- suppressMessages(KorAPConnection(accessToken = NULL, cache = FALSE, timeout = 20))
  skip_if(is.null(kco@welcome), "The live KorAP server is unavailable")
  q <- corpusQuery(kco, "Test", "pubDate since 2014", fields = "textSigle")
  q <- fetchNext(q, maxFetch = 75)
  expect_equal(nrow(q@collectedMatches), 75)
  q <- fetchNext(q, maxFetch = 100)
  expect_equal(nrow(q@collectedMatches), 175)
})
