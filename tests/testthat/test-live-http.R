test_that("an explicitly enabled live KorAP workflow remains compatible", {
  skip_if_not(identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true"),
    "Set RKORAP_LIVE_TESTS=true to run real KorAP integration tests")
  skip_on_cran()
  skip_if_offline()
  kco <- suppressMessages(KorAPConnection(accessToken = NULL, cache = FALSE, timeout = 20))
  skip_if(is.null(kco@welcome), "The live KorAP server is unavailable")
  q <- corpusQuery(kco, "Ameisenplage", fields = c("textSigle", "corpusTitle"))
  expect_true(is.finite(q@totalResults) && q@totalResults > 0)
  expect_null(q@apiResponse$meta$timeExceeded)
  page <- fetchNext(q, maxFetch = 5)
  expect_equal(nrow(page@collectedMatches), min(5, q@totalResults))
  expect_true(all(c("textSigle", "corpusTitle", "matchID") %in% names(page@collectedMatches)))
  stats <- corpusStats(kco, "pubDate since 2020 & pubDate until 2021")
  expect_true(all(is.finite(c(stats@tokens, stats@documents, stats@sentences, stats@paragraphs))))
  expect_gt(stats@tokens, 0)
  expect_gt(stats@documents, 0)
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
