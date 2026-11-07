liveKorapConnection <- function() {
  skip_if_not(identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true"),
    "Set RKORAP_LIVE_TESTS=true to run real KorAP integration tests")
  skip_on_cran()
  skip_if_offline()
  kco <- suppressMessages(KorAPConnection(accessToken = NULL, cache = FALSE, timeout = 20))
  skip_if(is.null(kco@welcome), "The live KorAP server is unavailable")
  kco
}

test_that("an explicitly enabled live KorAP workflow remains compatible", {
  kco <- liveKorapConnection()
  checkKorapHttpWorkflow(kco)
  checkKorapDefaultMetadata(kco)
})

# Keep a real-server check alongside the offline partial-page regressions.
test_that("an explicitly enabled live fetch respects successive maxFetch limits", {
  kco <- liveKorapConnection()
  q <- corpusQuery(kco, "Test", "pubDate since 2014", fields = "textSigle")
  q <- fetchNext(q, maxFetch = 75)
  expect_equal(nrow(q@collectedMatches), 75)
  q <- fetchNext(q, maxFetch = 100)
  expect_equal(nrow(q@collectedMatches), 175)
})

test_that("live query encodings and URL imports remain compatible", {
  checkKorapQueryEncodings(liveKorapConnection())
})

test_that("live collocation score queries remain compatible", {
  checkKorapCollocationScores(liveKorapConnection())
})

test_that("live public annotations and collocation analysis remain compatible", {
  checkKorapContentWorkflow(liveKorapConnection())
})
