test_that("fetchNext works with randomizePageOrder", {
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    q <- corpusQuery(kco, "Test", "pubDate since 2014", fields = "textSigle")
    withr::local_seed(7)
    q <- fetchNext(q, maxFetch = 175, randomizePageOrder = TRUE)
    expect_equal(nrow(q@collectedMatches), 175)
    q <- fetchNext(q, maxFetch = 50, randomizePageOrder = TRUE)
    expect_equal(nrow(q@collectedMatches), 225)
    pages <- vapply(tail(state$requests, 5), function(url) {
      as.numeric(httr2::url_parse(url)$query$offset)
    }, numeric(1))
    expect_false(identical(pages, sort(pages)))
    expect_true(all(nzchar(q@collectedMatches$textSigle)))
  }, cassette = "fetch-random")
})
