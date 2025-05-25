test_that("fetchNext works with maxFetch", {
  skip_if_offline()
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014", fields = c("sigle"))
  q <- fetchNext(q, maxFetch = 75)
  expect_equal(nrow(q@collectedMatches), 75)
  cat("\n")
  q <- fetchNext(q, maxFetch = 100)
  expect_equal(nrow(q@collectedMatches), 175)
})

test_that("fetchNext works with randomizePageOrder", {
  skip_if_offline()
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014", fields = c("sigle"))
  q <- fetchNext(q, maxFetch = 175, randomizePageOrder = T)
  expect_equal(nrow(q@collectedMatches), 175)
  cat("\n")
  q <- fetchNext(q, maxFetch = 50, randomizePageOrder = T)
  expect_equal(nrow(q@collectedMatches), 225)
})

