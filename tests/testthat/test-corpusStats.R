test_that("corpusStats works", {
  skip_if_offline()
  stats <- KorAPConnection(accessToken = NULL) %>% corpusStats("pubDate since 2020 & pubDate until 2021")
  expect_gt(stats@tokens, 0)
  expect_gt(stats@paragraphs, 0)
  expect_gt(stats@documents, 0)
  expect(grepl("%26", stats@webUIRequestUrl), "webUIRequestUrl not properly url encoded")
})

test_that("corpusStats with result as df works", {
  skip_if_offline()
  stats <- KorAPConnection(accessToken = NULL) %>% corpusStats("pubDate since 2020 & pubDate until 2021", as.df = TRUE)
  expect_gt(stats$tokens, 0)
  expect_gt(stats$paragraphs, 0)
  expect_gt(stats$documents, 0)
  expect(grepl("%26", stats$webUIRequestUrl), "webUIRequestUrl not properly url encoded")
})

test_that("Printing corpusStats for the whole corpus works", {
  skip_if_offline()
  stats <- KorAPConnection(accessToken = NULL) %>% corpusStats()
  expect_error(print(stats), NA)
})

test_that("Printing corpusStats for a sub-corpus works", {
  skip_if_offline()
  stats <- KorAPConnection(accessToken = NULL) %>% corpusStats("pubDate since 2020 & pubDate until 2021")
  expect_error(print(stats), NA)
})
