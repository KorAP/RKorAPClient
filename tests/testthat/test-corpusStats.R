test_that("corpusStats works", {
  stats <- new("KorAPConnection") %>% corpusStats()
  expect_gt(stats@tokens, 0)
  expect_gt(stats@paragraphs, 0)
  expect_gt(stats@documents, 0)
})


test_that("Printing corpusStats for the whole corpus works", {
  stats <- new("KorAPConnection") %>% corpusStats()
  expect_error(print(stats), NA)
})

test_that("Printing corpusStats for a sub-corpus works", {
  stats <- new("KorAPConnection") %>% corpusStats("pubDate in 2018")
  expect_error(print(stats), NA)
})
