test_that("association scores are calculated correctly", {
  x <- sapply(defaultAssociationScoreFunctions(), mapply, 4258869, 2165, 32, 21304641202, 4.327907, 10)
  expect_that(x[["ll"]], equals(73.05347, tolerance=0.01))
  expect_that(x[["pmi"]], equals(2.886331, tolerance=0.01))
  expect_that(x[["mi2"]], equals(7.886331, tolerance=0.01))
  expect_that(x[["mi3"]], equals(12.886331, tolerance=0.01))
  # logDice follows Rychly (2008): 14 + log2(2 * O / (O1 + O2)), without a
  # window size factor, so that values are comparable to other tools
  expect_that(x[["logDice"]], equals(-2.022772, tolerance=0.01))

  x <- sapply(defaultAssociationScoreFunctions(), mapply, 4258869, 2165, 0, 21304641202, 4.327907, 10)
  expect_that(x[["ll"]], equals(8.664477, tolerance=0.01))
  expect_equal(x[["pmi"]], -Inf)
  expect_equal(x[["mi2"]], -Inf)
  expect_equal(x[["mi3"]], -Inf)
  expect_equal(x[["logDice"]], -Inf)

})

test_that("filterByObservedExpectedRatio keeps only attested enough collocates", {
  filterBy <- RKorAPClient:::filterByObservedExpectedRatio
  result <- tibble::tibble(
    collocate = c("attracted", "asExpected", "repelled"),
    O = c(100, 10, 1),
    E = c(10, 10, 10)
  )

  expect_equal(filterBy(result, 1)$collocate, c("attracted", "asExpected"))
  expect_equal(filterBy(result, 5)$collocate, "attracted")
  # 0 and NULL switch the filter off, for studying repulsion for instance
  expect_equal(nrow(filterBy(result, 0)), 3)
  expect_equal(nrow(filterBy(result, NULL)), 3)

  # rows without an expected frequency are kept rather than silently dropped
  withNA <- tibble::tibble(collocate = "unknown", O = 1, E = NA_real_)
  expect_equal(nrow(filterBy(withNA, 1)), 1)

  # nothing to do without the columns, or without rows
  expect_equal(nrow(filterBy(tibble::tibble(collocate = "x"), 1)), 1)
  expect_equal(nrow(filterBy(result[0, ], 1)), 0)
})
