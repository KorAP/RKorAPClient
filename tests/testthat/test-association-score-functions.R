test_that("association scores are calculated correctly", {
  x <- sapply(defaultAssociationScoreFunctions(), mapply, 4258869, 2165, 32, 21304641202, 4.327907, 10)
  expect_that(x[["ll"]], equals(73.05347, tolerance=0.01))
  expect_that(x[["pmi"]], equals(2.886331, tolerance=0.01))
  expect_that(x[["mi2"]], equals(7.886331, tolerance=0.01))
  expect_that(x[["mi3"]], equals(12.886331, tolerance=0.01))
  expect_that(x[["logDice"]], equals(-5.34404, tolerance=0.01))

  x <- sapply(defaultAssociationScoreFunctions(), mapply, 4258869, 2165, 0, 21304641202, 4.327907, 10)
  expect_that(x[["ll"]], equals(8.664477, tolerance=0.01))
  expect_equal(x[["pmi"]], -Inf)
  expect_equal(x[["mi2"]], -Inf)
  expect_equal(x[["mi3"]], -Inf)
  expect_equal(x[["logDice"]], -Inf)

})
