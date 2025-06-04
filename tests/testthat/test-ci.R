test_that("ci function works with basic input", {
  # Create a simple test data frame
  df <- data.frame(
    totalResults = c(100, 200, 50),
    total = c(1000, 2000, 500),
    query = c("test1", "test2", "test3")
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_true("f" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_equal(nrow(result), 3)

  # Check that relative frequencies are calculated correctly
  expect_equal(result$f[1], 0.1, tolerance = 0.001)
  expect_equal(result$f[2], 0.1, tolerance = 0.001)
  expect_equal(result$f[3], 0.1, tolerance = 0.001)
})

test_that("ci function handles custom column names", {
  # Test with custom column names
  df <- data.frame(
    observed = c(50, 100),
    N_total = c(500, 1000),
    condition = c("A", "B")
  )

  result <- ci(df, x = observed, N = N_total)

  expect_s3_class(result, "data.frame")
  expect_true("f" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$f[1], 0.1, tolerance = 0.001)
  expect_equal(result$f[2], 0.1, tolerance = 0.001)
})

test_that("ci function handles different confidence levels", {
  df <- data.frame(
    totalResults = c(100),
    total = c(1000)
  )

  # Test 90% confidence level
  result_90 <- ci(df, conf.level = 0.90)
  expect_s3_class(result_90, "data.frame")
  expect_true("f" %in% names(result_90))
  expect_true("conf.low" %in% names(result_90))
  expect_true("conf.high" %in% names(result_90))

  # Test 99% confidence level
  result_99 <- ci(df, conf.level = 0.99)
  expect_s3_class(result_99, "data.frame")

  # 99% CI should be wider than 90% CI
  ci_width_90 <- result_90$conf.high[1] - result_90$conf.low[1]
  ci_width_99 <- result_99$conf.high[1] - result_99$conf.low[1]
  expect_true(ci_width_99 > ci_width_90)
})

test_that("ci function handles zero and negative totals", {
  df <- data.frame(
    totalResults = c(10, 20, 30),
    total = c(100, 0, -10)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # First row should have valid values
  expect_false(is.na(result$f[1]))
  expect_false(is.na(result$conf.low[1]))
  expect_false(is.na(result$conf.high[1]))

  # Rows with zero or negative totals should have NA values
  expect_true(is.na(result$f[2]))
  expect_true(is.na(result$conf.low[2]))
  expect_true(is.na(result$conf.high[2]))
  expect_true(is.na(result$f[3]))
  expect_true(is.na(result$conf.low[3]))
  expect_true(is.na(result$conf.high[3]))
})

test_that("ci function handles NA values in totals", {
  df <- data.frame(
    totalResults = c(10, 20, 30),
    total = c(100, NA, 300)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # First and third rows should have valid values
  expect_false(is.na(result$f[1]))
  expect_false(is.na(result$f[3]))

  # Second row (with NA total) should have NA values
  expect_true(is.na(result$f[2]))
  expect_true(is.na(result$conf.low[2]))
  expect_true(is.na(result$conf.high[2]))
})

test_that("ci function handles edge cases with very small frequencies", {
  df <- data.frame(
    totalResults = c(1, 0),
    total = c(1000000, 1000000)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  # Check that very small frequencies are handled correctly
  expect_true(result$f[1] > 0)
  expect_true(result$f[1] < 0.01)
  expect_equal(result$f[2], 0)
})

test_that("ci function handles large numbers correctly", {
  df <- data.frame(
    totalResults = c(1000000),
    total = c(10000000)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$f[1], 0.1, tolerance = 0.001)
  expect_true(result$conf.low[1] > 0)
  expect_true(result$conf.high[1] < 1)
})

test_that("ci function preserves original columns", {
  df <- data.frame(
    totalResults = c(100, 200),
    total = c(1000, 2000),
    query = c("test1", "test2"),
    condition = c("A", "B"),
    year = c(2020, 2021)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_true("query" %in% names(result))
  expect_true("condition" %in% names(result))
  expect_true("year" %in% names(result))
  expect_true("totalResults" %in% names(result))
  expect_true("total" %in% names(result))

  # Check that original values are preserved
  expect_equal(result$query, c("test1", "test2"))
  expect_equal(result$condition, c("A", "B"))
  expect_equal(result$year, c(2020, 2021))
})

test_that("ci function handles empty data frame", {
  df <- data.frame(
    totalResults = numeric(0),
    total = numeric(0)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("f" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
})

test_that("ci function handles all zero totals", {
  df <- data.frame(
    totalResults = c(10, 20, 30),
    total = c(0, 0, 0)
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # All rows should have NA values
  expect_true(all(is.na(result$f)))
  expect_true(all(is.na(result$conf.low)))
  expect_true(all(is.na(result$conf.high)))
})

test_that("ci function validates confidence level parameter", {
  df <- data.frame(
    totalResults = c(100),
    total = c(1000)
  )

  # Test invalid confidence levels
  expect_error(ci(df, conf.level = 1.1))
  expect_error(ci(df, conf.level = 0))
  expect_error(ci(df, conf.level = -0.1))
})

test_that("ci function handles tibble input", {
  if (requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::tibble(
      totalResults = c(100, 200),
      total = c(1000, 2000),
      query = c("test1", "test2")
    )

    result <- ci(df)

    expect_s3_class(result, "tbl_df")
    expect_true("f" %in% names(result))
    expect_true("conf.low" %in% names(result))
    expect_true("conf.high" %in% names(result))
    expect_equal(nrow(result), 2)
  }
})

test_that("ci function confidence intervals are reasonable", {
  # Test with a known case
  df <- data.frame(
    totalResults = c(50),  # 50 out of 100 = 50%
    total = c(100)
  )

  result <- ci(df, conf.level = 0.95)

  expect_s3_class(result, "data.frame")
  expect_equal(result$f[1], 0.5, tolerance = 0.001)

  # For 50% with n=100, 95% CI should be roughly symmetric around 0.5
  expect_true(result$conf.low[1] < 0.5)
  expect_true(result$conf.high[1] > 0.5)

  # CI should be reasonable width (not too narrow or too wide)
  ci_width <- result$conf.high[1] - result$conf.low[1]
  expect_true(ci_width > 0.05)  # Not too narrow
  expect_true(ci_width < 0.5)   # Not too wide
})

test_that("ci function works with mixed valid and invalid data", {
  df <- data.frame(
    totalResults = c(100, 200, 50, 75),
    total = c(1000, 0, NA, 500),
    condition = c("A", "B", "C", "D")
  )

  result <- ci(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)

  # First and fourth rows should have valid values
  expect_false(is.na(result$f[1]))
  expect_false(is.na(result$f[4]))

  # Second and third rows should have NA values
  expect_true(is.na(result$f[2]))
  expect_true(is.na(result$f[3]))

  # Check that valid calculations are correct
  expect_equal(result$f[1], 0.1, tolerance = 0.001)
  expect_equal(result$f[4], 0.15, tolerance = 0.001)
})

test_that("ci function preserves row order with mixed valid/invalid data", {
  # Test data with alternating valid and invalid rows
  df <- data.frame(
    totalResults = c(100, 0, 200, NA, 50),
    total = c(1000, 0, 2000, 1500, 500),
    query = c("first", "second", "third", "fourth", "fifth"),
    stringsAsFactors = FALSE
  )

  result <- ci(df)

  # Check that the order is preserved
  expect_equal(result$query, c("first", "second", "third", "fourth", "fifth"))

  # Check that valid rows have computed values
  expect_false(is.na(result$f[1]))  # first row should have valid f
  expect_false(is.na(result$f[3]))  # third row should have valid f
  expect_false(is.na(result$f[5]))  # fifth row should have valid f

  # Check that invalid rows have NA values
  expect_true(is.na(result$f[2]))   # second row (total = 0)
  expect_true(is.na(result$f[4]))   # fourth row (total = NA)

  expect_true(is.na(result$conf.low[2]))
  expect_true(is.na(result$conf.high[2]))
  expect_true(is.na(result$conf.low[4]))
  expect_true(is.na(result$conf.high[4]))
})
