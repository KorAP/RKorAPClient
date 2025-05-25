test_that("benchmark time formatting function works correctly", {
  # Create a mock environment to test the formatting function
  format_benchmark_time <- function(time_string) {
    if (is.character(time_string) && grepl("s$", time_string)) {
      time_value <- as.numeric(sub("s$", "", time_string))
      paste0(round(time_value, 2), "s")
    } else {
      time_string
    }
  }

  # Test with various inputs
  expect_equal(format_benchmark_time("3.395072759s"), "3.4s")
  expect_equal(format_benchmark_time("0.123456s"), "0.12s")
  expect_equal(format_benchmark_time("1.999s"), "2s")
  expect_equal(format_benchmark_time("0.001s"), "0s")

  # Test with non-matching inputs
  expect_equal(format_benchmark_time("invalid"), "invalid")
  expect_equal(format_benchmark_time(NULL), NULL)
  expect_equal(format_benchmark_time(123), 123)
})
