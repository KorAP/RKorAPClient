test_that("benchmark time is rounded in log output", {
  skip("Skipping test that requires network connection")

  # Capture output to check formatting
  temp_file <- tempfile()
  sink(temp_file)

  # Create connection and perform a query
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014")

  # End capturing
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo output for debugging
  cat("\nCaptured output from corpus query:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for testing
  output_str <- paste(output, collapse = "\n")

  # Test that the time display is properly rounded (should be in the format "X.XXs")
  # Looking for numbers with exactly 2 decimal places followed by 's'
  expect_match(
    output_str,
    "took \\d+\\.\\d{2}s",
    info = "Benchmark time should be rounded to 2 decimal places"
  )

  # Ensure the raw format with many decimal places is NOT present
  expect_false(
    grepl("took \\d+\\.\\d{6,}s", output_str),
    info = "Benchmark time should not have more than 2 decimal places"
  )
})

test_that("frequencyQuery shows rounded benchmark time", {
  skip("Skipping test that requires network connection")

  # Capture output to check formatting
  temp_file <- tempfile()
  sink(temp_file)

  # Create connection and perform a frequency query
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  result <- kco %>% frequencyQuery("Test", "pubDate since 2014")

  # End capturing
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo output for debugging
  cat("\nCaptured output from frequency query:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for testing
  output_str <- paste(output, collapse = "\n")

  # Test that the time display is properly rounded (should be in the format "X.XXs")
  expect_match(
    output_str,
    "took \\d+\\.\\d{2}s",
    info = "Benchmark time in frequencyQuery should be rounded to 2 decimal places"
  )

  # Ensure the raw format with many decimal places is NOT present
  expect_false(
    grepl("took \\d+\\.\\d{6,}s", output_str),
    info = "Benchmark time in frequencyQuery should not have more than 2 decimal places"
  )
})
