# Page numbering and ETA tests

test_that("page numbering is displayed correctly in sequential mode", {
  skip_if_offline()
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014", fields = c("sigle"))

  # Capture output - we need to use sink to capture the actual console output
  temp_file <- tempfile()
  sink(temp_file)
  q <- fetchNext(q, maxFetch = 75)
  cat("\n")
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo the output to console
  cat("\nCaptured output from sequential mode:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for all tests
  output_str <- paste(output, collapse = "\n")

  # Test 1: Check page numbering format
  expect_match(
    output_str,
    "Retrieved page .+/\\d+ \\(page \\d+ of \\d+ total\\)",
    info = "Page numbering format not found in output"
  )

  # Test 2: Check that ETA is displayed with time values (not "N/A")
  expect_match(
    output_str,
    "ETA: [^N][^/][^A]", # Negative pattern to ensure "N/A" is not in the ETA
    info = "ETA format is not correct or contains N/A"
  )

  # Test 3: Check that completion time is shown in parentheses
  expect_match(
    output_str,
    "\\(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\)",
    info = "Completion time format not found in output"
  )
})

test_that("page numbering and ETA are displayed correctly in randomized mode", {
  skip_if_offline()
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014", fields = c("sigle"))

  # Set a fixed seed for reproducible tests
  set.seed(123)

  # Capture output - we need to use sink to capture the actual console output
  temp_file <- tempfile()
  sink(temp_file)
  q <- fetchNext(q, maxFetch = 75, randomizePageOrder = TRUE)
  cat("\n")
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo the output to console
  cat("\nCaptured output from randomized mode:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for all tests
  output_str <- paste(output, collapse = "\n")

  # Test 1: Check page numbering format in randomized mode
  expect_match(
    output_str,
    "Retrieved page .+/\\d+ \\(actual page \\d+\\)",
    info = "Randomized page numbering format not found in output"
  )

  # Test 2: Check that ETA is displayed and doesn't contain "N/A (random order)"
  expect_match(
    output_str,
    "ETA: [^N][^/][^A]", # Ensure "N/A" is not in the ETA
    info = "ETA format is incorrect or contains N/A"
  )

  # Test 3: Check that proper time values and completion time are shown
  expect_match(
    output_str,
    "ETA: \\d+s \\(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\)",
    info = "Time format or completion time not found in output"
  )
})

test_that("page numbering and ETA are displayed correctly in subsequent calls with randomized mode", {
  skip_if_offline()
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)
  q <- kco %>% corpusQuery("Test", "pubDate since 2014", fields = c("sigle"))

  # Set a fixed seed for reproducible tests
  set.seed(123)

  # First call to fetchNext (we don't need to test this part)
  q <- fetchNext(q, maxFetch = 75, randomizePageOrder = TRUE)

  # Capture output from the subsequent call
  temp_file <- tempfile()
  sink(temp_file)
  q <- fetchNext(q, maxFetch = 50, randomizePageOrder = TRUE)
  cat("\n")
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo the output to console
  cat("\nCaptured output from subsequent call with randomized mode:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for all tests
  output_str <- paste(output, collapse = "\n")

  # Test 1: Check that page numbering format is correct and not negative
  expect_match(
    output_str,
    "Retrieved page [1-9]\\d*/\\d+ \\(actual page \\d+\\)",
    info = "Randomized page numbering format is incorrect or negative in subsequent call"
  )

  # Test 2: Check that ETA is displayed - we're now ensuring it contains digits followed by 's'
  expect_match(
    output_str,
    "ETA: \\d+s",
    info = "ETA format should show digits followed by 's'"
  )

  # Test 3: Check that completion time is shown in parentheses
  expect_match(
    output_str,
    "\\(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\)",
    info = "Completion time not found in subsequent call output"
  )
})
