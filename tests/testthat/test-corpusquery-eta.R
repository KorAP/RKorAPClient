test_that("corpusQuery displays ETA with multiple queries", {
  skip_if_offline()  # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)

  # Use simple queries to ensure they complete quickly
  query <- c("Test", "der")
  vc <- c("pubDate in 2020", "pubDate in 2021")

  # Capture output from corpusQuery with multiple queries
  temp_file <- tempfile()
  sink(temp_file)
  result <- corpusQuery(kco, query = query, vc = vc, metadataOnly = TRUE, as.df = TRUE, expand = TRUE)
  cat("\n")
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo the output to console for debugging
  cat("\nCaptured output from corpusQuery with multiple queries:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for all tests - strip ANSI color codes
  output_str <- paste(output, collapse = "\n")
  # Remove ANSI escape sequences
  output_str <- gsub("\\033\\[[0-9;]*m", "", output_str)

  # Test 1: Check that query progress is shown (format: "Query X/Y completed")
  expect_match(
    output_str,
    "Query \\d+/\\d+ completed",
    info = "Query progress counter not found in output"
  )

  # Test 2: Check that ETA is displayed (should contain digits followed by 's')
  expect_match(
    output_str,
    "ETA: \\d+s",
    info = "ETA format should show digits followed by 's'"
  )

  # Test 3: Check that completion time is shown
  expect_match(
    output_str,
    "\\(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\)",
    info = "Completion time format not found in output"
  )

  # Test 4: Check that we get results for all query combinations
  # Note: with expand=TRUE (default), we should get length(query) * length(vc) results
  expect_equal(nrow(result), length(query) * length(vc),
               info = paste("Should get results for all query/vc combinations. Got:", nrow(result), "Expected:", length(query) * length(vc)))
})

test_that("corpusQuery ETA works with frequencyQuery", {
  skip_if_offline()  # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)

  # Test the exact pattern from the user's example (but smaller)
  query <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")
  years <- c(2020:2021)  # Just 2 years for testing
  as.alternatives <- TRUE
  vc <- "textType = /Zeit.*/ & pubDate in"

  # Capture output from frequencyQuery which calls corpusQuery internally
  temp_file <- tempfile()
  sink(temp_file)
  result <- frequencyQuery(kco, query, paste(vc, years), as.alternatives = as.alternatives)
  cat("\n")
  sink()

  # Read the captured output
  output <- readLines(temp_file)
  unlink(temp_file)

  # Echo the output to console for debugging
  cat("\nCaptured output from frequencyQuery with ETA:\n")
  cat(paste(output, collapse = "\n"))

  # Combined output string for all tests - strip ANSI color codes
  output_str <- paste(output, collapse = "\n")
  # Remove ANSI escape sequences
  output_str <- gsub("\\033\\[[0-9;]*m", "", output_str)

  # Test 1: Check that multiple queries are processed (format: "Query X/Y completed")
  expect_match(
    output_str,
    "Query \\d+/\\d+ completed",
    info = "Query progress should be shown for multiple queries"
  )

  # Test 2: Check that ETA is displayed when processing multiple queries
  expect_match(
    output_str,
    "ETA:",
    info = "ETA should be displayed when processing multiple queries"
  )

  # Test 3: Check that we get results
  expect_true(nrow(result) > 0,
              info = "Should get frequency query results")

  # Test 4: Check that result has expected columns
  expect_true(all(c("query", "vc", "totalResults") %in% names(result)),
              info = "Result should contain expected columns")
})

test_that("corpusQuery ETA only displays with verbose=TRUE and multiple queries", {
  skip_if_offline()  # Commented out for testing

  # Test with verbose=FALSE - should not show ETA
  kco_quiet <- KorAPConnection(verbose = FALSE, cache = FALSE)
  query <- c("Test", "der")
  vc <- c("pubDate in 2020", "pubDate in 2021")

  # Capture output with verbose=FALSE
  temp_file <- tempfile()
  sink(temp_file)
  result1 <- corpusQuery(kco_quiet, query = query, vc = vc, metadataOnly = TRUE, as.df = TRUE)
  cat("\n")
  sink()

  output <- readLines(temp_file)
  unlink(temp_file)
  output_str <- paste(output, collapse = "\n")
  # Remove ANSI escape sequences
  output_str <- gsub("\\033\\[[0-9;]*m", "", output_str)

  # Should not contain ETA information when verbose=FALSE
  expect_false(grepl("ETA:", output_str),
               info = "ETA should not be displayed when verbose=FALSE")

  # Test with single query - should not show ETA even with verbose=TRUE
  kco_verbose <- KorAPConnection(verbose = TRUE, cache = FALSE)
  temp_file2 <- tempfile()
  sink(temp_file2)
  result2 <- corpusQuery(kco_verbose, query = "Test", vc = "pubDate in 2020",
                        metadataOnly = TRUE, as.df = TRUE)
  cat("\n")
  sink()

  output2 <- readLines(temp_file2)
  unlink(temp_file2)
  output_str2 <- paste(output2, collapse = "\n")
  # Remove ANSI escape sequences
  output_str2 <- gsub("\\033\\[[0-9;]*m", "", output_str2)

  # Should not contain ETA for single query
  expect_false(grepl("ETA:", output_str2),
               info = "ETA should not be displayed for single queries")
})

test_that("corpusQuery ETA format_duration function works correctly", {
  # This tests the internal format_duration function indirectly
  # by checking that ETA displays reasonable time formats
  skip_if_offline()  # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE)

  # Use multiple queries to trigger ETA display
  query <- c("Test", "der", "und")
  vc <- c("pubDate in 2020", "pubDate in 2021")

  # Capture output
  temp_file <- tempfile()
  sink(temp_file)
  result <- corpusQuery(kco, query = query, vc = vc, metadataOnly = TRUE, as.df = TRUE)
  cat("\n")
  sink()

  output <- readLines(temp_file)
  unlink(temp_file)
  output_str <- paste(output, collapse = "\n")
  # Remove ANSI escape sequences
  output_str <- gsub("\\033\\[[0-9;]*m", "", output_str)

  # Check that ETA contains reasonable time format (digits followed by 's')
  # This indirectly tests that format_duration is working
  expect_match(
    output_str,
    "ETA: \\d+s",
    info = "ETA should display time in seconds format"
  )

  # Also check for completion time format which uses the same function
  expect_match(
    output_str,
    "\\(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\)",
    info = "Completion time should be formatted correctly"
  )
})
