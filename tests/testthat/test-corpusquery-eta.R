test_that("corpusQuery displays ETA with multiple queries", {
  skip_if_offline() # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE, accessToken = NULL)

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
  # Remove ANSI escape sequences - the output seems to contain escaped sequences
  output_str <- gsub("\\\\033\\[[0-9;]*[a-zA-Z]", "", output_str)
  output_str <- gsub("\\033\\[[0-9;]*[a-zA-Z]", "", output_str)
  # Also remove literal ANSI sequences if they exist
  output_str <- gsub("\\\033\\[[0-9;]*[a-zA-Z]", "", output_str)

  # Test 1: Check that one progress row per query is shown, with its hits
  expect_match(output_str, "Searching 4 queries", info = "Header not found in output")
  expect_match(output_str, "\\[1/4\\]  Test\\s+.*[0-9,]+ hits", info = "Progress row not found in output")

  # the remaining time is left out when it would be under a second, as it is
  # where the server answers from its cache, so it is tested offline below

  # Test 3: Check that the summary is shown
  expect_match(output_str, "4 queries in \\d+s", info = "Summary not found in output")

  # Test 4: Check that we get results for all query combinations
  # Note: with expand=TRUE (default), we should get length(query) * length(vc) results
  expect_equal(nrow(result), length(query) * length(vc),
    info = paste("Should get results for all query/vc combinations. Got:", nrow(result), "Expected:", length(query) * length(vc))
  )
})

test_that("corpusQuery ETA works with frequencyQuery", {
  skip_if_offline() # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE, accessToken = NULL)

  # Test the exact pattern from the user's example (but smaller)
  query <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")
  years <- c(2020:2021) # Just 2 years for testing
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
  # Remove ANSI escape sequences - the output seems to contain escaped sequences
  output_str <- gsub("\\\\033\\[[0-9;]*[a-zA-Z]", "", output_str)
  output_str <- gsub("\\033\\[[0-9;]*[a-zA-Z]", "", output_str)
  # Also remove literal ANSI sequences if they exist
  output_str <- gsub("\\\033\\[[0-9;]*[a-zA-Z]", "", output_str)

  # Test 1: Check that multiple search queries are processed
  expect_match(output_str, "\\[\\d+/\\d+\\].*[0-9,]+ hits", info = "Search results should be shown for multiple queries")


  # Test 3: Check that we get results
  expect_true(nrow(result) > 0,
    info = "Should get frequency query results"
  )

  # Test 4: Check that result has expected columns
  expect_true(all(c("query", "vc", "totalResults") %in% names(result)),
    info = "Result should contain expected columns"
  )
})

test_that("corpusQuery ETA only displays with verbose=TRUE and multiple queries", {
  skip_if_offline() # Commented out for testing

  # Test with verbose=FALSE - should not show ETA
  kco_quiet <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
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
  expect_equal(trimws(output_str), "", info = "Nothing should be displayed when verbose=FALSE")

  # Test with single query - should not show ETA even with verbose=TRUE
  kco_verbose <- KorAPConnection(verbose = TRUE, cache = FALSE, accessToken = NULL)
  temp_file2 <- tempfile()
  sink(temp_file2)
  result2 <- corpusQuery(kco_verbose,
    query = "Test", vc = "pubDate in 2020",
    metadataOnly = TRUE, as.df = TRUE
  )
  cat("\n")
  sink()

  output2 <- readLines(temp_file2)
  unlink(temp_file2)
  output_str2 <- paste(output2, collapse = "\n")
  # Remove ANSI escape sequences
  output_str2 <- gsub("\\033\\[[0-9;]*m", "", output_str2)

  # Should not contain ETA for single query
  expect_false(grepl("left", output_str2),
    info = "The remaining time should not be displayed for single queries"
  )
  expect_match(output_str2, "Searching \"Test\" in \"pubDate in 2020\": [0-9,]+ hits")
})

test_that("progress rows are aligned, and without colour when not on a terminal", {
  layout <- progress_layout(12, c("Hund", "Katze"), c("\u2026 2010", "\u2026 2011"))
  out <- capture.output({
    progress_row_start(TRUE, layout, 3, "Hund", "\u2026 2010")
    progress_row_end(TRUE, "31,883 hits", 4.83, "ok", "~18s left")
    progress_row_start(TRUE, layout, 10, "Katze", "\u2026 2011")
    progress_row_end(TRUE, "failed", 0.2, "failed")
    progress_summary(TRUE, "queries", c("ok", "cached", "failed"), 21)
  })
  expect_false(any(grepl("\033", out, fixed = TRUE)))
  expect_equal(out[1], "  [ 3/12]  Hund   \u2026 2010         31,883 hits    4.8s  ~18s left")
  expect_equal(out[2], "  [10/12]  Katze  \u2026 2011              failed    0.2s")
  expect_match(out[3], "3 queries in 21s \\(1 cached, 1 failed\\)$")
  expect_equal(capture.output(progress_row_start(FALSE, layout, 1, "Hund", "x")), character(0))
})

test_that("remaining time is estimated from the finished, uncached items", {
  expect_equal(format_remaining(c(4, 6), c(FALSE, FALSE), 6), "~20s left")
  # items served from the cache do not count, neither as time nor as a sample
  expect_equal(format_remaining(c(0.01, 4), c(TRUE, FALSE), 4), "~8s left")
  expect_equal(format_remaining(c(0.01), TRUE, 4), "")
  expect_equal(format_remaining(c(0.2), FALSE, 3), "")
  expect_equal(format_remaining(c(4, 6), c(FALSE, FALSE), 2), "")
  expect_match(format_remaining(700, FALSE, 2), "^~11m 40s left \\(until \\d{2}:\\d{2}\\)$")
})

test_that("durations and counts are formatted compactly", {
  expect_equal(format_duration_short(4.83, precise = TRUE), "4.8s")
  expect_equal(format_duration_short(18.2), "18s")
  expect_equal(format_duration_short(185), "3m 05s")
  expect_equal(format_duration_short(3720), "1h 02m")
  expect_equal(format_duration_short(NA), "?")
  expect_equal(format_count(c(12, 31883, 31469333310)), c("12", "31,883", "31,469,333,310"))
})

test_that("labels are shortened in the middle, and a shared beginning is factored out", {
  expect_equal(truncate_display("textType = /Zeit.*/ & pubDate in 2010", 13), "textTy\u2026n 2010")
  expect_equal(truncate_display("short", 13), "short")

  f <- factor_common_prefix(paste("textType = /Zeit.*/ & pubDate in", 2010:2011))
  expect_equal(f$prefix, "textType = /Zeit.*/ & pubDate in \u2026")
  expect_equal(f$labels, c("\u20262010", "\u20262011"))
  # the prefix is cut back to a word boundary
  expect_equal(
    factor_common_prefix(c("textType = /Zeitung.*/", "textType = /Zeitschrift.*/"))$prefix,
    "textType = \u2026"
  )
  # short labels, too short a prefix, or nothing to tell apart are left alone
  expect_equal(factor_common_prefix(c("pubDate in 2010", "pubDate in 2011"))$prefix, "")
  # nor where it would leave long labels that are better read in full
  expect_equal(factor_common_prefix(c("pubDate in 2020", paste("pubDate in 2020 &", strrep("x", 40))))$prefix, "")
  expect_equal(factor_common_prefix(paste0(c("Zeitung", "Zeitschrift"), strrep("x", 20)))$prefix, "")
  expect_equal(factor_common_prefix(c("pubDate in 2010", "pubDate in 2010"))$prefix, "")
})

test_that("corpusQuery ETA format_duration function works correctly", {
  # This tests the internal format_duration function indirectly
  # by checking that ETA displays reasonable time formats
  skip_if_offline() # Commented out for testing
  kco <- KorAPConnection(verbose = TRUE, cache = FALSE, accessToken = NULL)

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

  # Check that the total is formatted as a duration
  expect_match(output_str, "6 queries in \\d+s", info = "Summary should show the total time")
})
