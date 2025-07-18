test_that("fetchAnnotations works with valid matches", {
  skip_if_offline()

  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    corpusQuery("Test", "pubDate since 2014", metadataOnly = FALSE, fields = c("textSigle", "snippet")) %>%
    fetchNext(maxFetch = 2)

  # Skip test if no matches found
  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for test query")

  # Test that structured annotation columns are initially absent
  expect_false("atokens" %in% colnames(q@collectedMatches))
  expect_false("pos" %in% colnames(q@collectedMatches))

  # Test that matchID is preserved in collectedMatches
  expect_true("matchID" %in% colnames(q@collectedMatches))
  expect_true(all(!is.na(q@collectedMatches$matchID)))

  # Test fetchAnnotations with default foundry
  q_with_annotations <- fetchAnnotations(q, verbose = FALSE)

  # Check that structured annotation columns are now populated
  expect_true("atokens" %in% colnames(q_with_annotations@collectedMatches))
  expect_true("pos" %in% colnames(q_with_annotations@collectedMatches))

  # Check that the structured columns have left/match/right components
  expect_true(all(c("left", "match", "right") %in% names(q_with_annotations@collectedMatches$atokens)))
  expect_true(all(c("left", "match", "right") %in% names(q_with_annotations@collectedMatches$pos)))

  # Test fetchAnnotations with specific foundry
  q_with_tt <- fetchAnnotations(q, foundry = "tt", verbose = FALSE)
  expect_true("atokens" %in% colnames(q_with_tt@collectedMatches))
  expect_true("pos" %in% colnames(q_with_tt@collectedMatches))

  # Test that annotations contain actual content (regression test for URL construction)
  if (nrow(q_with_tt@collectedMatches) > 0) {
    # Check that the first match has populated annotation data
    expect_true(length(q_with_tt@collectedMatches$atokens$left[[1]]) > 0 ||
      length(q_with_tt@collectedMatches$atokens$match[[1]]) > 0 ||
      length(q_with_tt@collectedMatches$atokens$right[[1]]) > 0)
    expect_true(length(q_with_tt@collectedMatches$pos$left[[1]]) > 0 ||
      length(q_with_tt@collectedMatches$pos$match[[1]]) > 0 ||
      length(q_with_tt@collectedMatches$pos$right[[1]]) > 0)
  }
})

test_that("fetchAnnotations handles empty matches gracefully", {
  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)

  # Create a query object with no collected matches
  q <- KorAPQuery(
    korapConnection = kco,
    collectedMatches = NULL
  )

  # Should warn and return original object
  expect_warning(
    result <- fetchAnnotations(q, verbose = FALSE),
    "No collected matches found"
  )
  expect_identical(result, q)
})

test_that("fetchAnnotations preserves original object structure", {
  skip_if_offline()

  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    corpusQuery("Test", "pubDate since 2014", metadataOnly = FALSE, fields = c("textSigle", "snippet")) %>%
    fetchNext(maxFetch = 1)

  # Skip test if no matches found
  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for test query")

  q_original <- q
  q_with_annotations <- fetchAnnotations(q, verbose = FALSE)

  # Check that all original slots are preserved
  expect_identical(q_with_annotations@korapConnection, q_original@korapConnection)
  expect_identical(q_with_annotations@request, q_original@request)
  expect_identical(q_with_annotations@vc, q_original@vc)
  expect_identical(q_with_annotations@totalResults, q_original@totalResults)

  # collectedMatches should have additional annotation columns
  expect_true(nrow(q_with_annotations@collectedMatches) == nrow(q_original@collectedMatches))
  expect_true(ncol(q_with_annotations@collectedMatches) > ncol(q_original@collectedMatches))

  # Original columns should be preserved
  original_cols <- colnames(q_original@collectedMatches)
  expect_true(all(original_cols %in% colnames(q_with_annotations@collectedMatches)))

  # New annotation columns should be present
  expect_true("atokens" %in% colnames(q_with_annotations@collectedMatches))
  expect_true("pos" %in% colnames(q_with_annotations@collectedMatches))
})

test_that("fetchAnnotations returns structured left/match/right format", {
  skip_if_offline()

  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    corpusQuery("Test", "pubDate since 2014", metadataOnly = FALSE, fields = c("textSigle", "snippet")) %>%
    fetchNext(maxFetch = 1)

  # Skip test if no matches found
  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for test query")

  q_with_annotations <- fetchAnnotations(q, foundry = "tt", verbose = FALSE)

  # Test that structured annotation columns exist
  expect_true("atokens" %in% colnames(q_with_annotations@collectedMatches))
  expect_true("pos" %in% colnames(q_with_annotations@collectedMatches))

  # Test the structure of annotation columns
  atokens <- q_with_annotations@collectedMatches$atokens
  pos <- q_with_annotations@collectedMatches$pos

  expect_true(is.data.frame(atokens))
  expect_true(is.data.frame(pos))

  expect_true(all(c("left", "match", "right") %in% names(atokens)))
  expect_true(all(c("left", "match", "right") %in% names(pos)))

  # Test that each component is a list column
  expect_true(is.list(atokens$left))
  expect_true(is.list(atokens$match))
  expect_true(is.list(atokens$right))
  expect_true(is.list(pos$left))
  expect_true(is.list(pos$match))
  expect_true(is.list(pos$right))

  # Test that the first match has actual data
  if (nrow(q_with_annotations@collectedMatches) > 0) {
    # At least one of left/match/right should have content
    total_tokens <- length(atokens$left[[1]]) + length(atokens$match[[1]]) + length(atokens$right[[1]])
    expect_true(total_tokens > 0)

    total_pos <- length(pos$left[[1]]) + length(pos$match[[1]]) + length(pos$right[[1]])
    expect_true(total_pos > 0)

    # Token count should match POS count
    expect_equal(total_tokens, total_pos)

    # Match tokens should not be empty (since we found a match)
    expect_true(length(atokens$match[[1]]) > 0)
    expect_true(length(pos$match[[1]]) > 0)
  }
})

test_that("matchID is preserved in collectedMatches", {
  skip_if_offline()

  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    corpusQuery("Test", "pubDate since 2014", metadataOnly = FALSE, fields = c("textSigle", "snippet")) %>%
    fetchNext(maxFetch = 1)

  # Skip test if no matches found
  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for test query")

  # Check that matchID is present and valid
  expect_true("matchID" %in% colnames(q@collectedMatches))
  expect_true(all(!is.na(q@collectedMatches$matchID)))

  # Verify matchID format (should contain position information)
  expect_true(all(grepl("-p\\d+-\\d+", q@collectedMatches$matchID)))

  # Verify that matchStart and matchEnd are correctly extracted from matchID
  for (i in seq_len(nrow(q@collectedMatches))) {
    match_id <- q@collectedMatches$matchID[i]
    positions <- gsub(".*-p(\\d+)-(\\d+).*", "\\1 \\2", match_id)
    expected_start <- as.integer(stringr::word(positions, 1))
    expected_end <- as.integer(stringr::word(positions, 2)) - 1

    expect_equal(q@collectedMatches$matchStart[i], expected_start)
    expect_equal(q@collectedMatches$matchEnd[i], expected_end)
  }
})

test_that("fetchAnnotations handles morphological annotations with pipe separators", {
  skip_if_offline()

  kco <- KorAPConnection("https://korap.dnb.de", verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    auth() %>%
    corpusQuery("Ameisenplage", metadataOnly = FALSE) %>%
    fetchNext(maxFetch = 1)

  # Skip test if no matches found
  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for test query")

  # Test with marmot foundry which provides morphological annotations
  q_with_morph <- fetchAnnotations(q, foundry = "marmot", verbose = FALSE)

  # Check that morphological annotation columns exist
  expect_true("morph" %in% colnames(q_with_morph@collectedMatches))
  expect_true("atokens" %in% colnames(q_with_morph@collectedMatches))

  # Test the structure of morphological annotation columns
  morph <- q_with_morph@collectedMatches$morph
  expect_true(is.data.frame(morph))
  expect_true(all(c("left", "match", "right") %in% names(morph)))
  expect_true(is.list(morph$match))

  # Test that morphological features use pipe separators
  if (nrow(q_with_morph@collectedMatches) > 0) {
    morph_data <- morph$match[[1]]

    # Check that we have morphological data
    expect_true(length(morph_data) > 0)

    # If morphological data exists and is not NA, it should contain pipe separators
    # for multiple features (e.g., "case:acc|gender:fem|number:sg")
    if (!is.na(morph_data[1]) && nchar(morph_data[1]) > 0) {
      # Should contain morphological features separated by pipes
      expect_true(grepl("^[^|]+", morph_data[1])) # At least one feature

      # If multiple features exist, they should be pipe-separated
      if (grepl("\\|", morph_data[1])) {
        features <- unlist(strsplit(morph_data[1], "\\|"))
        expect_true(length(features) > 1)
        # Each feature should follow the pattern "attribute:value"
        expect_true(all(grepl("^[^:]+:[^:]+$", features)))
      }
    }
  }
})
