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

test_that("parser covers full span across multiple <mark> blocks", {
  # Local, offline test to ensure correct match extraction when multiple
  # <mark>…</mark> segments occur within the match span.
  xml_snippet <- '<span class="context-left"></span>
  <span class="match">
    <span title="tt/l:Wir"><span title="tt/p:PPER">Wir</span></span>
    <mark>
      <span title="tt/l:können"><span title="tt/p:VVFIN">können</span></span>
    </mark>
    <span title="tt/l:alles"><span title="tt/p:PIS">alles</span></span>
    <mark>
      <span title="tt/l:außer"><span title="tt/p:APPR">außer</span></span>
      <span title="tt/l:Plan"><span title="tt/p:NN">Plan</span></span>
    </mark>
  </span>
  <span class="context-right"></span>'

  parsed <- parse_xml_annotations_structured(xml_snippet)

  # Left context contains the pre-mark token
  expect_equal(parsed$atokens$left, c("Wir"))

  # Match should include everything from the first <mark> to the last </mark>,
  # including tokens between them
  expect_equal(parsed$atokens$match, c("können", "alles", "außer", "Plan"))

  # POS and lemma lengths align with tokens in each section
  expect_length(parsed$pos$match, length(parsed$atokens$match))
  expect_length(parsed$lemma$match, length(parsed$atokens$match))
  expect_equal(parsed$pos$match, c("VVFIN", "PIS", "APPR", "NN"))
  expect_equal(parsed$lemma$match, c("können", "alles", "außer", "Plan"))

  # Right context should be empty in this snippet
  expect_length(parsed$atokens$right, 0)
})

test_that("parser keeps tokens separated across &nbsp; between spans", {
  xml_snippet <- '<span class="context-left"></span>
  <span class="match">
    <mark><span title="tt/l:können"><span title="tt/p:VVFIN">können</span></span></mark>&nbsp;<span title="tt/l:alles"><span title="tt/p:PIS">alles</span></span><mark><span title="tt/l:außer"><span title="tt/p:APPR">außer</span></span></mark>
  </span>
  <span class="context-right"></span>'

  parsed <- parse_xml_annotations_structured(xml_snippet)
  expect_equal(parsed$atokens$match, c("können", "alles", "außer"))
  expect_equal(parsed$pos$match, c("VVFIN", "PIS", "APPR"))
  expect_equal(parsed$lemma$match, c("können", "alles", "außer"))
})

test_that("parser keeps tokens separated across punctuation between spans", {
  xml_snippet <- '<span class="context-left"></span>
  <span class="match">
    <mark><span title="tt/l:können"><span title="tt/p:VVFIN">können</span></span></mark>, <span title="tt/l:alles"><span title="tt/p:PIS">alles</span></span><mark><span title="tt/l:außer"><span title="tt/p:APPR">außer</span></span></mark>
  </span>
  <span class="context-right"></span>'

  parsed <- parse_xml_annotations_structured(xml_snippet)
  expect_equal(parsed$atokens$match, c("können", "alles", "außer"))
  expect_equal(parsed$pos$match, c("VVFIN", "PIS", "APPR"))
  expect_equal(parsed$lemma$match, c("können", "alles", "außer"))
})

test_that("online: fetchAnnotations aligns pos/lemma with tokens for complex query", {
  skip_if_offline()

  kco <- KorAPConnection(verbose = FALSE, cache = FALSE, accessToken = NULL)
  q <- kco %>%
    corpusQuery('[orth="[wW]ir"] können alles [orth="ausser" | orth="außer"] [tt/pos=NN]',
                metadataOnly = FALSE,
                fields = c("textSigle", "snippet", "tokens")) %>%
    fetchNext(maxFetch = 20)

  skip_if(is.null(q@collectedMatches) || nrow(q@collectedMatches) == 0, "No matches found for online test")

  q2 <- fetchAnnotations(q, foundry = "tt", verbose = FALSE)

  # For each match, POS and lemma counts must equal token count in the match span
  for (i in seq_len(nrow(q2@collectedMatches))) {
    tt <- q2@collectedMatches$tokens$match[[i]]
    pp <- q2@collectedMatches$pos$match[[i]]
    ll <- q2@collectedMatches$lemma$match[[i]]
    expect_equal(length(tt), length(pp))
    expect_equal(length(tt), length(ll))
  }
})

test_that("fetchAnnotations aligns tokens and annotations across multiple <mark> blocks (stubbed API)", {
  # Define a minimal dummy KorAPConnection-like S4 class for offline testing
  setClass('DummyKCO', slots = c(apiUrl='character', verbose='logical'))
  setMethod('apiCall', 'DummyKCO', function(kco, url, json = TRUE, getHeaders = FALSE, cache = FALSE, timeout = 10) {
    list(snippet = '<span class="context-left"></span> <span class="match"> <span title="tt/l:Wir"><span title="tt/p:PPER">Wir</span></span> <mark> <span title="tt/l:können"><span title="tt/p:VVFIN">können</span></span> </mark> <span title="tt/l:alles"><span title="tt/p:PIS">alles</span></span> <mark> <span title="tt/l:außer"><span title="tt/p:APPR">außer</span></span> <span title="tt/l:Plan"><span title="tt/p:NN">Plan</span></span> </mark> </span> <span class="context-right"></span>')
  })

  # Build a minimal KorAPQuery with a dummy connection and a single match row
  kco <- new('DummyKCO', apiUrl = 'http://dummy/', verbose = FALSE)
  df <- data.frame(textSigle = 'A/B/C', matchStart = 1, matchEnd = 5, matchID = 'match-A/B/C-p1-5', stringsAsFactors = FALSE)
  q <- KorAPQuery(korapConnection = kco, collectedMatches = df)

  q2 <- fetchAnnotations(q, foundry = 'tt', verbose = FALSE)

  # Expect full match span to be covered
  expect_equal(q2@collectedMatches$atokens$left[[1]], c('Wir'))
  expect_equal(q2@collectedMatches$atokens$match[[1]], c('können','alles','außer','Plan'))
  expect_equal(q2@collectedMatches$pos$match[[1]], c('VVFIN','PIS','APPR','NN'))
  expect_equal(q2@collectedMatches$lemma$match[[1]], c('können','alles','außer','Plan'))

  # Alignment checks
  expect_length(q2@collectedMatches$pos$match[[1]], length(q2@collectedMatches$atokens$match[[1]]))
  expect_length(q2@collectedMatches$lemma$match[[1]], length(q2@collectedMatches$atokens$match[[1]]))
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

test_that("fetchAnnotations adds missing layer without overwriting existing, and can overwrite when requested", {
  # Define a separate dummy connection that serves different snippets by foundry
  if (!isClass("DummyKCO2")) setClass('DummyKCO2', slots = c(apiUrl='character', verbose='logical'))
  setMethod('apiCall', 'DummyKCO2', function(kco, url, json = TRUE, getHeaders = FALSE, cache = FALSE, timeout = 10) {
    # Return TT-only snippet by default, and TT+MarMoT morph when foundry=marmot
    tt_xml <- '<span class="context-left"></span>
  <span class="match">
    <mark><span title="tt/l:können tt/p:VVFIN">können</span></mark>&nbsp;<span title="tt/l:alles tt/p:PIS">alles</span><mark><span title="tt/l:außer tt/p:APPR">außer</span></mark>
  </span>
  <span class="context-right"></span>'
    marmot_xml <- '<span class="context-left"></span>
  <span class="match">
    <mark><span title="tt/l:können tt/p:VVFIN marmot/m:verbform:fin">können</span></mark>&nbsp;<span title="tt/l:alles tt/p:PIS marmot/m:pos:pron">alles</span><mark><span title="tt/l:außer tt/p:APPR marmot/m:pos:adp|case:acc">außer</span></mark>
  </span>
  <span class="context-right"></span>'
    if (grepl("foundry=marmot", url)) list(snippet = marmot_xml) else list(snippet = tt_xml)
  })

  # Build query with one match row
  kco <- new('DummyKCO2', apiUrl = 'http://dummy/', verbose = FALSE)
  df <- data.frame(textSigle = 'X/Y/Z', matchStart = 1, matchEnd = 3, matchID = 'match-X/Y/Z-p1-3', stringsAsFactors = FALSE)
  q <- KorAPQuery(korapConnection = kco, collectedMatches = df)

  # First call with TT: should populate pos/lemma, morph empty/NA
  q1 <- fetchAnnotations(q, foundry = 'tt', verbose = FALSE)
  pos_tt <- q1@collectedMatches$pos$match[[1]]
  lem_tt <- q1@collectedMatches$lemma$match[[1]]

  expect_equal(pos_tt, c('VVFIN','PIS','APPR'))
  expect_equal(lem_tt, c('können','alles','außer'))
  # Morph should be empty or NA-only at this point
  morph1 <- q1@collectedMatches$morph$match[[1]]
  expect_true(length(morph1) == 0 || all(is.na(morph1)))

  # Second call with marmot: should add morph but keep pos/lemma unchanged when overwrite=FALSE
  q2 <- fetchAnnotations(q1, foundry = 'marmot', verbose = FALSE)
  expect_equal(q2@collectedMatches$pos$match[[1]], pos_tt)
  expect_equal(q2@collectedMatches$lemma$match[[1]], lem_tt)

  morph2 <- q2@collectedMatches$morph$match[[1]]
  expect_equal(morph2, c('verbform:fin','pos:pron','pos:adp|case:acc'))

  # Corrupt existing POS and ensure overwrite=TRUE repairs it
  q2@collectedMatches$pos$match[[1]][1] <- 'DAMAGED'
  q3 <- fetchAnnotations(q2, foundry = 'tt', overwrite = TRUE, verbose = FALSE)
  expect_equal(q3@collectedMatches$pos$match[[1]][1], 'VVFIN')
})
