test_that("textMetadata includes expected metadata fields", {
  skip_if_offline()
  kco <- RKorAPClient::KorAPConnection(accessToken = NULL)
  skip_if_offline()
  kco <- RKorAPClient::KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, "WUD17/B96/57558")

  expect_s3_class(m, "tbl_df")
  expect_true("textSigle" %in% names(m))
  expect_true("requestUrl" %in% names(m))
  expect_true("webUIRequestUrl" %in% names(m))
  expect_equal(m$textSigle[1], "WUD17/B96/57558")
  expect_true(nrow(m) == 1)
})

test_that("textMetadata works with multiple text sigles", {
  skip_if_offline()
  kco <- RKorAPClient::KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, c("WUD17/B96/57558", "WUD17/A97/08541"))

  expect_s3_class(m, "tbl_df")
  expect_true("textSigle" %in% names(m))
  expect_true(nrow(m) == 2)
  expect_true(all(c("WUD17/B96/57558", "WUD17/A97/08541") %in% m$textSigle))
})

test_that("textMetadata includes expected metadata columns", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, "WUD17/B96/57558")

  # Check for common metadata fields
  expect_true("textType" %in% names(m))
  expect_true("textSigle" %in% names(m))
  expect_true("requestUrl" %in% names(m))
  expect_true("webUIRequestUrl" %in% names(m))
})

test_that("textMetadata works for unknown text sigles", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, c("WUD17/B96/57558", "unknownsigle"))

  expect_true("errors" %in% names(m))
  expect_true(nrow(m) >= 1)

  # Test with only unknown sigle
  m_unknown <- textMetadata(kco, "completely/unknown/sigle")
  expect_true("errors" %in% names(m_unknown))
})

test_that("textMetadata works with list valued fields", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, "WUD17/B96/57558")

  expect_true("staat-gesellschaft\\tbiographien-interviews" == m$textClass[1] ||
              grepl("\\t", m$textClass[1]),
              "multiple text classes / domains should be tab separated")
})

test_that("textMetadata respects verbose parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)

  # Test with verbose = FALSE (default from connection)
  expect_silent(m1 <- textMetadata(kco, "WUD17/B96/57558"))

  # Test with explicit verbose = TRUE
  expect_output(m2 <- textMetadata(kco, "WUD17/B96/57558", verbose = TRUE),
                "Getting metadata")

  # Results should be the same regardless of verbosity
  expect_equal(m1$textSigle, m2$textSigle)
})

test_that("textMetadata handles URL encoding correctly", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)

  # Test with a text sigle that contains special characters
  m <- textMetadata(kco, "WUD17/B96/57558")

  expect_true(grepl("corpus/WUD17%2FB96%2F57558", m$requestUrl[1]))
  expect_true(grepl("textSigle", m$webUIRequestUrl[1]))
})

test_that("textMetadata returns proper data types", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, "WUD17/B96/57558")

  # All columns should be character type due to mutate(across(everything(), as.character))
  expect_true(all(sapply(m, is.character)))
})

test_that("textMetadata column ordering", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  m <- textMetadata(kco, "WUD17/B96/57558")

  # textSigle should be the first column due to relocate(textSigle)
  expect_equal(names(m)[1], "textSigle")
})

test_that("textMetadata handles special characters in sigle", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)

  # Test with valid sigle containing numbers and special format
  m <- textMetadata(kco, "WUD17/A97/08542")

  expect_true("textSigle" %in% names(m))
  expect_equal(m$textSigle[1], "WUD17/A97/08542")
})

test_that("textMetadata error handling for API failures", {
  # Mock a connection with invalid API URL to test error handling
  kco <- KorAPConnection(KorAPUrl = "https://invalid.url", accessToken = NULL)

  # This should handle API failures gracefully
  m <- textMetadata(kco, "WUD17/B96/57558")
  expect_true("errors" %in% names(m) || nrow(m) == 0)
})

test_that("textMetadata preserves request information", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  test_sigle <- "WUD17/B96/57558"
  m <- textMetadata(kco, test_sigle)

  expect_true("requestUrl" %in% names(m))
  expect_true("webUIRequestUrl" %in% names(m))
  expect_true(grepl(URLencode(test_sigle, reserved = TRUE), m$requestUrl[1]))
  expect_true(grepl("textSigle", m$webUIRequestUrl[1]))
})

test_that("textMetadata batch processing consistency", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)

  # Get metadata individually
  m1 <- textMetadata(kco, "WUD17/B96/57558")
  m2 <- textMetadata(kco, "WUD17/A97/08541")

  # Get metadata in batch
  m_batch <- textMetadata(kco, c("WUD17/B96/57558", "WUD17/A97/08541"))

  # Should have same number of rows
  expect_equal(nrow(m_batch), 2)

  # Should contain both sigles
  expect_true(all(c("WUD17/B96/57558", "WUD17/A97/08541") %in% m_batch$textSigle))
})
