test_that("textMetadata includes expected metadata fields", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, "TEST/2023/00001")

    expect_s3_class(m, "tbl_df")
    expect_true("textSigle" %in% names(m))
    expect_true("requestUrl" %in% names(m))
    expect_true("webUIRequestUrl" %in% names(m))
    expect_equal(m$textSigle[1], "TEST/2023/00001")
    expect_true(nrow(m) == 1)
  }, cassette = "metadata-single")
})

test_that("textMetadata works with multiple text sigles", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, c("TEST/2023/00001", "TEST/2023/00002"))

    expect_s3_class(m, "tbl_df")
    expect_true("textSigle" %in% names(m))
    expect_true(nrow(m) == 2)
    expect_true(all(c("TEST/2023/00001", "TEST/2023/00002") %in% m$textSigle))
  }, cassette = "metadata-two")
})

test_that("textMetadata includes expected metadata columns", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, "TEST/2023/00001")

    # Check for common metadata fields
    expect_true("textType" %in% names(m))
    expect_true("textSigle" %in% names(m))
    expect_true("requestUrl" %in% names(m))
    expect_true("webUIRequestUrl" %in% names(m))
  }, cassette = "metadata-single")
})

test_that("textMetadata works for unknown text sigles", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, c("TEST/2023/00001", "unknownsigle"))

    expect_true("errors" %in% names(m))
    expect_true(nrow(m) >= 1)

    # Test with only unknown sigle
    m_unknown <- textMetadata(kco, "completely/unknown/sigle")
    expect_true("errors" %in% names(m_unknown))
  }, cassette = "metadata-errors")
})

test_that("textMetadata works with list valued fields", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, "TEST/2023/00001")

    # DeReKo-2026 renamed textClass to dmozDomain (list valued); list
    # valued fields are flattened tab separated
    expect_true("staat-gesellschaft\\tbiographien-interviews" == m$dmozDomain[1] ||
                grepl("\\t", m$dmozDomain[1]),
                "multiple text classes / domains should be tab separated")
  }, cassette = "metadata-single")
})

test_that("textMetadata respects verbose parameter", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())

    # Test with verbose = FALSE (default from connection)
    expect_silent(m1 <- textMetadata(kco, "TEST/2023/00001"))

    # Test with explicit verbose = TRUE
    expect_output(m2 <- textMetadata(kco, "TEST/2023/00001", verbose = TRUE),
                  "Getting metadata")

    # Results should be the same regardless of verbosity
    expect_equal(m1$textSigle, m2$textSigle)
  }, cassette = "metadata-verbose")
})

test_that("textMetadata handles URL encoding correctly", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())

    # Test with a text sigle that contains special characters
    m <- textMetadata(kco, "TEST/2023/00001")

    expect_true(grepl("corpus/TEST%2F2023%2F00001", m$requestUrl[1]))
    expect_true(grepl("textSigle", m$webUIRequestUrl[1]))
  }, cassette = "metadata-single")
})

test_that("textMetadata returns proper data types", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, "TEST/2023/00001")

    # All columns should be character type due to mutate(across(everything(), as.character))
    expect_true(all(sapply(m, is.character)))
  }, cassette = "metadata-single")
})

test_that("textMetadata column ordering", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    m <- textMetadata(kco, "TEST/2023/00001")

    # textSigle should be the first column due to relocate(textSigle)
    expect_equal(names(m)[1], "textSigle")
  }, cassette = "metadata-single")
})

test_that("textMetadata handles special characters in sigle", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())

    # Test with valid sigle containing numbers and special format
    m <- textMetadata(kco, "TEST/2023/00003")

    expect_true("textSigle" %in% names(m))
    expect_equal(m$textSigle[1], "TEST/2023/00003")
  }, cassette = "metadata-special")
})

test_that("textMetadata error handling for API failures", {
  withKorapFixtureApi(function(state) {
    # Mock a connection with invalid API URL to test error handling
    kco <- suppressMessages(fixtureKorapConnection())

    # This should handle API failures gracefully
    local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
    m <- textMetadata(kco, "TEST/2023/00001")
    expect_true("errors" %in% names(m) || nrow(m) == 0)
  }, cassette = "metadata-single")
})

test_that("textMetadata preserves request information", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())
    test_sigle <- "TEST/2023/00001"
    m <- textMetadata(kco, test_sigle)

    expect_true("requestUrl" %in% names(m))
    expect_true("webUIRequestUrl" %in% names(m))
    expect_true(grepl(URLencode(test_sigle, reserved = TRUE), m$requestUrl[1]))
    expect_true(grepl("textSigle", m$webUIRequestUrl[1]))
  }, cassette = "metadata-single")
})

test_that("textMetadata batch processing consistency", {
  withKorapFixtureApi(function(state) {

    kco <- suppressMessages(fixtureKorapConnection())

    # Get metadata individually
    m1 <- textMetadata(kco, "TEST/2023/00001")
    m2 <- textMetadata(kco, "TEST/2023/00002")

    # Get metadata in batch
    m_batch <- textMetadata(kco, c("TEST/2023/00001", "TEST/2023/00002"))

    # Should have same number of rows
    expect_equal(nrow(m_batch), 2)

    # Should contain both sigles
    expect_true(all(c("TEST/2023/00001", "TEST/2023/00002") %in% m_batch$textSigle))
  }, cassette = "metadata-repeat")
})
