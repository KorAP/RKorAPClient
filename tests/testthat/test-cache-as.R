test_that("cacheAsFileName appends .rds where it is missing", {
  expect_equal(RKorAPClient:::cacheAsFileName("analysis"), "analysis.rds")
  expect_equal(RKorAPClient:::cacheAsFileName("analysis.rds"), "analysis.rds")
  expect_equal(RKorAPClient:::cacheAsFileName("analysis.RDS"), "analysis.RDS")
})

test_that("cacheAsInfo says what produced a file", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)

  frequencyQuery(kco, "Ameisenplage", cacheAs = file)
  info <- cacheAsInfo(file)

  expect_equal(info$packageVersion, as.character(utils::packageVersion("RKorAPClient")))
  expect_equal(info$apiUrl, kco@apiUrl)
  expect_equal(info$indexRevision, kco@indexRevision)
  expect_equal(info$parameters$query, "Ameisenplage")
  # given without its extension, the .rds file is found
  expect_equal(cacheAsInfo(sub("\\.rds$", "", file)), info)
})

test_that("cacheAsInfo has nothing to report for a file from before 1.4.0", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(tibble::tibble(x = 1), file)

  expect_null(cacheAsInfo(file))
})

test_that("cacheAsInfo refuses a file that is not there", {
  expect_error(
    cacheAsInfo(file.path(tempdir(), "no-such-cache.rds")),
    "does not exist"
  )
})

test_that("a cache file is written and read back, without contacting the server", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  dir <- file.path(tempdir(), "cacheAsTest")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # every query function that takes cacheAs, so that one added later without a
  # cache file of its own does not go unnoticed
  queries <- list(
    "frequencyQuery" = function(f) frequencyQuery(kco, "Ameisenplage", cacheAs = f),
    "corpusStats" = function(f) corpusStats(kco, vc = "pubDate since 2020", as.df = TRUE, cacheAs = f),
    "collocationScoreQuery" = function(f) collocationScoreQuery(kco, "Grund", "triftiger", cacheAs = f),
    "textMetadata" = function(f) textMetadata(kco, "WPD17/L79/98721", cacheAs = f)
  )

  for (name in names(queries)) {
    query <- queries[[name]]
    file <- file.path(dir, name)

    fresh <- query(file)
    expect_true(file.exists(paste0(file, ".rds")), info = name)

    fromCache <- local({
      testthat::local_mocked_bindings(
        apiCall = function(...) stop("server must not be contacted"),
        .package = "RKorAPClient"
      )
      query(file)
    })
    expect_equal(fromCache, fresh, info = name)

    # what produced the result lives in the file only, not in the value
    expect_null(attr(fromCache, RKorAPClient:::cacheAsAttribute), info = name)
    expect_equal(
      attr(readRDS(paste0(file, ".rds")), RKorAPClient:::cacheAsAttribute)$scoreVersion,
      RKorAPClient:::cacheAsScoreVersion,
      info = name
    )
  }
})

test_that("a cache file written before the scores were corrected is refused", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)

  frequencyQuery(kco, "Ameisenplage", cacheAs = file)
  aged <- readRDS(file)
  record <- attr(aged, RKorAPClient:::cacheAsAttribute)
  # as a file from before the corrections has it: an older score generation, and
  # an older version to go with it
  record$scoreVersion <- "1.3.0"
  record$packageVersion <- "1.3.0"
  attr(aged, RKorAPClient:::cacheAsAttribute) <- record
  saveRDS(aged, file)

  expect_warning(frequencyQuery(kco, "Ameisenplage", cacheAs = file), "1\\.3\\.0")
  # and the refused file is replaced by a current one, so the warning comes once
  expect_silent(frequencyQuery(kco, "Ameisenplage", cacheAs = file))
})

test_that("a query that differs is recomputed rather than read back", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)

  frequencyQuery(kco, "Ameisenplage", cacheAs = file)
  expect_warning(frequencyQuery(kco, "Heuschreckenplage", cacheAs = file), "query")
})

test_that("verbosity is not part of what a cache file records", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)

  corpusStats(kco, vc = "pubDate since 2020", as.df = TRUE, cacheAs = file)
  # how loud a query is does not change what it returns, so it must not make
  # the file be thrown away
  expect_silent(
    corpusStats(kco, vc = "pubDate since 2020", as.df = TRUE, verbose = TRUE, cacheAs = file)
  )
})
