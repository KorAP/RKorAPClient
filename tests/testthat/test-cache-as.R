test_that("cacheAsFileName appends .rds where it is missing", {
  expect_equal(RKorAPClient:::cacheAsFileName("analysis"), "analysis.rds")
  expect_equal(RKorAPClient:::cacheAsFileName("analysis.rds"), "analysis.rds")
  expect_equal(RKorAPClient:::cacheAsFileName("analysis.RDS"), "analysis.RDS")
})

offlineKco <- function() {
  offlineKorapConnection(apiUrl = "https://example.invalid/",
    KorAPUrl = "https://example.invalid/",
    authorizationSupported = FALSE,
    verbose = FALSE
  )
}

test_that("frequency caches from before dispersion remain reusable and unchanged", {
  kco <- offlineKco()
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  kept <- tibble::tibble(query = "word", vc = "", totalResults = 7, total = 100)
  attr(kept, RKorAPClient:::cacheAsAttribute) <- list(
    scoreVersion = "1.4.0", packageVersion = "1.4.1",
    parameters = list(query = "word", vc = "", conf.level = .95, as.alternatives = FALSE),
    dots = list(), apiUrl = kco@apiUrl, indexRevision = "older-index")
  saveRDS(kept, file)
  before <- readBin(file, "raw", n = file.info(file)$size)
  testthat::local_mocked_bindings(corpusQuery = function(...) stop("must reuse saved results"),
    .package = "RKorAPClient")
  expect_silent(result <- frequencyQuery(kco, "word", cacheAs = file))
  expect_equal(result$totalResults, 7)
  expect_identical(readBin(file, "raw", n = file.info(file)$size), before)
})

test_that("blessCacheAs makes an old file usable again", {
  kco <- offlineKco()
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  legacy <- tibble::tibble(node = "Test", collocate = "c", logDice = 7)
  saveRDS(legacy, file)

  testthat::local_mocked_bindings(
    collocatesQuery = function(...) stop("server must not be contacted"),
    .package = "RKorAPClient"
  )

  expect_message(blessCacheAs(file), "records no parameters")
  expect_silent(result <- collocationAnalysis(kco, "Test", cacheAs = file))
  expect_equal(result, legacy)
})

test_that("blessing records itself without claiming a provenance", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(tibble::tibble(x = 1), file)

  suppressMessages(blessCacheAs(file))
  info <- cacheAsInfo(file)

  expect_equal(info$scoresConfirmedFor, RKorAPClient:::cacheAsScoreVersion)
  expect_s3_class(info$blessedAt, "POSIXct")
  # what actually wrote the file is unknown and stays unclaimed
  expect_null(info$packageVersion)
})

test_that("blessCacheAs refuses a file that is not there", {
  expect_error(blessCacheAs(file.path(tempdir(), "no-such-cache.rds")), "does not exist")
})

test_that("withCachedResults takes a file as it is, and says so", {
  kco <- offlineKco()
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  legacy <- tibble::tibble(node = "Test", collocate = "c", logDice = 7)
  saveRDS(legacy, file)

  testthat::local_mocked_bindings(
    collocatesQuery = function(...) stop("server must not be contacted"),
    .package = "RKorAPClient"
  )

  expect_warning(
    result <- withCachedResults(collocationAnalysis(kco, "Test", cacheAs = file)),
    "used as it is"
  )
  expect_equal(result, legacy)
  # and the mode does not outlive the expression
  expect_null(getOption("rkorap.cacheAs"))
})

test_that("the offline mode refuses to compute what is not in a file", {
  kco <- offlineKco()
  missing <- file.path(tempdir(), "not-written-yet.rds")

  expect_error(
    withCachedResults(
      collocationAnalysis(kco, "Test", cacheAs = missing),
      mode = "offline"
    ),
    "offline"
  )
})

test_that("the cacheAs mode comes from the option, then the environment", {
  mode <- RKorAPClient:::cacheAsMode

  expect_equal(mode(), "check")

  local({
    old <- Sys.getenv("KORAP_CACHE_AS", unset = NA)
    on.exit(if (is.na(old)) Sys.unsetenv("KORAP_CACHE_AS") else Sys.setenv(KORAP_CACHE_AS = old))
    Sys.setenv(KORAP_CACHE_AS = "reuse")
    expect_equal(mode(), "reuse")
    # the option wins where both are given
    previous <- options(rkorap.cacheAs = "offline")
    on.exit(options(previous), add = TRUE)
    expect_equal(mode(), "offline")
  })

  local({
    old <- options(rkorap.cacheAs = "nonsense")
    on.exit(options(old))
    expect_error(mode(), "Unknown cacheAs mode")
  })
})

test_that("cacheAsInfo says what produced a file", {
  withWorkflowFixture("workflow-cache-as-9", function(state) {

    kco <- suppressMessages(fixtureKorapConnection(verbose = FALSE))
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
  withWorkflowFixture("workflow-cache-as-12", function(state) {

    kco <- suppressMessages(fixtureKorapConnection(verbose = FALSE))
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
})

test_that("a cache file written before the scores were corrected is refused", {
  withWorkflowFixture("workflow-cache-as-13", function(state) {

    kco <- suppressMessages(fixtureKorapConnection(verbose = FALSE))
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
})

test_that("a query that differs is recomputed rather than read back", {
  withWorkflowFixture("workflow-cache-as-14", function(state) {

    kco <- suppressMessages(fixtureKorapConnection(verbose = FALSE))
    file <- tempfile(fileext = ".rds")
    on.exit(unlink(file), add = TRUE)

    frequencyQuery(kco, "Ameisenplage", cacheAs = file)
    expect_warning(frequencyQuery(kco, "Heuschreckenplage", cacheAs = file), "query")
  })
})

test_that("verbosity is not part of what a cache file records", {
  withWorkflowFixture("workflow-cache-as-15", function(state) {

    kco <- suppressMessages(fixtureKorapConnection(verbose = FALSE))
    file <- tempfile(fileext = ".rds")
    on.exit(unlink(file), add = TRUE)

    corpusStats(kco, vc = "pubDate since 2020", as.df = TRUE, cacheAs = file)
    # how loud a query is does not change what it returns, so it must not make
    # the file be thrown away
    expect_silent(
      corpusStats(kco, vc = "pubDate since 2020", as.df = TRUE, verbose = TRUE, cacheAs = file)
    )
  })
})

test_that("cache records ignore only options explicitly named by their caller", {
  kco <- offlineKco()
  otherAnalysis <- function(kco, as.alternatives = FALSE, seed = 7) {
    RKorAPClient:::cacheAsRecord(environment(), list(), kco)
  }
  record <- otherAnalysis(kco)
  expect_equal(record$parameters$seed, 7)
  expect_equal(record$parameters$as.alternatives, FALSE)
  expect_null(RKorAPClient:::cacheAsRejectionReason(record, record))
  expect_match(RKorAPClient:::cacheAsRejectionReason(record, otherAnalysis(kco, seed = 8)), "seed")
})
