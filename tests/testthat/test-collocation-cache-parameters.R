offlineConnection <- function() {
  methods::new(
    "KorAPConnection",
    apiUrl = "https://example.invalid/",
    KorAPUrl = "https://example.invalid/",
    # keeps the unrelated "authorize your application" warning out of the way
    authorizationSupported = FALSE,
    verbose = FALSE
  )
}

# Returning no candidates lets collocationAnalysis() finish without contacting
# the server, so that the caching around it can be tested offline.
mockEmptyAnalysis <- function() {
  testthat::local_mocked_bindings(
    collocatesQuery = function(...) tibble::tibble(),
    .package = "RKorAPClient",
    .env = parent.frame()
  )
}

test_that("cache files record the parameters of the analysis", {
  mockEmptyAnalysis()
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)

  collocationAnalysis(offlineConnection(), "Test", minOccur = 3, cacheAs = cacheFile)

  stored <- attr(readRDS(cacheFile), RKorAPClient:::cacheAsAttribute)
  expect_false(is.null(stored))
  expect_equal(stored$scoreVersion, RKorAPClient:::cacheAsScoreVersion)
  expect_equal(stored$packageVersion, as.character(utils::packageVersion("RKorAPClient")))
  expect_equal(stored$parameters$node, "Test")
  expect_equal(stored$parameters$minOccur, 3)
  expect_equal(stored$apiUrl, "https://example.invalid/")
  # every parameter of the analysis is recorded, not just those visible in the
  # result, so that added parameters are covered automatically
  expect_true(all(c(
    "topCollocatesLimit", "searchHitsSampleLimit", "exactFrequencies",
    "stopwords", "threshold", "collocateFilterRegex", "seed"
  ) %in% names(stored$parameters)))
  # kco and cacheAs are not parameters of the analysis
  expect_false(any(c("kco", "cacheAs") %in% names(stored$parameters)))
})

test_that("the returned result is the same whether it was cached or not", {
  mockEmptyAnalysis()
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  fresh <- collocationAnalysis(kco, "Test", cacheAs = cacheFile)
  fromCache <- collocationAnalysis(kco, "Test", cacheAs = cacheFile)

  expect_equal(fromCache, fresh)
  # the parameters live in the file only, not in the returned value
  expect_null(attr(fromCache, RKorAPClient:::cacheAsAttribute))
})

test_that("an unchanged call is served from the cache without contacting the server", {
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  local({
    mockEmptyAnalysis()
    collocationAnalysis(kco, "Test", minOccur = 3, cacheAs = cacheFile)
  })

  testthat::local_mocked_bindings(
    collocatesQuery = function(...) stop("server must not be contacted"),
    .package = "RKorAPClient"
  )
  expect_no_warning(collocationAnalysis(kco, "Test", minOccur = 3, cacheAs = cacheFile))
})

test_that("changed parameters make the analysis be recomputed, with a warning", {
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  local({
    mockEmptyAnalysis()
    collocationAnalysis(kco, "Test", minOccur = 3, cacheAs = cacheFile)
  })

  testthat::local_mocked_bindings(
    collocatesQuery = function(...) stop("recomputed"),
    .package = "RKorAPClient"
  )

  # the warning names the parameter that differs, and the analysis is rerun
  expect_warning(
    expect_error(
      collocationAnalysis(kco, "Test", minOccur = 5, cacheAs = cacheFile),
      "recomputed"
    ),
    "minOccur"
  )

  # a changed node is caught as well
  expect_warning(
    expect_error(
      collocationAnalysis(kco, "Other", minOccur = 3, cacheAs = cacheFile),
      "recomputed"
    ),
    "node"
  )
})

test_that("a recomputed analysis overwrites the stale cache file", {
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  local({
    mockEmptyAnalysis()
    collocationAnalysis(kco, "Test", minOccur = 3, cacheAs = cacheFile)
  })

  local({
    mockEmptyAnalysis()
    expect_warning(
      collocationAnalysis(kco, "Test", minOccur = 5, cacheAs = cacheFile),
      "minOccur"
    )
  })

  stored <- attr(readRDS(cacheFile), RKorAPClient:::cacheAsAttribute)
  expect_equal(stored$parameters$minOccur, 5)
})

test_that("cache files written before the scores were corrected are refused", {
  mockEmptyAnalysis()
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  # as written by RKorAPClient 1.3.0, whose logDice and ll differ from today's
  legacy <- tibble::tibble(node = "Test", collocate = "c", logDice = 7)
  saveRDS(legacy, cacheFile)

  expect_warning(
    result <- collocationAnalysis(kco, "Test", cacheAs = cacheFile),
    "logDice"
  )
  expect_false(identical(result, legacy))
  # and the stale file is replaced by one that records what wrote it
  expect_false(is.null(attr(readRDS(cacheFile), RKorAPClient:::cacheAsAttribute)))
})

test_that("cache files of an older version are refused, naming it", {
  mockEmptyAnalysis()
  cacheFile <- tempfile(fileext = ".rds")
  on.exit(unlink(cacheFile), add = TRUE)
  kco <- offlineConnection()

  collocationAnalysis(kco, "Test", cacheAs = cacheFile)
  aged <- readRDS(cacheFile)
  record <- attr(aged, RKorAPClient:::cacheAsAttribute)
  record$scoreVersion <- "1.3.0"
  record$packageVersion <- "1.3.0"
  attr(aged, RKorAPClient:::cacheAsAttribute) <- record
  saveRDS(aged, cacheFile)

  expect_warning(collocationAnalysis(kco, "Test", cacheAs = cacheFile), "1\\.3\\.0")
})

test_that("cacheAsRejectionReason says why a cache file cannot be used", {
  reason <- RKorAPClient:::cacheAsRejectionReason

  stored <- list(
    scoreVersion = RKorAPClient:::cacheAsScoreVersion,
    parameters = list(node = "Test", minOccur = 3, vc = ""),
    dots = list(),
    apiUrl = "https://korap.ids-mannheim.de/api/v1.0/"
  )

  expect_null(reason(stored, stored))
  expect_match(reason(NULL, stored), "before RKorAPClient")

  # a file from before the corrections: an older score generation, and the
  # version that wrote it, which the reason names
  aged <- stored
  aged$scoreVersion <- "1.3.0"
  aged$packageVersion <- "1.3.0"
  expect_match(reason(aged, stored), "written by RKorAPClient 1\\.3\\.0")

  # one that does not even say what wrote it
  anonymous <- stored
  anonymous$scoreVersion <- NULL
  expect_match(reason(anonymous, stored), "written before RKorAPClient")

  changed <- stored
  changed$parameters$minOccur <- 5
  expect_match(reason(stored, changed), "minOccur")

  changed$parameters$vc <- "textType=/Zeit.*/"
  expect_match(reason(stored, changed), "minOccur, vc")

  changed <- stored
  changed$dots <- list(smoothingConstant = 1)
  expect_match(reason(stored, changed), "\\.\\.\\.")

  changed <- stored
  changed$apiUrl <- "https://korap.dnb.de/api/v1.0/"
  expect_match(reason(stored, changed), "KorAP instance")
})
