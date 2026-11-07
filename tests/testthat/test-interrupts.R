interruptTestConnection <- function() {
  testthat::local_mocked_bindings(apiCall = function(...) NULL, .package = "RKorAPClient")
  suppressMessages(KorAPConnection(
    apiUrl = "https://example.invalid/api/v1.0/",
    accessToken = NULL, cache = FALSE, verbose = FALSE
  ))
}

test_that("requests scope the error hook and restore it on success and failure", {
  rlang::local_options(error = quote(stop("user error hook")))
  previous <- getOption("error")
  request <- httr2::request("https://example.invalid/")
  response <- httr2::response_json(body = list(ok = TRUE))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      expect_null(getOption("error"))
      response
    },
    .package = "httr2"
  )
  expect_identical(RKorAPClient:::performRequest(request), response)
  expect_identical(getOption("error"), previous)

  failure <- simpleError("connection failed")
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      expect_null(getOption("error"))
      stop(failure)
    },
    .package = "httr2"
  )
  observed <- NULL
  caught <- tryCatch(
    withCallingHandlers(
      RKorAPClient:::performRequest(request),
      error = function(e) observed <<- getOption("error")
    ),
    error = identity
  )
  expect_identical(caught, failure)
  expect_identical(observed, previous)
  expect_identical(getOption("error"), previous)
})

test_that("cancelling an API request propagates an interrupt without caching a result", {
  kco <- interruptTestConnection()
  rlang::local_options(error = quote(stop("user error hook")))
  previous <- getOption("error")
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  testthat::local_mocked_bindings(
    loadCache = function(...) NULL,
    saveCache = function(...) stop("cancelled response must not be cached"),
    .package = "R.cache"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      expect_null(getOption("error"))
      rlang::interrupt()
    },
    .package = "httr2"
  )
  continued <- FALSE
  caught <- tryCatch({
    apiCall(kco, "https://example.invalid/search", cache = TRUE)
    continued <- TRUE
  }, interrupt = identity)
  expect_s3_class(caught, "interrupt")
  expect_false(inherits(caught, "error"))
  expect_false(continued)
  expect_identical(getOption("error"), previous)
})

test_that("cancelling a page fetch stops retrieval instead of returning partial success", {
  kco <- interruptTestConnection()
  rlang::local_options(error = quote(stop("user error hook")))
  previous <- getOption("error")
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  calls <- 0L
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      calls <<- calls + 1L
      if (calls > 1L) rlang::interrupt()
      httr2::response_json(body = list(
        meta = list(startIndex = 0, itemsPerPage = 50, totalResults = 120),
        matches = data.frame(
          textSigle = paste0("A00/JAN/", seq_len(50)),
          matchID = paste0("match-A00/JAN/", seq_len(50), "-p10-11")
        )
      ))
    },
    .package = "httr2"
  )
  q <- methods::new(
    "KorAPQuery",
    korapConnection = kco,
    totalResults = 120,
    fields = "textSigle",
    requestUrl = "https://example.invalid/search?q=Test&fields=textSigle",
    request = "?q=Test"
  )
  before <- q
  completed <- FALSE
  caught <- tryCatch({
    q <- fetchAll(q, verbose = FALSE)
    completed <- TRUE
  }, interrupt = identity)
  expect_s3_class(caught, "interrupt")
  expect_false(completed)
  expect_equal(calls, 2L)
  expect_identical(q, before)
  expect_identical(getOption("error"), previous)
})

test_that("cancellation logs only when verbose and never signals a warning", {
  kco <- interruptTestConnection()
  rlang::local_options(warn = 2, error = quote(stop("user error hook")))
  previous <- getOption("error")
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  testthat::local_mocked_bindings(
    req_perform = function(...) rlang::interrupt(), .package = "httr2"
  )
  for (verbose in c(FALSE, TRUE)) {
    # The explicit argument must override the connection's default in both directions.
    kco@verbose <- !verbose
    caught <- NULL
    output <- testthat::capture_output(caught <- tryCatch(
      apiCall(kco, "https://example.invalid/search", cache = FALSE, verbose = verbose),
      interrupt = identity
    ))
    expect_s3_class(caught, "interrupt")
    expect_false(inherits(caught, "error"))
    if (verbose) {
      expect_match(output, "Request cancelled by user\\.")
      expect_length(gregexpr("Request cancelled by user", output, fixed = TRUE)[[1]], 1L)
    } else {
      expect_identical(output, "")
    }
    expect_identical(getOption("error"), previous)
  }
})

test_that("queries and page fetches respect their own cancellation verbosity", {
  kco <- interruptTestConnection()
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  testthat::local_mocked_bindings(
    req_perform = function(...) rlang::interrupt(), .package = "httr2"
  )
  q <- methods::new("KorAPQuery", korapConnection = kco, totalResults = 100,
    requestUrl = "https://example.invalid/search?q=word", request = "?q=word")
  for (verbose in c(FALSE, TRUE)) {
    kco@verbose <- !verbose
    q@korapConnection <- kco
    operations <- list(
      function() corpusQuery(kco, "word", verbose = verbose),
      function() corpusQuery(kco, c("one", "two"), verbose = verbose),
      function() fetchNext(q, verbose = verbose)
    )
    for (operation in operations) {
      caught <- NULL
      output <- testthat::capture_output(caught <- tryCatch(operation(), interrupt = identity))
      expect_s3_class(caught, "interrupt")
      if (verbose) expect_match(output, "Request cancelled by user\\.") else expect_identical(output, "")
    }
  }
})

