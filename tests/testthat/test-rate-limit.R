# Kustvakt limits the requests per access token and answers 429 (Too Many
# Requests) with a Retry-After header beyond that. These tests run offline.

mockConnection <- function() {
  methods::new(
    "KorAPConnection",
    apiUrl = "https://example.invalid/api/v1.0/",
    KorAPUrl = "https://example.invalid/",
    authorizationSupported = FALSE,
    verbose = FALSE
  )
}

rateLimitResponse <- function(retryAfter = "18") {
  # Kustvakt labels its plain text message as JSON
  httr2::response(
    429,
    headers = list("Retry-After" = retryAfter, "Content-Type" = "application/json"),
    body = charToRaw("Rate limit exceeded")
  )
}

test_that("retryAfterSeconds waits as long as the server asks, plus a second", {
  expect_equal(RKorAPClient:::retryAfterSeconds(rateLimitResponse("18")), 19)
  expect_true(is.na(RKorAPClient:::retryAfterSeconds(httr2::response(503))))
})

test_that("retryAfterSeconds reports the wait if verbose", {
  expect_output(RKorAPClient:::retryAfterSeconds(rateLimitResponse("5"), verbose = TRUE), "retrying in 6s")
})

test_that("apiCall retries requests that were rate limited", {
  performed <- NULL
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      performed <<- req
      httr2::response_json(body = list(meta = list(totalResults = 1)))
    },
    .package = "httr2"
  )

  apiCall(mockConnection(), "https://example.invalid/api/v1.0/search?q=Test", cache = FALSE)

  expect_equal(performed$policies$retry_max_tries, RKorAPClient:::maxApiTries)
  expect_equal(performed$policies$retry_max_wait, RKorAPClient:::maxRetryWaitSeconds)
  expect_equal(performed$policies$retry_after(rateLimitResponse("3")), 4)
})

test_that("apiCall shows plain text error messages labelled as JSON", {
  testthat::local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  testthat::local_mocked_bindings(req_perform = function(req, ...) rateLimitResponse(), .package = "httr2")

  messages <- testthat::capture_messages(
    result <- apiCall(mockConnection(), "https://example.invalid/api/v1.0/search?q=Test", cache = FALSE)
  )
  expect_null(result)
  expect_match(messages, "status 429", all = FALSE)
  expect_match(messages, "Error details: Rate limit exceeded", all = FALSE)
})

test_that("a page that cannot be fetched leaves the result incomplete, not finished", {
  kco <- mockConnection()
  page <- function(offset) {
    ids <- offset + seq_len(50)
    list(
      meta = list(startIndex = offset, itemsPerPage = 50, totalResults = 120),
      matches = data.frame(
        textSigle = paste0("A00/JAN/", ids),
        matchID = paste0("match-A00/JAN/", ids, "-p10-11"),
        stringsAsFactors = FALSE
      )
    )
  }
  calls <- 0
  testthat::local_mocked_bindings(
    apiCall = function(kco, url, ...) {
      calls <<- calls + 1
      if (calls == 1) page(0) else NULL
    },
    .package = "RKorAPClient"
  )
  q <- methods::new(
    "KorAPQuery",
    korapConnection = kco,
    totalResults = 120,
    fields = "textSigle",
    requestUrl = "https://example.invalid/api/v1.0/search?q=Test&fields=textSigle",
    request = "?q=Test"
  )

  expect_warning(q <- fetchAll(q, verbose = FALSE), "from offset 50 failed.*incomplete.*fetchRest")
  expect_equal(nrow(q@collectedMatches), 50)
  expect_equal(q@nextStartIndex, 50)
  expect_true(q@hasMoreMatches)
})
