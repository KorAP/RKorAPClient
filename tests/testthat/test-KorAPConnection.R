withConnectionFixture <- function(code, cassette = "connection") {
  withKorapFixtureApi(code, cassette = cassette)
}

# Exercise the real keyring front end with an in-memory backend, never the
# user's OS keyring. All backend functions and entries are scoped to this call.
withMemoryKeyring <- function(code) {
  keys <- data.frame(service = character(), username = character(), password = character())
  backend <- list(
    set_with_value = function(service, username, password, keyring = NULL) {
      keys <<- keys[!(keys$service == service & keys$username == username), , drop = FALSE]
      keys <<- rbind(keys, data.frame(service, username, password))
      invisible(NULL)
    },
    get = function(service, username, keyring = NULL) {
      found <- keys$service == service & keys$username == username
      if (sum(found) != 1L) stop("No such key")
      keys$password[found]
    },
    list = function(service, keyring = NULL) {
      keys[keys$service == service, c("service", "username"), drop = FALSE]
    },
    delete = function(service, username, keyring = NULL) {
      found <- keys$service == service & keys$username == username
      if (!any(found)) stop("No such key")
      keys <<- keys[!found, , drop = FALSE]
      invisible(NULL)
    }
  )
  local_mocked_bindings(default_backend = function() backend, .package = "keyring")
  code(backend)
}

test_that("connection initialization reports simulated resolver and timeout failures", {
  # Transport failures have no HTTP response to put in a vcr cassette. The
  # separate webfakes suite also checks an actual curl timeout over loopback.
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  for (failure in c("Could not resolve host", "Timeout was reached")) {
    local_mocked_bindings(curl_fetch_memory = function(...) stop(failure), .package = "curl")
    expect_message(kco <- KorAPConnection(KorAPUrl = "https://korap.example.invalid/",
      accessToken = NULL, cache = FALSE), failure)
    expect_null(kco@welcome)
  }
})

test_that("connection initialization handles rejected tokens and bad gateways offline", {
  withConnectionFixture(function(state) {
    for (case in list(c("unauthorized", "401"), c("bad-gateway", "502"))) {
      expect_message(kco <- KorAPConnection(KorAPUrl = "https://korap.example.invalid/",
        apiUrl = paste0("https://korap.example.invalid/api/v1.0/", case[1]),
        accessToken = "fixture-invalid-token", cache = FALSE), case[2])
      expect_null(kco@welcome)
    }
  }, cassette = "connection-init-errors")
})

test_that("connection URLs are normalized and connection information is printable offline", {
  withConnectionFixture(function(state) {
    for (url in c("https://korap.example.invalid", "https://korap.example.invalid/")) {
      kco <- suppressMessages(KorAPConnection(KorAPUrl = url, accessToken = NULL, cache = FALSE))
      expect_identical(kco@apiUrl, "https://korap.example.invalid/api/v1.0/")
      expect_identical(kco@KorAPUrl, "https://korap.example.invalid/")
      expect_output(print(kco), "<KorAPConnection>")
      expect_output(show(kco), "apiUrl:")
    }
  })
})

test_that("persisting a missing or confidential-client token never writes a key", {
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    local_mocked_bindings(default_backend = function() stop("Unexpected keyring access"),
      .package = "keyring")
    expect_error(persistAccessToken(kco), "not supplied any access token")
    kco@oauthClient <- list(id = "fixture-client")
    expect_warning(result <- persistAccessToken(kco, "fixture-token"),
      "Short lived access tokens.*cannot be persisted")
    expect_identical(result, kco)
  })
})

test_that("persisted tokens are read by getAccessToken and the connection default", {
  withConnectionFixture(function(state) {
    withMemoryKeyring(function(backend) {
      kco <- suppressMessages(fixtureKorapConnection())
      result <- persistAccessToken(kco, "fixture-token")
      expect_identical(result@accessToken, "fixture-token")
      expect_identical(backend$list("RKorAPClientAccessToken")$username, kco@KorAPUrl)
      expect_identical(RKorAPClient:::getAccessToken(kco@KorAPUrl), "fixture-token")
      restored <- suppressMessages(KorAPConnection(KorAPUrl = kco@KorAPUrl, cache = FALSE))
      expect_identical(restored@accessToken, "fixture-token")
      expect_null(kco@accessToken)
    })
  })
})

test_that("clearing a token removes the in-memory key and updates the returned connection", {
  withConnectionFixture(function(state) {
    withMemoryKeyring(function(backend) {
      kco <- persistAccessToken(suppressMessages(fixtureKorapConnection()), "fixture-token")
      result <- clearAccessToken(kco)
      expect_s4_class(result, "KorAPConnection")
      expect_null(result@accessToken)
      expect_null(RKorAPClient:::getAccessToken(kco@KorAPUrl))
      expect_identical(kco@accessToken, "fixture-token")
      expect_error(clearAccessToken(kco), "No such key")
      expect_identical(kco@accessToken, "fixture-token")
    })
  })
})

test_that("missing or unavailable keyrings return no token", {
  withMemoryKeyring(function(backend) {
    expect_null(RKorAPClient:::getAccessToken("https://korap.example.invalid/"))
  })
  local_mocked_bindings(default_backend = function() stop("Keyring unavailable"),
    .package = "keyring")
  expect_null(RKorAPClient:::getAccessToken("https://korap.example.invalid/"))
})

test_that("authorization warnings depend on the connection's token and capabilities", {
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    expect_warning(RKorAPClient:::warnIfNotAuthorized(kco), "authorize your application")
    kco@accessToken <- "fixture-token"
    expect_silent(RKorAPClient:::warnIfNotAuthorized(kco))
    kco@accessToken <- NULL
    kco@authorizationSupported <- FALSE
    expect_silent(RKorAPClient:::warnIfNotAuthorized(kco))
  })
})

test_that("auth handles unsupported authorization, other instances and existing tokens offline", {
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    kco@authorizationSupported <- FALSE
    expect_identical(auth(kco), kco)
    kco@authorizationSupported <- TRUE
    expect_warning(result <- auth(kco), "You can use the default app_id only for")
    expect_identical(result, kco)
    kco@accessToken <- "fixture-token"
    expect_identical(auth(kco, app_id = "fixture-client"), kco)
    expect_length(state$requests, 2)
  })
})

test_that("apiCall reports offline status without trying a request", {
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
    expect_message(result <- apiCall(kco, paste0(kco@apiUrl, "not-requested")), "No internet connection")
    expect_null(result)
    expect_length(state$requests, 2)
  })
})

test_that("apiCall handles HTTP errors, non-JSON content and malformed JSON offline", {
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    for (case in list(c("missing", "status 404"), c("html", "API did not return JSON"),
        c("broken-json", "Failed to parse JSON"))) {
      expect_message(result <- apiCall(kco, paste0(kco@apiUrl, case[1])), case[2])
      expect_null(result)
    }
  }, cassette = "api-errors")
})

test_that("successful responses are persisted, reused and cleared in an isolated cache", {
  directory <- tempfile("connection-cache-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(KorAPCacheSubDir = function() directory, .package = "RKorAPClient")
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection(cache = TRUE))
    url <- paste0(kco@apiUrl, "ok")
    first <- apiCall(kco, url)
    key <- list(url, kco@accessToken, kco@indexRevision)
    expect_equal(R.cache::loadCache(dir = directory, key = key), first)
    before <- length(state$requests)
    cached <- apiCall(kco, url)
    expect_identical(cached$meta$cached, "local")
    expect_length(state$requests, before)
    clearCache(kco)
    expect_null(R.cache::loadCache(dir = directory, key = key))
    expect_equal(apiCall(kco, url), first)
    expect_length(state$requests, before + 1)
  }, cassette = "connection-cache")
})

test_that("KorAPCacheSubDir uses the package version", {
  expect_match(RKorAPClient:::KorAPCacheSubDir(), "^RKorAPClient_[0-9]+\\.[0-9]+$")
})

test_that("warning 682 is reported and the response is not persisted", {
  directory <- tempfile("connection-warning-cache-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(KorAPCacheSubDir = function() directory, .package = "RKorAPClient")
  withConnectionFixture(function(state) {
    kco <- suppressMessages(fixtureKorapConnection(cache = TRUE))
    url <- paste0(kco@apiUrl, "warning")
    before <- length(state$requests)
    expect_message(first <- apiCall(kco, url), "682")
    expect_equal(first$meta$totalResults, 2)
    expect_null(R.cache::loadCache(dir = directory,
      key = list(url, kco@accessToken, kco@indexRevision)))
    expect_message(apiCall(kco, url), "682")
    expect_length(state$requests, before + 2)
  }, cassette = "connection-warning")
})
