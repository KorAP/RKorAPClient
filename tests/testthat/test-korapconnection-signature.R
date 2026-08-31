withOfflineKorAPEnv <- function(code, verboseEnv = NA_character_) {
  oldUrl <- Sys.getenv("KORAP_URL", unset = NA_character_)
  oldVerbose <- Sys.getenv("KORAP_VERBOSE", unset = NA_character_)
  on.exit(
    {
      if (is.na(oldUrl)) Sys.unsetenv("KORAP_URL") else Sys.setenv(KORAP_URL = oldUrl)
      if (is.na(oldVerbose)) {
        Sys.unsetenv("KORAP_VERBOSE")
      } else {
        Sys.setenv(KORAP_VERBOSE = oldVerbose)
      }
    },
    add = TRUE
  )
  Sys.setenv(KORAP_URL = "https://example.invalid/")
  if (is.na(verboseEnv)) Sys.unsetenv("KORAP_VERBOSE") else Sys.setenv(KORAP_VERBOSE = verboseEnv)
  force(code)
}

test_that("the documented KorAPConnection() defaults are the ones actually used", {
  # KorAPConnection() only forwards the arguments that were supplied, so its
  # own defaults are never evaluated. They mirror those of the initialize
  # method to document them, and this test makes sure that the two cannot
  # drift apart silently.
  withOfflineKorAPEnv({
    kco <- KorAPConnection(accessToken = NULL)
    documented <- formals(KorAPConnection)

    # accessToken (keyring), apiUrl (derived) and authorizationSupported
    # (answered by the server) are covered by the tests below instead
    for (parameter in c(
      "KorAPUrl", "apiVersion", "oauthClient", "oauthScope",
      "userAgent", "timeout", "verbose", "cache"
    )) {
      expect_equal(
        slot(kco, parameter),
        eval(documented[[parameter]]),
        info = parameter
      )
    }
  })
})

test_that("KorAPConnection() forwards only the arguments that were supplied", {
  # `missing()` based overrides in the initialize method must keep working
  withOfflineKorAPEnv(
    {
      expect_true(KorAPConnection(accessToken = NULL)@verbose)
      # an explicitly passed verbose argument still wins over the env var
      expect_false(KorAPConnection(accessToken = NULL, verbose = FALSE)@verbose)
    },
    verboseEnv = "true"
  )
})

test_that("KorAPConnection() derives apiUrl unless it is given explicitly", {
  kco <- KorAPConnection(KorAPUrl = "https://example.invalid/", accessToken = NULL)
  expect_equal(kco@apiUrl, "https://example.invalid/api/v1.0/")

  kco <- KorAPConnection(
    KorAPUrl = "https://example.invalid/",
    apiUrl = "https://other.invalid/api/",
    accessToken = NULL
  )
  expect_equal(kco@apiUrl, "https://other.invalid/api/")
})

test_that("KorAPConnection objects can still be created with new()", {
  kco <- methods::new(
    "KorAPConnection",
    KorAPUrl = "https://example.invalid/",
    apiUrl = "https://example.invalid/api/v1.0/",
    accessToken = NULL,
    verbose = FALSE
  )
  expect_s4_class(kco, "KorAPConnection")
  expect_equal(kco@apiUrl, "https://example.invalid/api/v1.0/")
})

test_that("defaultKorAPUrl() honours KORAP_URL", {
  old <- Sys.getenv("KORAP_URL", unset = NA_character_)
  on.exit(
    if (is.na(old)) Sys.unsetenv("KORAP_URL") else Sys.setenv(KORAP_URL = old),
    add = TRUE
  )

  Sys.setenv(KORAP_URL = "https://example.invalid/")
  expect_equal(defaultKorAPUrl(), "https://example.invalid/")

  # an empty KORAP_URL counts as unset
  Sys.setenv(KORAP_URL = "")
  expect_equal(defaultKorAPUrl(), "https://korap.ids-mannheim.de/")

  Sys.unsetenv("KORAP_URL")
  expect_equal(defaultKorAPUrl(), "https://korap.ids-mannheim.de/")
})
