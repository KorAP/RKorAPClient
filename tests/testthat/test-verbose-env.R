test_that("KORAP_VERBOSE overrides verbose default and is restorable", {
  old <- Sys.getenv("KORAP_VERBOSE", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("KORAP_VERBOSE") else Sys.setenv(KORAP_VERBOSE = old)
  }, add = TRUE)

  # Turn on via env var
  Sys.setenv(KORAP_VERBOSE = "true")
  k1 <- KorAPConnection(accessToken = NULL)
  expect_true(k1@verbose)

  # Explicit argument takes precedence
  k2 <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  expect_false(k2@verbose)

  # Turn off via env var
  Sys.setenv(KORAP_VERBOSE = "false")
  k3 <- KorAPConnection(accessToken = NULL)
  expect_false(k3@verbose)
})

