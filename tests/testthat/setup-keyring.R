# Keep test credentials in an environment backend and restore the caller's
# settings when the suite finishes, including interactive devtools runs.
withr::local_envvar(
  c(KEYRING_BACKEND = "env", KEYRING_PASS = ""),
  .local_envir = testthat::teardown_env()
)
