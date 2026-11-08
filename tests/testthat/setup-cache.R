# Isolate R.cache's root for the whole suite. Individual fixture caches use
# relative subdirectories and are removed when their test scope ends.
local({
  directory <- tempfile("rkorap-test-cache-")
  withr::local_options(list(R.cache.rootPath = directory),
    .local_envir = testthat::teardown_env())
  withr::defer(unlink(directory, recursive = TRUE), envir = testthat::teardown_env())
})
