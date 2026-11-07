# R.cache treats dir as relative even when it looks absolute. Isolate its root
# for the whole suite so every test cache is removed, including nested paths.
local({
  directory <- tempfile("rkorap-test-cache-")
  withr::local_options(list(R.cache.rootPath = directory),
    .local_envir = testthat::teardown_env())
  withr::defer(unlink(directory, recursive = TRUE), envir = testthat::teardown_env())
})
