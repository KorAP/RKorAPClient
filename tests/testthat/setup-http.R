# Tests replay cassettes by default. Explicit integration runs may contact
# their services; local webfakes servers remain available in developer runs.
local({
  live <- identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true")
  models <- nzchar(trimws(Sys.getenv("RKORAP_LLM_MODELS", unset = "")))
  if (!live && !models) {
    nativeFetch <- curl::curl_fetch_memory
    blocked <- character()
    testthat::local_mocked_bindings(curl_fetch_memory = function(url, ...) {
      if (grepl("^https?://(127[.]0[.]0[.]1|localhost|\\[::1\\])(:[0-9]+)?(/|$)", url)) {
        return(nativeFetch(url, ...))
      }
      blocked <<- c(blocked, sub("\\?.*$", "", url))
      stop("External HTTP is disabled in offline tests; add a vcr cassette.", call. = FALSE)
    }, .package = "curl", .env = testthat::teardown_env())
    # apiCall() handles transport errors, so validate at teardown as well: an
    # unexpected request must not silently become a successful NULL-result test.
    withr::defer({
      if (length(blocked)) stop(paste("Unmocked external HTTP in offline tests:",
        paste(unique(blocked), collapse = "\n")), call. = FALSE)
    }, envir = testthat::teardown_env())
  }
})
