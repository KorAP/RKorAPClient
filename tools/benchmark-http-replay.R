# Explicit developer benchmark, never part of package checks.
# Run from the package root with RKORAP_LIVE_TESTS=true.
benchmarkHttpReplay <- function() {
  if (!identical(Sys.getenv("RKORAP_LIVE_TESTS"), "true")) {
    stop("Set RKORAP_LIVE_TESTS=true to allow this benchmark to contact KorAP.")
  }
  devtools::load_all(quiet = TRUE)
  directory <- tempfile("korap-replay-benchmark-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rlang::local_options(timeout = 25, httr2_mock = getOption("httr2_mock"))
  vcr::local_vcr_configure(dir = directory, record = "none", serialize_with = "json",
    match_requests_on = c("method", "uri"), ignore_hosts = NULL,
    ignore_localhost = FALSE, turned_off = FALSE, re_record_interval = NULL)

  workflow <- function() {
    kco <- suppressMessages(KorAPConnection(accessToken = NULL, cache = FALSE,
      timeout = 15, verbose = FALSE))
    if (is.null(kco@welcome)) stop("KorAP is unavailable for this benchmark.")
    q <- corpusQuery(kco, "[orth='Nutzer*innen']",
      vc = "pubDate since 2023 & pubDate until 2025",
      fields = c("textSigle", "corpusTitle", "pubDate"))
    if (is.null(q@apiResponse) || isTRUE(q@apiResponse$meta$timeExceeded) ||
        !is.finite(q@totalResults) || q@totalResults < 51) {
      stop("The benchmark needs a complete query with at least 51 hits.")
    }
    result <- fetchNext(q, maxFetch = 51)
    stopifnot(nrow(result@collectedMatches) == 51)
    result@collectedMatches
  }
  timingRow <- function(mode, run, timing) {
    data.frame(mode = mode, run = run, elapsed = unname(timing["elapsed"]),
      cpu = unname(timing["user.self"] + timing["sys.self"]))
  }
  onlineTime <- system.time(
    vcr::use_cassette("benchmark", online <- workflow(), record = "all"))
  replay <- function() {
    # Bypass DNS reachability checks only during replay, and reject any actual
    # transfer. All request construction, parsing and pagination remain active.
    testthat::local_mocked_bindings(has_internet = function() TRUE,
      curl_fetch_memory = function(...) stop("Unexpected network transfer during replay."),
      .package = "curl")
    vcr::use_cassette("benchmark", result <- workflow(), record = "none")
    result
  }
  rows <- list(timingRow("live+record", 1, onlineTime))
  for (i in seq_len(3)) {
    timing <- system.time(result <- replay())
    stopifnot(identical(online, result))
    rows[[length(rows) + 1L]] <- timingRow("replay", i, timing)
  }
  report <- do.call(rbind, rows)
  print(report, row.names = FALSE)
  message("All three replays reproduced the same 51 matches; temporary recordings removed on exit.")
  output <- commandArgs(trailingOnly = TRUE)
  if (length(output)) write.csv(report, output[[1]], row.names = FALSE)
  invisible(report)
}

benchmarkHttpReplay()
