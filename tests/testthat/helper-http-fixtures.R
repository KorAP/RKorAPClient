# Scope all replacements to the callback. API parsing, URL construction and
# pagination run normally; only connectivity probing and transport are isolated.
withKorapFixtureApi <- function(code, cassette = "connection") {
  testthat::skip_if_not_installed("vcr", minimum_version = "2.0.0")
  state <- new.env(parent = emptyenv())
  state$requests <- character()
  state$missing <- character()
  state$transfers <- 0L
  testthat::local_mocked_bindings(
    has_internet = function() TRUE,
    curl_fetch_memory = function(...) {
      state$transfers <- state$transfers + 1L
      stop("Real HTTP transfers are disabled in fixture tests.")
    },
    .package = "curl"
  )
  vcr::local_vcr_configure(ignore_hosts = NULL, ignore_localhost = FALSE,
    turned_off = FALSE, re_record_interval = NULL)
  on.exit({
    # apiCall() intentionally handles HTTP failures, so a missing fixture must
    # also fail here instead of becoming an apparently legitimate NULL result.
    if (length(state$missing)) stop(paste("Unmatched fixture request:",
      paste(state$missing, collapse = "\n")), call. = FALSE)
    testthat::expect_identical(state$transfers, 0L)
  }, add = TRUE)
  perform <- RKorAPClient:::performRequest
  testthat::local_mocked_bindings(performRequest = function(req, verbose = FALSE) {
    state$requests <- c(state$requests, req$url)
    withCallingHandlers(perform(req, verbose = verbose), error = function(e) {
      if (inherits(e, "vcr_unhandled")) state$missing <- c(state$missing, req$url)
    })
  }, .package = "RKorAPClient")
  # Let the public httr2 interface restore any caller mock after vcr finishes.
  httr2::with_mocked_responses(NULL, {
    vcr::use_cassette(cassette, result <- code(state),
      dir = testthat::test_path("fixtures", "http-pilot"), record = "none",
      match_requests_on = c("method", "uri"), serialize_with = "json")
  })
  result
}

fixtureKorapConnection <- function(cache = FALSE, verbose = FALSE) {
  KorAPConnection(KorAPUrl = "https://korap.example.invalid/", accessToken = NULL,
    cache = cache, verbose = verbose)
}

korapHttpTestQuery <- function(kco, ...) {
  corpusQuery(kco, "[orth='Nutzer*innen']", vc = "pubDate since 2023 & pubDate until 2025",
    fields = c("textSigle", "corpusTitle", "pubDate"), ...)
}

# The same server-independent assertions run against synthetic cassettes and,
# when explicitly enabled, KorAP. Exact fixture counts are checked separately.
checkKorapHttpWorkflow <- function(kco) {
  testthat::expect_s4_class(kco, "KorAPConnection")
  testthat::expect_false(is.null(kco@welcome))
  testthat::expect_true(nzchar(kco@indexRevision))
  q <- korapHttpTestQuery(kco)
  testthat::expect_s4_class(q, "KorAPQuery")
  testthat::expect_true(is.finite(q@totalResults) && q@totalResults > 0)
  testthat::expect_false(isTRUE(q@apiResponse$meta$timeExceeded))
  testthat::expect_null(q@collectedMatches)
  result <- fetchNext(q, maxFetch = 51)
  matches <- result@collectedMatches
  testthat::expect_equal(nrow(matches), min(51, q@totalResults))
  testthat::expect_true(all(c("textSigle", "corpusTitle", "pubDate", "matchID",
    "matchStart", "matchEnd") %in% names(matches)))
  testthat::expect_equal(length(unique(matches$matchID)), nrow(matches))
  testthat::expect_true(all(nzchar(matches$textSigle)))
  testthat::expect_true(all(is.finite(matches$matchStart) & matches$matchStart <= matches$matchEnd))
  vc <- "pubDate since 2020 & pubDate until 2021"
  stats <- corpusStats(kco, vc)
  df <- corpusStats(kco, vc, as.df = TRUE)
  testthat::expect_s4_class(stats, "KorAPCorpusStats")
  testthat::expect_s3_class(df, "data.frame")
  sizes <- c(stats@documents, stats@tokens, stats@sentences, stats@paragraphs)
  testthat::expect_true(all(is.finite(sizes) & sizes > 0))
  testthat::expect_equal(unlist(df[c("documents", "tokens", "sentences", "paragraphs")],
    use.names = FALSE), sizes)
  testthat::expect_identical(httr2::url_parse(df$webUIRequestUrl)$query$cq, vc)
  metadata <- textMetadata(kco, matches$textSigle[1])
  testthat::expect_s3_class(metadata, "tbl_df")
  testthat::expect_identical(metadata$textSigle, matches$textSigle[1])
  testthat::expect_true(all(c("requestUrl", "webUIRequestUrl") %in% names(metadata)))
  testthat::expect_true(all(vapply(metadata, is.character, logical(1))))
  invisible(result)
}

# Use for algorithm tests which already replace their HTTP-dependent functions.
# Initialization still runs through the real constructor and a vcr cassette.
offlineKorapConnection <- function(...) {
  kco <- withKorapFixtureApi(function(state) suppressMessages(fixtureKorapConnection()))
  kco@authorizationSupported <- FALSE
  args <- list(...)
  if (!"apiUrl" %in% names(args) && any(c("KorAPUrl", "apiVersion") %in% names(args))) {
    root <- if (is.null(args$KorAPUrl)) kco@KorAPUrl else args$KorAPUrl
    if (!endsWith(root, "/")) root <- paste0(root, "/")
    args$KorAPUrl <- root
    args$apiUrl <- paste0(root, "api/", if (is.null(args$apiVersion)) kco@apiVersion else args$apiVersion, "/")
  }
  for (name in names(args)) methods::slot(kco, name) <- args[[name]]
  kco
}

withWorkflowFixture <- function(cassette, code) {
  directory <- tempfile("workflow-cache-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(KorAPCacheSubDir = function() directory, .package = "RKorAPClient")
  withKorapFixtureApi(code, cassette = cassette)
}

checkKorapQueryEncodings <- function(kco) {
  latinQuery <- "Ameisenb\xe4r"
  latinVc <- "pubPlace=N\xfcrnberg"
  Encoding(latinQuery) <- Encoding(latinVc) <- "latin1"
  utfQuery <- enc2utf8(latinQuery)
  utfVc <- enc2utf8(latinVc)
  latin <- corpusQuery(kco, latinQuery, vc = latinVc)
  utf <- corpusQuery(kco, utfQuery, vc = utfVc)
  fromUrl <- corpusQuery(kco, KorAPUrl = utf@webUIRequestUrl)
  testthat::expect_false(is.null(latin@apiResponse))
  testthat::expect_false(is.null(utf@apiResponse))
  testthat::expect_false(is.null(fromUrl@apiResponse))
  totals <- c(latin@totalResults, utf@totalResults, fromUrl@totalResults)
  testthat::expect_true(all(is.finite(totals) & totals >= 0))
  testthat::expect_equal(totals, rep(totals[1], 3))
  parameters <- httr2::url_parse(utf@webUIRequestUrl)$query
  testthat::expect_identical(parameters$q, utfQuery)
  testthat::expect_identical(parameters$cq, utfVc)
  invisible(totals)
}

checkKorapCollocationScores <- function(kco) {
  df <- collocationScoreQuery(kco, "Ameisenplage", "heimgesucht",
    leftContextSize = 0, rightContextSize = 1)
  testthat::expect_gt(df$logDice, 1)
  # Preserve the former live regression against the individual score functions.
  for (score in c("ll", "pmi", "mi2", "mi3", "logDice")) {
    testthat::expect_equal(df[[score]], get(score, envir = asNamespace("RKorAPClient"))(
      df$O1, df$O2, df$O, df$N, df$E, df$w))
  }
  invisible(df)
}

checkKorapContentWorkflow <- function(kco) {
  query <- "aufgrund einer Ameisenplage"
  vc <- "availability=/CC.*/"
  q <- corpusQuery(kco, query, vc, metadataOnly = FALSE, fields = c("textSigle", "snippet", "tokens"))
  testthat::expect_true(is.finite(q@totalResults) && q@totalResults > 0)
  q <- fetchNext(q, maxFetch = 1)
  testthat::expect_equal(nrow(q@collectedMatches), 1)
  annotated <- fetchAnnotations(q, foundry = "marmot", verbose = FALSE)@collectedMatches
  testthat::expect_true(all(c("pos", "lemma", "morph", "atokens") %in% names(annotated)))
  testthat::expect_true(all(lengths(annotated$pos$match) > 0))
  testthat::expect_equal(lengths(annotated$pos$match), lengths(annotated$tokens$match))
  testthat::expect_true(any(grepl("[|]", unlist(annotated$morph$match))))
  testthat::expect_warning(scores <- collocationAnalysis(kco, query, vc = vc,
    minOccur = 1, minObservedExpectedRatio = 0, leftContextSize = 3, rightContextSize = 3,
    searchHitsSampleLimit = 2, topCollocatesLimit = 1, exactFrequencies = FALSE), "access token")
  testthat::expect_s3_class(scores, "data.frame")
  testthat::expect_gt(nrow(scores), 0)
  testthat::expect_true(all(c("collocate", "O", "E", "logDice") %in% names(scores)))
  testthat::expect_true(all(is.finite(c(scores$O, scores$E, scores$logDice))))
  invisible(scores)
}

checkKorapDefaultMetadata <- function(kco) {
  q <- corpusQuery(kco, "Test", vc = "textType=/Zeit.*/ & pubDate in 2023")
  testthat::expect_true(is.finite(q@totalResults) && q@totalResults > 0)
  result <- fetchNext(q, maxFetch = 1)@collectedMatches
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true(all(c("corpusSigle", "corpusTitle", "textSigle", "pubDate",
    "pubPlace", "availability", "dmozDomain", "wikiDomain") %in% names(result)))
  testthat::expect_true(any(!is.na(result$dmozDomain) & nzchar(result$dmozDomain)))
  testthat::expect_true(all(nzchar(result$textSigle)))
}
