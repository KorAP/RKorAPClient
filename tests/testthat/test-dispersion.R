test_that("dispersion agrees with Gries' worked example and unequal part sizes", {
  scores <- dispersionAnalysis(c(1, 2, 3, 4, 5), c(9, 10, 10, 10, 11))
  expect_equal(scores$DP, .18)
  expect_equal(scores$DPnorm, .18 / .82)
  expect_equal(scores$JuillandD, .785, tolerance = .001)
  expect_equal(scores$CarrollD2, .938, tolerance = .001)
  expect_equal(scores$RosengrenS, .950, tolerance = .001)
  expect_equal(scores$range, 5)
  even <- dispersionAnalysis(c(9, 10, 11), c(9, 10, 11))
  expect_equal(unlist(even), c(DP = 0, DPnorm = 0, JuillandD = 1,
                              CarrollD2 = 1, RosengrenS = 1, range = 3))
  concentrated <- dispersionAnalysis(c(10, 0), c(10, 90))
  expect_equal(concentrated$DP, .9)
  expect_equal(concentrated$DPnorm, 1)
  expect_equal(concentrated$RosengrenS, .1)
})

test_that("custom functions replace or extend the dispersion measures", {
  custom <- function(frequencies, sizes) sum(frequencies) / sum(sizes)
  expect_equal(dispersionAnalysis(c(1, 0), c(10, 20), list(rate = custom))$rate, 1 / 30)
  scores <- dispersionAnalysis(c(2, 0), c(20, 10),
    c(defaultDispersionFunctions(), list(rate = custom)))
  expect_equal(scores$rate, 2 / 30)
  expect_equal(scores$range, 1)
  expect_error(dispersionAnalysis(c(1, 0), c(1, 1), list(custom)), "named list")
  expect_error(dispersionAnalysis(c(1, 0), c(1, 1), list(x = custom, x = custom)), "named list")
  expect_error(dispersionAnalysis(c(1, 0), c(1, 1), list(x = function(...) c(1, 2))), "one finite")
  expect_error(dispersionAnalysis(c(1, 0), c(1, 1), list(x = function(...) Inf)), "one finite")
})

test_that("undefined and invalid dispersion inputs are handled explicitly", {
  scores <- dispersionAnalysis(c(0, 0), c(10, 20))
  expect_true(all(is.na(scores[1:5])))
  expect_equal(scores$range, 0)
  expect_true(is.na(dispersionAnalysis(1, 10)$DPnorm))
  expect_equal(dispersionAnalysis(c(0, 1, 1), c(0, 1, 1)), dispersionAnalysis(c(1, 1), c(1, 1)))
  expect_error(dispersionAnalysis(c(1, 0), c(0, 10)), "no hits in zero")
  expect_error(dispersionAnalysis(c(1, NA), c(1, 1)), "finite")
  expect_error(dispersionAnalysis(c(-1, 2), c(1, 1)), "non-negative")
  expect_error(dispersionAnalysis(1, c(1, 1)), "matching")
  expect_error(dispersionAnalysis(c(0, 0), c(0, 0)), "positive total")
})

dispersionTestConnection <- function() {
  testthat::local_mocked_bindings(apiCall = function(...) NULL, .package = "RKorAPClient")
  suppressMessages(KorAPConnection(KorAPUrl = "https://example.invalid/", accessToken = NULL, verbose = FALSE, cache = FALSE))
}

dispersionTestPartition <- function(kco, by = "corpusTitle", values = c("a", "b"),
                                    tokens = c(100, 100), baseVc = "") {
  p <- tibble::tibble(part = values, value = values,
    vc = paste0(by, '="', values, '"'), tokens = tokens)
  class(p) <- c("KorAPCorpusPartition", class(p))
  attr(p, "partition") <- list(by = by, fields = by, year = by == "pubDate",
    classify = NULL, canClassify = TRUE, baseVc = baseVc, unionVc = "union",
    baseTokens = sum(tokens), coveredTokens = sum(tokens), excludedTokens = 0,
    apiUrl = kco@apiUrl, indexRevision = kco@indexRevision)
  p
}

dispersionTestQuery <- function(kco, n, fields = "corpusTitle") {
  KorAPQuery(korapConnection = kco, totalResults = n, fields = fields,
    apiResponse = list(meta = list(totalResults = n)))
}

test_that("partitions quote values, preserve labels and check overlap and coverage", {
  kco <- dispersionTestConnection()
  vcs <- list()
  testthat::local_mocked_bindings(corpusStats = function(kco, vc, ...) {
    vcs[[length(vcs) + 1]] <<- vc
    tibble::tibble(tokens = if (length(vc) == 2) c(80, 20) else 100)
  }, .package = "RKorAPClient")
  p <- corpusPartition(kco, "source", c(first = 'a "quoted" title', second = "b"), vc = "textType=newspaper")
  expect_equal(p$part, c("first", "second"))
  expect_equal(p$tokens, c(80, 20))
  expect_match(p$vc[1], '\\"quoted\\"', fixed = TRUE)
  expect_match(p$vc[1], "(textType=newspaper) & (", fixed = TRUE)
  y <- corpusPartition(kco, "year", 2020:2021)
  expect_equal(y$vc, c("pubDate in 2020", "pubDate in 2021"))
  expect_true(attr(y, "partition")$year)
  expect_error(corpusPartition(kco, "source", c("same", "same")), "distinct values")
  testthat::local_mocked_bindings(corpusStats = function(kco, vc, ...) {
    tibble::tibble(tokens = if (length(vc) == 2) c(80, 40) else 100)
  }, .package = "RKorAPClient")
  expect_error(corpusPartition(kco, "region", c("DE", "AT")), "overlap")
})

test_that("exact dispersion counts retain zero-hit parts without fetching matches", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, query, vc, as.df = FALSE, ...) {
      if (!as.df) dispersionTestQuery(kco, 10) else tibble::tibble(totalResults = c(10, 0))
    },
    fetchNext = function(...) stop("must not download matches"),
    .package = "RKorAPClient")
  result <- dispersionQuery(kco, "word", p)
  expect_false(result$estimated)
  expect_equal(result$sampledHits, 0)
  expect_equal(result$DP, .5)
  expect_equal(result$distribution[[1]]$observed, c(10, 0))
  expect_equal(result$range, 1)
  expect_equal(result$excludedHits, 0)
  expect_match(result$distribution[[1]]$webUIRequestUrl[1], "q=word", fixed = TRUE)
})

test_that("one random page sample serves multiple metadata dimensions and restores RNG", {
  kco <- dispersionTestConnection()
  source <- dispersionTestPartition(kco)
  year <- dispersionTestPartition(kco, "pubDate", c("2020", "2021"))
  pages <- numeric()
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, query, fields, ...) dispersionTestQuery(kco, 125, fields),
    fetchNext = function(kqo, offset, maxFetch, ...) {
      pages <<- c(pages, offset / 50)
      rows <- seq.int(offset + 1, min(offset + 50, kqo@totalResults))
      m <- tibble::tibble(corpusTitle = ifelse(rows <= 50, "a", "b"),
        pubDate = ifelse(rows <= 100, "20200101", "2021-01-01"))
      kqo@collectedMatches <- dplyr::bind_rows(kqo@collectedMatches, m)
      kqo
    }, .package = "RKorAPClient")
  set.seed(123)
  before <- .Random.seed
  expect_warning(result <- dispersionQuery(kco, "word", list(source = source, year = year),
    exactFrequencies = FALSE, searchHitsSampleLimit = 100, seed = 7), "pages, not independent hits")
  expect_identical(.Random.seed, before)
  expect_equal(nrow(result), 2)
  expect_length(pages, 2)
  expect_length(unique(pages), 2)
  expect_true(all(result$estimated))
  expect_true(all(result$rangeIsLowerBound))
  expect_true(all(result$sampledHits <= 100))
  expect_equal(result$sampling[[1]]$pages, result$sampling[[2]]$pages)
  expect_equal(result$sampling[[1]]$inclusionProbability, 2 / 3)
  expect_equal(result$distribution[[1]]$frequency,
               result$distribution[[1]]$observed * 3 / 2)
  expect_equal(result$sampledHits, rep(sum(result$distribution[[1]]$observed), 2))
  expect_warning(again <- dispersionQuery(kco, "word", source, exactFrequencies = FALSE,
                            searchHitsSampleLimit = 100, seed = 7), "pages, not independent hits")
  expect_identical(result$distribution[[1]], again$distribution[[1]])
})

test_that("full metadata, missing metadata, and no hits remain distinguishable", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, query, ...) dispersionTestQuery(kco, if (query == "absent") 0 else 3),
    fetchNext = function(kqo, ...) {
      kqo@collectedMatches <- tibble::tibble(corpusTitle = c("a", NA, "outside"))
      kqo
    }, .package = "RKorAPClient")
  result <- dispersionQuery(kco, "word", p, exactFrequencies = FALSE, searchHitsSampleLimit = Inf)
  expect_false(result$estimated)
  expect_equal(result$excludedHits, 2)
  expect_equal(result$usedHits, 1)
  expect_equal(result$range, 1)
  empty <- dispersionQuery(kco, "absent", p, exactFrequencies = FALSE)
  expect_equal(empty$totalResults, 0)
  expect_equal(empty$range, 0)
  expect_true(is.na(empty$DP))
})

test_that("incomplete requests and incompatible partitions cannot yield scores", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, ...) dispersionTestQuery(kco, 100),
    fetchNext = function(kqo, ...) {
      kqo@collectedMatches <- tibble::tibble(corpusTitle = "a")
      kqo
    }, .package = "RKorAPClient")
  expect_error(dispersionQuery(kco, "word", p, exactFrequencies = FALSE, searchHitsSampleLimit = 50), "incomplete")
  expect_error(dispersionQuery(kco, "word", p, exactFrequencies = FALSE, searchHitsSampleLimit = 10), ">= 50")
  expect_error(dispersionQuery(kco, "word", list(a = p, b = dispersionTestPartition(kco, baseVc = "other"))), "same base")
  bad <- p
  attr(bad, "partition")$indexRevision <- "different"
  expect_error(dispersionQuery(kco, "word", bad), "index revision")
  testthat::local_mocked_bindings(corpusQuery = function(...) {
    RKorAPClient:::warnIncompleteResults("timeExceeded: incomplete search")
    dispersionTestQuery(kco, 0)
  }, .package = "RKorAPClient")
  expect_error(dispersionQuery(kco, "word", p), "complete requests")
})

test_that("multivalued metadata require explicit membership rules", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  spec <- attr(p, "partition")
  classify <- RKorAPClient:::classifyDispersionMatches
  expect_error(classify(tibble::tibble(corpusTitle = list(c("a", "b"))), p, spec), "Multivalued")
  spec$classify <- function(matches) rep("b", nrow(matches))
  expect_equal(classify(tibble::tibble(corpusTitle = list(c("a", "b"))), p, spec), "b")
  spec$classify <- function(matches) "unknown"
  expect_error(classify(tibble::tibble(corpusTitle = "a"), p, spec), "known part")
})

test_that("frequencyQuery opts in to dispersion and forwards custom scores", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  received <- NULL
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, query, vc, ...) tibble::tibble(query = query,
      vc = vc, totalResults = 2),
    corpusStats = function(...) tibble::tibble(tokens = 200),
    dispersionQuery = function(kco, query, partitions, scoreFunctions, ...) {
      received <<- scoreFunctions
      tibble::tibble(custom = scoreFunctions$custom(c(2, 0), c(100, 100)))
    }, .package = "RKorAPClient")
  expect_false("dispersion" %in% names(frequencyQuery(kco, "word")))
  custom <- list(custom = function(frequencies, sizes) sum(frequencies))
  result <- frequencyQuery(kco, "word", dispersionBy = p, dispersionFunctions = custom)
  expect_identical(received, custom)
  expect_equal(result$dispersion[[1]]$custom, 2)
  expect_error(frequencyQuery(kco, "word", vc = "other", dispersionBy = p), "base corpus")
})

test_that("the new measures reproduce the historical publication's results", {
  data <- utils::read.csv(testthat::test_path("fixtures", "dispersion-2023.csv"), check.names = FALSE)
  result <- lapply(data[c("NutzerInnen", "Nutzer*innen", "Nutzer:innen")], function(v)
    dispersionAnalysis(v, data$tokens))
  expect_equal(vapply(result, function(x) x$JuillandD, numeric(1)),
    c(NutzerInnen = .229419374, `Nutzer*innen` = .406009103, `Nutzer:innen` = .561038331), tolerance = 1e-7)
  expect_equal(round(vapply(result, function(x) x$CarrollD2, numeric(1)), 3),
    c(NutzerInnen = .280, `Nutzer*innen` = .295, `Nutzer:innen` = .454))
  expect_equal(round(vapply(result, function(x) 1 - x$DPnorm, numeric(1)), 3),
    c(NutzerInnen = .173, `Nutzer*innen` = .106, `Nutzer:innen` = .119))
})

test_that("dispersion uses the real API parsing and pagination without snippets", {
  kco <- dispersionTestConnection()
  offsets <- numeric()
  api <- function(kco, url, ...) {
    u <- httr2::url_parse(url)
    if (grepl("statistics$", u$path)) {
      tokens <- if (identical(u$query$cq, 'corpusTitle = "a"')) 80 else
        if (identical(u$query$cq, 'corpusTitle = "b"')) 20 else 100
      return(list(tokens = tokens, documents = 1, sentences = 10, paragraphs = 5))
    }
    expect_match(u$path, "search$")
    expect_false("snippet" %in% strsplit(u$query$fields, ",", fixed = TRUE)[[1]])
    n <- if (identical(u$query$cq, 'corpusTitle = "a"')) 50L else
      if (identical(u$query$cq, 'corpusTitle = "b"')) 1L else 51L
    offset <- if (is.null(u$query$offset)) 0 else as.numeric(u$query$offset)
    count <- as.numeric(u$query$count)
    rows <- if (count == 0) integer() else seq.int(offset + 1L, min(offset + count, n))
    matches <- if (!length(rows)) list() else data.frame(
      textSigle = paste0("T/", rows), corpusTitle = ifelse(rows <= 50, "a", "b"),
      matchID = paste0("match-T/", rows, "-p0-1"), stringsAsFactors = FALSE)
    if (count > 0) offsets <<- c(offsets, offset)
    list(meta = list(totalResults = n, startIndex = offset,
                    itemsPerPage = length(rows)), matches = matches)
  }
  testthat::local_mocked_bindings(apiCall = api, .package = "RKorAPClient")
  p <- corpusPartition(kco, "source", c("a", "b"))
  exact <- dispersionQuery(kco, "word", p)
  complete <- dispersionQuery(kco, "word", p, exactFrequencies = FALSE, searchHitsSampleLimit = Inf)
  expect_equal(complete$distribution[[1]]$observed, c(50, 1))
  expect_equal(complete$DP, exact$DP)
  expect_equal(offsets, c(0, 50))
  expect_equal(complete$sampledHits, 51)
  expect_false(complete$estimated)
})

test_that("partition coverage, custom classifiers and result caches are explicit", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(corpusStats = function(kco, vc, ...) {
    tibble::tibble(tokens = if (length(vc) == 2) c(30, 20) else if (vc == "") 100 else 50)
  }, .package = "RKorAPClient")
  expect_warning(p <- corpusPartition(kco, "source", c("a", "b")), "excludes 50")
  expect_equal(attr(p, "partition")$excludedTokens, 50)
  attr(p, "partition")$canClassify <- FALSE
  expect_error(dispersionQuery(kco, "word", p, exactFrequencies = FALSE), "classify function")
  p <- dispersionTestPartition(kco)
  calls <- 0
  testthat::local_mocked_bindings(corpusQuery = function(kco, query, vc, as.df = FALSE, ...) {
    calls <<- calls + 1
    if (as.df) tibble::tibble(totalResults = c(10, 0)) else dispersionTestQuery(kco, 10)
  }, .package = "RKorAPClient")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  first <- dispersionQuery(kco, "word", p, cacheAs = file)
  n <- calls
  expect_equal(dispersionQuery(kco, "word", p, cacheAs = file), first)
  expect_equal(calls, n)
  expect_warning(changed <- dispersionQuery(kco, "word", p, cacheAs = file,
    scoreFunctions = list(evenness = function(frequencies, sizes) 1 - dispersionDP(frequencies, sizes))), "scoreFunctions")
  expect_gt(calls, n)
  expect_equal(changed$evenness, .5)
})

test_that("the fields API preserves multivalued categories for explicit classifiers", {
  kco <- dispersionTestConnection()
  p <- dispersionTestPartition(kco)
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    u <- httr2::url_parse(url)
    matches <- if (as.numeric(u$query$count) == 0) list() else
      tibble::tibble(matchID = "match-T/1-p0-1", fields = list(tibble::tibble(
        key = c("corpusTitle", "textSigle"),
        value = list(c("a", "b"), "T/1"))))
    list(meta = list(totalResults = 1L, startIndex = 0L,
                    itemsPerPage = if (is.data.frame(matches)) nrow(matches) else 0), matches = matches)
  }, .package = "RKorAPClient")
  expect_error(dispersionQuery(kco, "word", p, exactFrequencies = FALSE), "Multivalued")
  attr(p, "partition")$classify <- function(matches) {
    expect_equal(matches$corpusTitle[[1]], c("a", "b"))
    "a"
  }
  result <- dispersionQuery(kco, "word", p, exactFrequencies = FALSE)
  expect_equal(result$distribution[[1]]$observed, c(1, 0))
})

test_that("empty filters and failed size requests have distinct diagnostics", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(corpusStats = function(...) {
    tibble::tibble(tokens = 0)
  }, .package = "RKorAPClient")
  expect_error(corpusPartition(kco, "source", "Falter", vc = 'corpusTitle="Falter"'),
               "base virtual corpus is empty.*corpusTitle")
  testthat::local_mocked_bindings(corpusStats = function(kco, vc, ...) {
    tibble::tibble(tokens = if (identical(vc, "")) 100 else 0)
  }, .package = "RKorAPClient")
  expect_error(corpusPartition(kco, "source", "Falter"),
               "None of the listed corpus parts.*publication years")
  testthat::local_mocked_bindings(corpusStats = function(...) {
    tibble::tibble(tokens = NA_real_)
  }, .package = "RKorAPClient")
  expect_error(corpusPartition(kco, "year", 2020:2021),
               "Could not retrieve token counts for the base virtual corpus")
})

test_that("annual corpus titles can be grouped into sources", {
  kco <- dispersionTestConnection()
  sources <- c(taz = 'corpusTitle=/die tageszeitung.*/', Falter = 'corpusTitle=/Falter.*/')
  testthat::local_mocked_bindings(corpusStats = function(kco, vc, ...) {
    tibble::tibble(tokens = if (length(vc) == 2) c(80, 20) else 100)
  }, .package = "RKorAPClient")
  p <- corpusPartition(kco, "source", parts = sources, vc = "pubDate since 2020",
    classify = function(matches) {
      titles <- as.character(matches$corpusTitle)
      ifelse(startsWith(titles, "die tageszeitung"), "taz",
             ifelse(startsWith(titles, "Falter"), "Falter", NA_character_))
    })
  expect_equal(p$part, c("taz", "Falter"))
  expect_equal(p$tokens, c(80, 20))
  matches <- tibble::tibble(corpusTitle = list("die tageszeitung 2020", "die tageszeitung 2021", "Falter 2020"))
  expect_equal(RKorAPClient:::classifyDispersionMatches(matches, p, attr(p, "partition")),
               c("taz", "taz", "Falter"))
  union <- attr(p, "partition")$unionVc
  expect_length(regmatches(union, gregexpr("pubDate since 2020", union, fixed = TRUE))[[1]], 1)
})

test_that("unavailable corpus statistics remain numeric with verbose output", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(...) NULL, .package = "RKorAPClient")
  expect_output(stats <- corpusStats(kco, vc = "pubDate in 2020", as.df = TRUE, verbose = TRUE),
                "NA tokens")
  expect_type(stats$tokens, "double")
  expect_true(is.na(stats$tokens))
})

automaticDispersionMocks <- function(kco) {
  state <- new.env(parent = emptyenv())
  state$queryCalls <- state$probes <- state$hitFetches <- 0L
  query <- function(kco, query, vc = "base", fields = "textSigle", as.df = FALSE, ...) {
    if (as.df) return(tibble::tibble(totalResults = c(2, 0)))
    if (query == "<base/s=t>") {
      state$probes <- state$probes + 1L
      titles <- c("A 2020", "B 2021")
      compact <- stringr::str_match(vc, "corpusTitle != /([^/]+)/")[, 2]
      if (!is.na(compact)) titles <- titles[!grepl(paste0("^", compact, "$"), titles)]
      if (grepl("corpusTitle != /A(", vc, fixed = TRUE) || grepl('corpusTitle != "A 2020"', vc, fixed = TRUE)) titles <- setdiff(titles, "A 2020")
      if (grepl("corpusTitle != /B(", vc, fixed = TRUE) || grepl('corpusTitle != "B 2021"', vc, fixed = TRUE)) titles <- setdiff(titles, "B 2021")
      n <- as.integer(length(titles) > 0)
      request <- list(representative = if (n) titles[1] else NULL)
    } else {
      state$queryCalls <- state$queryCalls + 1L
      n <- 2L
      request <- query
    }
    KorAPQuery(korapConnection = kco, vc = vc, totalResults = n,
      request = request, fields = unique(c(fields, "textSigle")),
      requestUrl = httr2::url_modify("https://example.invalid/search",
        query = list(q = query, ql = "poliqarp", fields = paste(fields, collapse = ","))),
      apiResponse = list(meta = list(totalResults = n)))
  }
  fetch <- function(kqo, ...) {
    if (is.list(kqo@request)) {
      kqo@collectedMatches <- tibble::tibble(corpusTitle = list(kqo@request$representative), textSigle = "representative")
    } else {
      state$hitFetches <- state$hitFetches + 1L
      kqo@collectedMatches <- tibble::tibble(corpusTitle = list("A 2020", "A 2020"),
        textSigle = c("T/1", "T/2"), matchID = c("one", "two"))
    }
    kqo
  }
  sizes <- function(kco, vc, ...) tibble::tibble(tokens = if (length(vc) == 2) c(80, 20) else 100)
  list(corpusQuery = query, fetchNext = fetch, corpusStats = sizes, state = state)
}

test_that("query-result dispersion discovers zero-hit sources without a second word query", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  q <- m$corpusQuery(kco, "word", vc = "base")
  result <- dispersionAnalysis(q, by = "source", exactFrequencies = FALSE)
  expect_s3_class(result, "KorAPDispersion")
  expect_equal(m$state$queryCalls, 1)
  expect_equal(m$state$hitFetches, 1)
  expect_equal(result$nParts, 2)
  expect_equal(result$range, 1)
  expect_equal(result$DP, .2)
  expect_equal(result$distribution[[1]]$part, c("A", "B"))
  expect_equal(result$distribution[[1]]$observed, c(2, 0))
  expect_equal(result$distribution[[1]]$tokens, c(80, 20))
  expect_equal(attr(result, "partitions")$source$part, c("A", "B"))
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "DPnorm")
  expect_match(printed, "CarrollD2")
  expect_match(printed, "RosengrenS")
  expect_false(grepl("distribution", printed, fixed = TRUE))
})

test_that("full query metadata can be reused and custom functions remain replaceable", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  q <- m$corpusQuery(kco, "word", fields = "corpusTitle")
  q <- m$fetchNext(q)
  before <- m$state$hitFetches
  result <- dispersionAnalysis(q, by = "corpusTitle", regex = " [0-9]{4}$",
    scoreFunctions = list(zeroParts = function(frequencies, sizes) sum(frequencies == 0)))
  expect_equal(result$zeroParts, 1)
  expect_equal(m$state$hitFetches, before)
  expect_equal(m$state$queryCalls, 1)
  expect_equal(result$sampling[[1]]$method, "existing_metadata")
  expect_equal(q@collectedMatches$corpusTitle[[1]], "A 2020")
})

test_that("automatic partitions are complete even without hit-derived seeds", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  p <- corpusPartition(kco, "source", vc = "base")
  expect_equal(p$part, c("A", "B"))
  expect_equal(m$state$probes, 3)
  result <- dispersionQuery(kco, "word", vc = "base", by = "source")
  expect_equal(result$DP, .2)
  expect_equal(result$nParts, 2)
  expect_false(result$estimated)
  expect_equal(m$state$hitFetches, 0)
})

test_that("automatic grouping rules preserve corpus-query and metadata membership", {
  dimension <- RKorAPClient:::dispersionDimension("source")
  expect_equal(RKorAPClient:::dispersionGroupValues(c("A 2020", "A 2021", "A", NA), dimension),
    c("A", "A", "A", NA))
  condition <- RKorAPClient:::dispersionGroupCondition("A.B", dimension)
  expect_equal(condition, 'corpusTitle = /A\\.B( [0-9]{4})?/')
  expect_error(RKorAPClient:::dispersionDimension("corpusTitle", "[0-9]+"), "trailing suffix")
  expect_error(RKorAPClient:::dispersionDimension("corpusTitle", ".*$"), "empty string")
  expect_error(RKorAPClient:::dispersionGroupValues("2020",
    RKorAPClient:::dispersionDimension("corpusTitle", "[0-9]{4}$")), "entire metadata")
  expect_error(RKorAPClient:::dispersionGroupValues(list(c("a", "b")), dimension), "single metadata value")
})

test_that("automatic inventory caches depend on the corpus and grouping rule", {
  old <- options(R.cache.rootPath = tempfile("dispersion-inventory-cache-"))
  on.exit(options(old), add = TRUE)
  kco <- dispersionTestConnection()
  kco@cache <- TRUE
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  first <- corpusPartition(kco, "source", vc = "base")
  before <- m$state$probes
  expect_equal(corpusPartition(kco, "source", vc = "base"), first)
  expect_equal(m$state$probes, before)
  corpusPartition(kco, "corpusTitle", vc = "base")
  expect_gt(m$state$probes, before)
})

test_that("query-result cache records do not serialize connection credentials", {
  kco <- dispersionTestConnection()
  kco@accessToken <- "private-token-for-test"
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  q <- m$corpusQuery(kco, "word")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  first <- dispersionAnalysis(q, cacheAs = file)
  calls <- m$state$probes
  expect_equal(dispersionAnalysis(q, cacheAs = file), first)
  expect_equal(m$state$probes, calls)
  record <- cacheAsInfo(file)
  expect_equal(record$parameters$frequencies, list(query = "word", vc = "base", ql = "poliqarp"))
  expect_false(grepl("private-token-for-test", paste(capture.output(str(record)), collapse = "\n"), fixed = TRUE))
})

test_that("an access-rewritten text query cannot use unrestricted token denominators", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  q <- m$corpusQuery(kco, "word")
  q@apiResponse$collection <- list(rewrites = data.frame(scope = "availability(PUB)"))
  expect_error(dispersionAnalysis(q), "restricted by access rights.*metadataOnly = TRUE")
})

test_that("public page samples select whole pages and preserve caller randomness", {
  kco <- dispersionTestConnection()
  q <- dispersionTestQuery(kco, 1000)
  offsets <- numeric()
  testthat::local_mocked_bindings(fetchNext = function(kqo, offset, maxFetch, ...) {
    offsets <<- c(offsets, offset)
    rows <- seq.int(offset + 1, offset + maxFetch)
    kqo@collectedMatches <- dplyr::bind_rows(kqo@collectedMatches,
      tibble::tibble(matchID = as.character(rows), corpusTitle = "a"))
    kqo
  }, .package = "RKorAPClient")
  set.seed(392)
  before <- .Random.seed
  expect_warning(sample <- fetchSamplePages(q, maxHits = 50, seed = 1), "pages, not independent hits")
  expect_identical(.Random.seed, before)
  expect_equal(offsets, 150)
  expect_equal(sample@collectedMatches$matchID, as.character(151:200))
  provenance <- attr(sample, "korapSampling")
  expect_equal(provenance$pages, 3)
  expect_equal(provenance$inclusionProbability, 1 / 20)
  expect_equal(provenance$hitOrder, "index")
  expect_equal(provenance$sampleSize, 50)
  expect_equal(provenance$pageSize, 50)
  rounded <- fetchSamplePages(q, maxHits = 99, seed = 1, warn = FALSE)
  expect_equal(nrow(rounded@collectedMatches), 50)
  expect_error(fetchSamplePages(q, maxHits = 49, warn = FALSE), ">= 50")
  expect_error(fetchSamplePages(q, maxHits = 0), "positive integer")
  expect_error(fetchSamplePages(q, seed = NA), "non-negative integer")
})

test_that("complete page retrieval includes the short last page without duplicates", {
  kco <- dispersionTestConnection()
  q <- dispersionTestQuery(kco, 125, fields = "corpusTitle")
  q@requestUrl <- "https://example.invalid/search?q=word&ql=poliqarp&fields=corpusTitle"
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    parameters <- httr2::url_parse(url)$query
    offset <- as.integer(parameters$offset)
    rows <- seq.int(offset + 1, min(offset + 50, 125))
    list(meta = list(totalResults = 125L, startIndex = offset, itemsPerPage = length(rows)),
      matches = tibble::tibble(matchID = as.character(rows), corpusTitle = "a"))
  }, .package = "RKorAPClient")
  expect_message(complete <- fetchSamplePages(q, maxHits = 200), "complete results.*no random sample")
  expect_equal(complete@collectedMatches$matchID, as.character(1:125))
  expect_equal(attr(complete, "korapSampling")$pages, 0:2)
  sample <- fetchSamplePages(q, maxHits = 50, warn = FALSE)
  expect_error(fetchNext(sample), "not a sequential prefix")
  expect_error(fetchRest(sample), "not a sequential prefix")
  all <- fetchAll(sample)
  expect_null(attr(all, "korapSampling"))
  expect_equal(all@collectedMatches$matchID, as.character(1:125))
  prefix <- fetchNext(q)
  expect_equal(fetchAll(prefix)@collectedMatches$matchID, as.character(1:125))
})

test_that("dispersion reuses recorded pages and enriches only those pages", {
  kco <- dispersionTestConnection()
  q <- dispersionTestQuery(kco, 1000, "textSigle")
  q@requestUrl <- "https://example.invalid/search?q=word&ql=poliqarp&fields=textSigle"
  offsets <- numeric()
  testthat::local_mocked_bindings(fetchNext = function(kqo, offset, maxFetch, ...) {
    offsets <<- c(offsets, offset)
    m <- tibble::tibble(textSigle = paste0("T/", seq.int(offset + 1, offset + maxFetch)))
    if ("corpusTitle" %in% kqo@fields) m$corpusTitle <- "a"
    kqo@collectedMatches <- dplyr::bind_rows(kqo@collectedMatches, m)
    kqo
  }, .package = "RKorAPClient")
  sample <- fetchSamplePages(q, maxHits = 50, seed = 1, warn = FALSE)
  reused <- RKorAPClient:::sampleDispersionMetadata(sample, Inf, 99, FALSE)
  expect_equal(offsets, 150)
  expect_equal(reused$matches, sample@collectedMatches)
  enriched <- RKorAPClient:::prepareDispersionMetadata(sample, "corpusTitle")
  retrieved <- RKorAPClient:::sampleDispersionMetadata(enriched, Inf, 99, FALSE)
  expect_equal(offsets, c(150, 150))
  expect_equal(retrieved$pages, 3)
  expect_equal(retrieved$inclusionProbability, .05)
  expect_equal(retrieved$matches$textSigle, sample@collectedMatches$textSigle)
  expect_equal(retrieved$matches$corpusTitle, rep("a", 50))
})

test_that("vector queries retain fetchable states and preserve expanded labels", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(...) list(meta = list(totalResults = 2L)),
    .package = "RKorAPClient")
  q <- corpusQuery(kco, c(star = "one", colon = "two"), vc = c("first", "second"), expand = TRUE)
  expect_equal(q$query, c("one", "one", "two", "two"))
  expect_equal(attr(q, "korapQueryLabels"), c("star", "star", "colon", "colon"))
  expect_length(RKorAPClient:::korapQueryObjects(q), 4)
  summary <- corpusQuery(kco, c("one", "two"), as.df = TRUE)
  expect_null(attr(summary, "korapQueries"))
  expect_error(fetchSamplePages(summary), "unmodified multi-query result")
  expect_error(fetchSamplePages(q[1:2, ]), "unmodified multi-query result")
  q$query[1] <- "changed"
  expect_error(fetchSamplePages(q), "no longer match")
})

test_that("two fetched queries share one source basis and print two score columns", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    expression <- httr2::url_parse(url)$query$q
    list(meta = list(totalResults = if (expression == "one") 2L else 3L))
  }, .package = "RKorAPClient")
  queries <- corpusQuery(kco, c("Nutzer*innen" = "one", "Nutzer:innen" = "two"),
    vc = "base", fields = c("corpusTitle", "textSigle"))
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, corpusStats = m$corpusStats,
    fetchNext = function(kqo, ...) {
      if (is.list(kqo@request)) return(m$fetchNext(kqo, ...))
      m$state$hitFetches <- m$state$hitFetches + 1L
      expression <- httr2::url_parse(kqo@requestUrl)$query$q
      titles <- if (expression == "one") c("A 2020", "A 2020") else c("A 2020", "B 2021", "B 2021")
      kqo@collectedMatches <- tibble::tibble(corpusTitle = as.list(titles),
        textSigle = paste0("T/", seq_along(titles)))
      kqo
    }, .package = "RKorAPClient")
  expect_message(fetched <- fetchSamplePages(queries), "complete results")
  expect_equal(m$state$hitFetches, 2)
  result <- dispersionAnalysis(fetched)
  expect_equal(m$state$hitFetches, 2)
  expect_equal(m$state$queryCalls, 0)
  expect_equal(m$state$probes, 1)
  expect_equal(result$variant, c("Nutzer*innen", "Nutzer:innen"))
  expect_equal(result$DP, c(.2, 7 / 15))
  expect_equal(result$distribution[[1]]$tokens, result$distribution[[2]]$tokens)
  expect_equal(result$distribution[[1]]$part, c("A", "B"))
  expect_equal(result$distribution[[1]]$observed, c(2, 0))
  expect_equal(result$distribution[[2]]$observed, c(1, 2))
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "Nutzer*innen", fixed = TRUE)
  expect_match(printed, "Nutzer:innen", fixed = TRUE)
  expect_match(printed, "DPnorm")
  expect_match(printed, "CarrollD2")
  expect_false(any(result$estimated))
  expect_equal(result$sampling[[1]]$hitOrder, "index")
  expect_equal(result$sampling[[1]]$pageSize, 50)
})

test_that("cached analyses distinguish fetched metadata and retain no credentials", {
  kco <- dispersionTestConnection()
  kco@accessToken <- "private-token-for-test"
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery,
    fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  sample <- fetchSamplePages(m$corpusQuery(kco, "word", fields = "corpusTitle"), warn = FALSE)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  first <- dispersionAnalysis(sample, cacheAs = file)
  probes <- m$state$probes
  expect_equal(dispersionAnalysis(sample, cacheAs = file), first)
  expect_equal(m$state$probes, probes)
  sample@collectedMatches$corpusTitle[[2]] <- "B 2021"
  expect_warning(changed <- dispersionAnalysis(sample, cacheAs = file), "fetchedInput")
  expect_equal(changed$DP, .3)
  expect_equal(changed$range, 2)
  info <- cacheAsInfo(file)
  expect_equal(info$parameters$fetchedInput$sampling$sampleSize, 2)
  expect_false(grepl("private-token-for-test", paste(capture.output(str(info)), collapse = "\n"), fixed = TRUE))
})

test_that("source, country and calendar years reuse one fetched set per spelling", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    expression <- httr2::url_parse(url)$query$q
    list(meta = list(totalResults = if (expression == "one") 2L else 3L))
  }, .package = "RKorAPClient")
  queries <- corpusQuery(kco, c(star = "one", colon = "two"), vc = "base",
    fields = c("corpusTitle", "pubPlaceKey", "pubDate"))
  records <- tibble::tibble(corpusTitle = c("A 2020", "B 2021", "C 2022"),
    pubPlaceKey = c("AT", "DE", "CH"), pubDate = c("20200101", "2021-01-01", "20221231"))
  dimensions <- lapply(c("source", "pubPlaceKey", "year"), RKorAPClient:::dispersionDimension)
  names(dimensions) <- c("corpusTitle", "pubPlaceKey", "pubDate")
  labels <- lapply(dimensions, function(d) RKorAPClient:::dispersionGroupValues(records[[d$field]], d))
  probes <- fetches <- 0L
  testthat::local_mocked_bindings(
    corpusQuery = function(kco, query, vc, fields, ...) {
      if (query != "<base/s=t>") stop("must not repeat the term query")
      probes <<- probes + 1L
      d <- dimensions[[fields[1]]]
      excluded <- vapply(labels[[d$field]], function(label) grepl(
        RKorAPClient:::dispersionGroupCondition(label, d, exclude = TRUE), vc, fixed = TRUE), logical(1))
      if (!d$year) {
        compact <- stringr::str_match(vc, paste0(d$field, " != /([^/]+)/"))[, 2]
        if (!is.na(compact)) excluded <- grepl(paste0("^", compact, "$"), records[[d$field]])
      }
      remaining <- which(!excluded)
      q <- dispersionTestQuery(kco, as.integer(length(remaining) > 0), fields)
      q@request <- list(representative = if (length(remaining)) remaining[1] else NULL)
      q
    },
    fetchNext = function(kqo, ...) {
      if (is.list(kqo@request)) {
        kqo@collectedMatches <- records[kqo@request$representative, , drop = FALSE]
      } else {
        fetches <<- fetches + 1L
        expression <- httr2::url_parse(kqo@requestUrl)$query$q
        rows <- if (expression == "one") c(1, 1) else c(1, 2, 2)
        kqo@collectedMatches <- records[rows, , drop = FALSE]
      }
      kqo@collectedMatches$textSigle <- paste0("T/", seq_len(nrow(kqo@collectedMatches)))
      kqo
    },
    corpusStats = function(kco, vc, ...) {
      sizes <- vapply(vc, function(condition) {
        for (d in dimensions) {
          matched <- vapply(labels[[d$field]], function(label) grepl(
            RKorAPClient:::dispersionGroupCondition(label, d), condition, fixed = TRUE), logical(1))
          if (any(matched)) return(sum(c(50, 30, 20)[matched]))
        }
        100
      }, numeric(1))
      tibble::tibble(tokens = unname(sizes))
    }, .package = "RKorAPClient")
  fetched <- fetchSamplePages(queries, warn = FALSE)
  result <- dispersionAnalysis(fetched, by = c("source", "pubPlaceKey", "year"))
  expect_equal(fetches, 2)
  expect_equal(probes, 6)
  expect_equal(nrow(result), 6)
  expect_equal(result$partition, rep(c("source", "pubPlaceKey", "year"), 2))
  expect_equal(result$nParts, rep(3, 6))
  expect_equal(result$range, c(1, 1, 1, 2, 2, 2))
  expect_equal(result$DP, c(rep(.5, 3), rep(11 / 30, 3)))
  years <- result$distribution[[3]]
  expect_equal(years$part, c("2020", "2021", "2022"))
  expect_equal(years$tokens, c(50, 30, 20))
  expect_equal(years$observed, c(2, 0, 0))
  expect_equal(result$sampling[[1]]$pages, result$sampling[[3]]$pages)
  expect_equal(result$sampling[[4]]$pages, result$sampling[[6]]$pages)
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "Dispersion by pubPlaceKey")
  expect_match(printed, "Dispersion by year")
})

test_that("part-based scores do not measure chronological order or time distances", {
  # Adjacent occupied years and separated occupied years have the same scores
  # when their part sizes are equal. This is evenness, not a trend test.
  expect_equal(dispersionAnalysis(c(10, 10, 0, 0), rep(100, 4)),
    dispersionAnalysis(c(10, 0, 0, 10), rep(100, 4)))
  dimension <- RKorAPClient:::dispersionDimension("year")
  expect_equal(RKorAPClient:::dispersionGroupValues(c("2020-12-31", "20210101", "2021"), dimension),
    c("2020", "2021", "2021"))
})

test_that("many source exclusions stay short and preserve literal source membership", {
  labels <- c(sprintf("A long publication title %03d", 1:100), "A|B", "A.B", "C/D")
  d <- RKorAPClient:::dispersionDimension("source")
  compact <- RKorAPClient:::dispersionGroupSetCondition(labels, d, exclude = TRUE)
  repeated <- paste(vapply(labels, RKorAPClient:::dispersionGroupCondition,
    character(1), dimension = d, exclude = TRUE), collapse = " & ")
  expect_gt(nchar(URLencode(repeated, reserved = TRUE)), 8192)
  expect_lt(nchar(URLencode(compact, reserved = TRUE)), 7000)
  pattern <- sub("^[^/]+/(.*)/$", "\\1", compact)
  values <- c(labels, paste(labels, "2025"), "unrelated", "A", "B", "AxB", "C D")
  expect_equal(grepl(paste0("^", pattern, "$"), values, perl = TRUE),
    c(rep(TRUE, 2 * length(labels)), rep(FALSE, 5)))
  expect_match(compact, "corpusTitle !=", fixed = TRUE)
})

test_that("cached inventories still warn about excluded corpus tokens", {
  old <- options(R.cache.rootPath = tempfile("dispersion-coverage-cache-"))
  on.exit(options(old), add = TRUE)
  kco <- dispersionTestConnection()
  kco@cache <- TRUE
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, fetchNext = m$fetchNext,
    corpusStats = function(kco, vc, ...) tibble::tibble(tokens =
      if (length(vc) == 2) c(80, 20) else if (identical(vc, "base")) 125 else 100),
    .package = "RKorAPClient")
  expect_warning(first <- corpusPartition(kco, "source", vc = "base"), "excludes 25")
  before <- m$state$probes
  expect_warning(second <- corpusPartition(kco, "source", vc = "base"), "excludes 25")
  expect_equal(first, second)
  expect_equal(m$state$probes, before)
})

test_that("a fetched comparison retains a spelling with zero hits", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    word <- httr2::url_parse(url)$query$q
    list(meta = list(totalResults = if (word == "absent") 0L else 2L))
  }, .package = "RKorAPClient")
  queries <- corpusQuery(kco, c(present = "word", absent = "absent"), vc = "base",
    fields = c("corpusTitle", "textSigle"))
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, fetchNext = m$fetchNext,
    corpusStats = m$corpusStats, .package = "RKorAPClient")
  fetched <- fetchSamplePages(queries, warn = FALSE)
  result <- dispersionAnalysis(fetched)
  expect_equal(result$totalResults, c(2, 0))
  expect_equal(result$range, c(1, 0))
  expect_true(is.na(result$DP[2]))
  expect_equal(result$distribution[[2]]$observed, c(0, 0))
  expect_equal(result$distribution[[1]]$tokens, result$distribution[[2]]$tokens)
  expect_equal(m$state$hitFetches, 1)
})

test_that("automatic partitions survive cache round trips for both query APIs", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  calls <- 0L
  testthat::local_mocked_bindings(corpusQuery = function(kco, query, vc = "base", as.df = FALSE, ...) {
    calls <<- calls + 1L
    if (as.df && length(vc) == 1L && vc == "base") {
      return(tibble::tibble(query = query, vc = vc, totalResults = 2))
    }
    m$corpusQuery(kco, query, vc, as.df = as.df, ...)
  }, fetchNext = m$fetchNext, corpusStats = m$corpusStats, .package = "RKorAPClient")
  p <- corpusPartition(kco, "source", vc = "base")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  first <- dispersionQuery(kco, "word", partitions = list(source = p), cacheAs = file)
  before <- calls
  restored <- unserialize(serialize(p, NULL))
  expect_silent(again <- dispersionQuery(kco, "word", partitions = list(source = restored), cacheAs = file))
  expect_equal(again$DP, first$DP)
  expect_equal(calls, before)
  expect_null(attr(cacheAsInfo(file)$parameters$partitions$source, "partition")$classify)
  unlink(file)
  first <- frequencyQuery(kco, "word", vc = "base", dispersionBy = p, cacheAs = file)
  before <- calls
  expect_silent(again <- frequencyQuery(kco, "word", vc = "base", dispersionBy = restored, cacheAs = file))
  expect_equal(again$dispersion[[1]]$DP, first$dispersion[[1]]$DP)
  expect_equal(calls, before)
})

test_that("automatic discovery is shared across convenience-wrapper queries", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, fetchNext = m$fetchNext,
    corpusStats = m$corpusStats, .package = "RKorAPClient")
  result <- dispersionQuery(kco, c("one", "two"), vc = "base")
  expect_equal(nrow(result), 2)
  expect_equal(m$state$probes, 3)
})

test_that("unfetched dispersion uses exact counts and fetched samples remain explicit", {
  kco <- dispersionTestConnection()
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, fetchNext = m$fetchNext,
    corpusStats = m$corpusStats, .package = "RKorAPClient")
  q <- m$corpusQuery(kco, "word", vc = "base")
  result <- dispersionAnalysis(q)
  expect_false(result$estimated)
  expect_equal(result$sampling[[1]]$method, "exact_counts")
  expect_equal(m$state$hitFetches, 0)
  expect_equal(result$DP, .2)
})

test_that("incomplete-data conditions abort but unrelated warnings retain results", {
  strict <- RKorAPClient:::strictDispersionRequest
  expect_warning(value <- strict({ warning("dependency deprecation"); 42 }), "dependency deprecation")
  expect_equal(value, 42)
  expect_error(strict(RKorAPClient:::warnIncompleteResults("incomplete page")), "complete requests.*incomplete page")
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(...) list(meta = list(totalResults = 1L, timeExceeded = TRUE)),
    .package = "RKorAPClient")
  expect_error(strict(corpusQuery(kco, "word")), "complete requests.*timeExceeded")
  expect_error(strict(corpusQuery(kco, c("one", "two"))), "complete requests.*timeExceeded")
})

test_that("dispersion printing tolerates selected and removed columns", {
  result <- RKorAPClient:::asDispersionResult(tibble::tibble(query = "word", vc = "", partition = "source",
    variant = "star", totalResults = 1, nParts = 2, estimated = FALSE,
    sampling = list(list(method = "exact_counts", inclusionProbability = 1)), DP = .5), "DP")
  expect_output(print(dplyr::select(result, DP)), "DP")
  attr(result, "comparison") <- TRUE
  expect_output(print(dplyr::select(result, variant, DP)), "star")
  expect_output(print(dplyr::select(result, -sampling)), "DP")
})

test_that("comparisons expose unequal page coverage and warn about comparability", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    word <- httr2::url_parse(url)$query$q
    list(meta = list(totalResults = if (word == "rare") 2L else 100L))
  }, .package = "RKorAPClient")
  queries <- corpusQuery(kco, c(rare = "rare", frequent = "frequent"), vc = "base",
    fields = c("corpusTitle", "textSigle"))
  m <- automaticDispersionMocks(kco)
  testthat::local_mocked_bindings(corpusQuery = m$corpusQuery, corpusStats = m$corpusStats,
    fetchNext = function(kqo, offset, maxFetch, ...) {
      if (is.list(kqo@request)) return(m$fetchNext(kqo, ...))
      kqo@collectedMatches <- tibble::tibble(corpusTitle = as.list(rep("A 2020", maxFetch)),
        textSigle = paste0("T/", seq_len(maxFetch)))
      kqo
    }, .package = "RKorAPClient")
  fetched <- fetchSamplePages(queries, maxHits = 50, warn = FALSE)
  expect_warning(result <- dispersionAnalysis(fetched), "different page-inclusion probabilities")
  expect_equal(result$estimated, c(FALSE, TRUE))
  text <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(text, "2/2 hits; 1/1 pages; inclusion probability 1", fixed = TRUE)
  expect_match(text, "50/100 hits; 1/2 pages; inclusion probability 0.5", fixed = TRUE)
  # Explicit exact mode overrides a fetch and does not compare sampled scores.
  expect_silent(exact <- dispersionAnalysis(fetched, exactFrequencies = TRUE))
  expect_false(any(exact$estimated))
})

test_that("mixed scalar and array field values survive real JSON parsing across pages", {
  kco <- dispersionTestConnection()
  fetches <- 0L
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    p <- httr2::url_parse(url)$query
    offset <- if (is.null(p$offset)) 0L else as.integer(p$offset)
    count <- as.integer(p$count)
    rows <- if (!count) integer() else seq.int(offset + 1L, min(offset + count, 51L))
    if (count) fetches <<- fetches + 1L
    matches <- lapply(rows, function(i) list(matchID = paste0("match-T/", i, "-p0-1"),
      fields = list(list(key = "corpusTitle", value = if (i == 2L || i == 51L) c("a", "b") else "a"),
                    list(key = "textSigle", value = paste0("T/", i)))))
    jsonlite::fromJSON(jsonlite::toJSON(list(meta = list(totalResults = 51L, startIndex = offset,
      itemsPerPage = length(rows)), matches = matches), auto_unbox = TRUE))
  }, .package = "RKorAPClient")
  q <- corpusQuery(kco, "word", fields = c("corpusTitle", "textSigle"))
  preserved <- fetchAll(q, preserveMetadataValues = TRUE)
  expect_equal(nrow(preserved@collectedMatches), 51)
  expect_equal(preserved@collectedMatches$corpusTitle[[1]], "a")
  expect_equal(preserved@collectedMatches$corpusTitle[[2]], c("a", "b"))
  expect_equal(preserved@collectedMatches$corpusTitle[[51]], c("a", "b"))
  joined <- fetchAll(q)
  expect_equal(attr(joined, "korapMetadataValues"), "joined")
  before <- fetches
  refreshed <- RKorAPClient:::sampleDispersionMetadata(joined, Inf, 7, FALSE)
  expect_equal(fetches - before, 2)
  expect_equal(refreshed$matches$corpusTitle[[2]], c("a", "b"))
  expect_error(RKorAPClient:::classifyDispersionMatches(refreshed$matches,
    dispersionTestPartition(kco), attr(dispersionTestPartition(kco), "partition")), "Multivalued")
})

test_that("full page retrieval is informational even with warnings as errors", {
  kco <- dispersionTestConnection()
  q <- dispersionTestQuery(kco, 0)
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  expect_message(result <- fetchSamplePages(q), "complete results.*no random sample")
  expect_equal(nrow(result@collectedMatches), 0)
})

test_that("publication-date aliases and literal-title filters are consistent", {
  date <- RKorAPClient:::dispersionDimension("pubDate")
  year <- RKorAPClient:::dispersionDimension("year")
  expect_equal(RKorAPClient:::dispersionGroupValues(c("2023-01-01", "20241231"), date),
    RKorAPClient:::dispersionGroupValues(c("2023-01-01", "20241231"), year))
  expect_equal(RKorAPClient:::dispersionGroupCondition("2023", date), "pubDate in 2023")
  labels <- c("A.B 2023", "A|B 2024", "C/D 2025")
  dimension <- RKorAPClient:::dispersionDimension("corpusTitle")
  condition <- RKorAPClient:::dispersionGroupSetCondition(labels, dimension, exclude = TRUE)
  pattern <- sub("^[^/]+/(.*)/$", "\\1", condition)
  expect_true(all(grepl(paste0("^", pattern, "$"), labels)))
  expect_false(any(grepl(paste0("^", pattern, "$"), c("AxB 2023", "A 2024", "A.B 2023 extra"))))
  expect_equal(lengths(gregexpr("corpusTitle", condition, fixed = TRUE)), 1)
})

test_that("custom dispersion functions can return plain NA", {
  expect_true(is.na(dispersionAnalysis(c(1, 0), c(1, 1), list(undefined = function(frequencies, sizes) NA))$undefined))
})

# Exercise the real fields parser, pagination and fetch-mode bookkeeping.
scalarMetadataApi <- function(total = 51L, multivalued = integer(), missing = integer()) {
  function(kco, url, ...) {
    p <- httr2::url_parse(url)$query
    offset <- if (is.null(p$offset)) 0L else as.integer(p$offset)
    count <- as.integer(p$count)
    rows <- if (!count) integer() else seq.int(offset + 1L, min(offset + count, total))
    fields <- strsplit(p$fields, ",", fixed = TRUE)[[1]]
    matches <- lapply(rows, function(i) list(matchID = paste0("match-T/", i, "-p0-1"),
      fields = lapply(fields, function(field) list(key = field, value =
        if (field == "textSigle") paste0("T/", i) else
        if (i %in% missing) character() else
        if (i %in% multivalued) c("a", "b") else "a"))))
    jsonlite::fromJSON(jsonlite::toJSON(list(meta = list(totalResults = total, startIndex = offset,
      itemsPerPage = length(rows)), matches = matches), auto_unbox = TRUE))
  }
}

test_that("complete metadata lacking the requested dimension defaults to exact counts", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = scalarMetadataApi(total = 100L), .package = "RKorAPClient")
  q <- corpusQuery(kco, "word", fields = "textSigle") |>
    fetchAll(preserveMetadataValues = TRUE)
  expect_equal(nrow(q@collectedMatches), 100)
  expect_false("corpusTitle" %in% names(q@collectedMatches))
  exactCalls <- 0L
  part <- dispersionTestPartition(kco)
  testthat::local_mocked_bindings(
    discoverDispersionPartition = function(...) part,
    fetchNext = function(...) stop("the default must not fetch another sample"),
    corpusQuery = function(kco, query, vc, as.df, ...) {
      expect_true(as.df)
      exactCalls <<- exactCalls + 1L
      tibble::tibble(totalResults = c(100, 0))
    }, .package = "RKorAPClient")
  result <- dispersionAnalysis(q, by = "corpusTitle", searchHitsSampleLimit = 50)
  expect_false(result$estimated)
  expect_equal(result$sampling[[1]]$method, "exact_counts")
  expect_equal(exactCalls, 1)
})

test_that("vector discovery receives no seeds from joined metadata", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = function(kco, url, ...) {
    p <- httr2::url_parse(url)$query
    matches <- if (as.integer(p$count) == 0L) list() else
      tibble::tibble(matchID = "match-T/1-p0-1", fields = list(tibble::tibble(
        key = c("textClass", "textSigle"),
        value = list(if (p$q == "one") "politik" else c("politik", "sport"), "T/1"))))
    list(meta = list(totalResults = 1L, startIndex = 0L,
      itemsPerPage = if (is.data.frame(matches)) nrow(matches) else 0L), matches = matches)
  }, .package = "RKorAPClient")
  queries <- corpusQuery(kco, c(one = "one", two = "two"), vc = "base",
    fields = c("textClass", "textSigle")) |> fetchAll()
  objects <- RKorAPClient:::korapQueryObjects(queries)
  expect_equal(objects[[2]]@collectedMatches$textClass, "politik sport")
  expect_true(all(vapply(objects, function(q) identical(attr(q, "korapMetadataValues"), "joined"), logical(1))))
  seeds <- list()
  part <- dispersionTestPartition(kco, by = "textClass", values = c("politik", "sport"), baseVc = "base")
  testthat::local_mocked_bindings(
    discoverDispersionPartition = function(kco, by, vc, initialValues, ...) {
      seeds[[length(seeds) + 1L]] <<- list(initialValues)
      part
    },
    corpusQuery = function(...) tibble::tibble(totalResults = c(1, 0)),
    .package = "RKorAPClient")
  result <- dispersionAnalysis(queries, by = "textClass")
  expect_equal(nrow(result), 2)
  expect_length(seeds, 1)
  expect_length(seeds[[1]][[1]], 0)
  expect_false(any(result$estimated))
})

test_that("preserved scalar metadata stays tabulatable and empty values become NA", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = scalarMetadataApi(missing = 2L), .package = "RKorAPClient")
  q <- corpusQuery(kco, "word", fields = c("corpusTitle", "textSigle"))
  all <- fetchAll(q, preserveMetadataValues = TRUE)
  expect_type(all@collectedMatches$corpusTitle, "character")
  expect_type(all@collectedMatches$textSigle, "character")
  expect_true(is.na(all@collectedMatches$corpusTitle[2]))
  expect_length(dim(table(all@collectedMatches$corpusTitle)), 1)
  expect_equal(unname(as.integer(table(all@collectedMatches$corpusTitle))), 50L)
  sampled <- fetchSamplePages(q, maxHits = Inf, warn = FALSE)
  expect_equal(sampled@collectedMatches, all@collectedMatches)
})

test_that("continuations inherit preservation and promote only genuinely multivalued fields", {
  kco <- dispersionTestConnection()
  testthat::local_mocked_bindings(apiCall = scalarMetadataApi(multivalued = 51L), .package = "RKorAPClient")
  q <- corpusQuery(kco, "word", fields = c("corpusTitle", "textSigle"))
  first <- fetchNext(q, preserveMetadataValues = TRUE)
  expect_type(first@collectedMatches$corpusTitle, "character")
  rest <- fetchRest(first)
  expect_equal(nrow(rest@collectedMatches), 51)
  expect_type(rest@collectedMatches$textSigle, "character")
  expect_type(rest@collectedMatches$corpusTitle, "list")
  expect_equal(rest@collectedMatches$corpusTitle[[1]], "a")
  expect_equal(rest@collectedMatches$corpusTitle[[51]], c("a", "b"))
  expect_equal(attr(rest, "korapMetadataValues"), "preserved")
  expect_error(fetchNext(first, preserveMetadataValues = FALSE), "Cannot change.*Restart with fetchAll")
  joined <- fetchNext(q)
  expect_error(fetchNext(joined, preserveMetadataValues = TRUE), "Cannot change.*Restart with fetchAll")
  restarted <- fetchAll(joined, preserveMetadataValues = TRUE)
  expect_equal(restarted@collectedMatches, rest@collectedMatches)
})

test_that("printing estimated results without sampledHits falls back to table output", {
  result <- RKorAPClient:::asDispersionResult(tibble::tibble(query = "word", partition = "source",
    totalResults = 100, sampledHits = 50, nParts = 2, estimated = TRUE,
    sampling = list(list(method = "random_pages", pages = 0, inclusionProbability = .5)), DP = .5), "DP")
  expect_output(print(dplyr::select(result, -sampledHits)), "DP")
})

test_that("implicit sampling warning does not attribute a call to fetchSamplePages", {
  warningText <- tryCatch({
    RKorAPClient:::pageSampleWarning(100, 50, TRUE)
  }, warning = conditionMessage)
  expect_match(warningText, "result pages, not independent hits", fixed = TRUE)
  expect_false(grepl("fetchSamplePages", warningText, fixed = TRUE))
  expect_true(RKorAPClient:::dispersionDimension("creationDate")$year)
  expect_equal(RKorAPClient:::dispersionGroupCondition("2023",
    RKorAPClient:::dispersionDimension("creationDate")), "creationDate in 2023")
})
