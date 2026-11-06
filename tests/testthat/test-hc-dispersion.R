chartDispersionResult <- function(counts = list(star = c(2, 0), colon = c(1, 2)),
                                  tokens = c(80, 20), labels = c("a", "b")) {
  tibble::tibble(query = names(counts), variant = names(counts), partition = "source",
    estimated = FALSE, sampling = rep(list(list(method = "all_metadata", inclusionProbability = 1)), length(counts)),
    distribution = lapply(counts, function(n) tibble::tibble(part = labels,
      vc = paste0('corpusTitle="', labels, '"'), tokens = tokens,
      frequency = as.numeric(n), observed = as.numeric(n),
      webUIRequestUrl = paste0("https://example.invalid/?q=word&cq=", seq_along(n)))))
}

chartSeriesValues <- function(chart, series) vapply(chart$x$hc_opts$series[[series]]$data, `[[`, numeric(1), "y")

test_that("dispersion charts compare normalized shares with the corpus reference without queries", {
  d <- chartDispersionResult()
  before <- serialize(d, NULL)
  testthat::local_mocked_bindings(apiCall = function(...) stop("must not query"),
    corpusQuery = function(...) stop("must not query"), corpusStats = function(...) stop("must not query"),
    .package = "RKorAPClient")
  chart <- hc_dispersion(d)
  expect_s3_class(chart, "highchart")
  expect_equal(chartSeriesValues(chart, 1), c(80, 20))
  expect_equal(chartSeriesValues(chart, 2), c(100, 0))
  expect_equal(chartSeriesValues(chart, 3), c(100 / 3, 200 / 3))
  expect_equal(chart$x$hc_opts$series[[2]]$data[[1]]$perMillion, 25000)
  expect_match(chart$x$hc_opts$series[[2]]$data[[1]]$webUIRequestUrl, "https://example.invalid/", fixed = TRUE)
  expect_null(chart$x$hc_opts$series[[1]]$data[[1]]$webUIRequestUrl)
  expect_identical(serialize(d, NULL), before)
})

test_that("top parts use a common order and conserve counts and denominators in the remainder", {
  d <- chartDispersionResult(list(star = c(7, 0, 3), colon = c(1, 8, 1)),
    tokens = c(70, 20, 10), labels = c("a", "b", "c"))
  chart <- hc_dispersion(d, top = 1)
  expect_equal(chart$x$hc_opts$xAxis$categories, c("b", "Remaining 2 parts"))
  expect_equal(chartSeriesValues(chart, 1), c(20, 80))
  expect_equal(chartSeriesValues(chart, 2), c(0, 100))
  expect_equal(chartSeriesValues(chart, 3), c(80, 20))
  expect_equal(chart$x$hc_opts$series[[2]]$data[[2]]$frequency, 10)
  expect_equal(chart$x$hc_opts$series[[2]]$data[[2]]$tokens, 80)
  expect_equal(chart$x$hc_opts$series[[2]]$data[[2]]$perMillion, 125000)
  expect_null(chart$x$hc_opts$series[[2]]$data[[2]]$webUIRequestUrl)
  expect_match(chart$x$hc_opts$caption$text, "scores still use all parts", fixed = TRUE)
  all <- hc_dispersion(d, top = Inf)
  expect_equal(length(all$x$hc_opts$xAxis$categories), 3)
  expect_true(all(vapply(seq_len(3), function(i) abs(sum(chartSeriesValues(all, i)) - 100) < 1e-10, logical(1))))
})

test_that("charts align reordered parts but reject incompatible populations", {
  d <- chartDispersionResult()
  d$distribution[[2]] <- d$distribution[[2]][2:1, ]
  expect_equal(chartSeriesValues(hc_dispersion(d), 3), c(100 / 3, 200 / 3))
  changed <- d
  changed$distribution[[2]]$tokens[1] <- 21
  expect_error(hc_dispersion(changed), "same corpus parts, token sizes")
  changed <- d
  changed$distribution[[2]]$vc[1] <- "different corpus"
  expect_error(hc_dispersion(changed), "same corpus parts, token sizes")
  changed <- d
  changed$distribution[[2]]$part[1] <- changed$distribution[[2]]$part[2]
  expect_error(hc_dispersion(changed), "distributions are unavailable or invalid")
  changed <- d
  changed$sampling[[2]]$apiUrl <- "https://another.example.invalid/"
  expect_error(hc_dispersion(changed), "same KorAP instance and index revision")
})

test_that("chart partition selection is explicit when several dimensions are present", {
  d <- chartDispersionResult()
  other <- d
  other$partition <- "pubPlaceKey"
  d <- dplyr::bind_rows(d, other)
  expect_error(hc_dispersion(d), "Choose one partition")
  expect_equal(length(hc_dispersion(d, partition = "pubPlaceKey")$x$hc_opts$series), 3)
  expect_error(hc_dispersion(d, partition = "missing"), "Unknown partition")
  expect_error(hc_dispersion(d, partition = "source", top = 0), "top must be")
  expect_error(hc_dispersion(d, partition = "source", top = 1.5), "top must be")
  expect_error(hc_dispersion(dispersionAnalysis(c(1, 2), c(10, 10))), "query-based")
})

test_that("zero-hit variants have undefined shares while zero-hit parts remain", {
  d <- chartDispersionResult(list(absent = c(0, 0), present = c(2, 0)))
  chart <- hc_dispersion(d)
  expect_true(all(is.na(chartSeriesValues(chart, 2))))
  expect_equal(chartSeriesValues(chart, 3), c(100, 0))
  expect_equal(length(chart$x$hc_opts$xAxis$categories), 2)
  expect_match(chart$x$hc_opts$caption$text, "No hits within listed parts for absent", fixed = TRUE)
  d$estimated[1] <- TRUE
  d$sampling[[1]]$inclusionProbability <- .5
  expect_match(hc_dispersion(d)$x$hc_opts$caption$text, "No observed sample hits", fixed = TRUE)
  d <- chartDispersionResult(list(one = c(2, 0)), tokens = c(80, 0))
  expect_equal(chartSeriesValues(hc_dispersion(d), 1), 100)
})

test_that("sampling and exclusions remain visible without inventing uncertainty", {
  d <- chartDispersionResult()
  d$estimated[2] <- TRUE
  d$sampling[[2]]$inclusionProbability <- .25
  d$distribution[[2]]$frequency <- d$distribution[[2]]$observed * 4
  d$sampling[[2]]$excludedTokens <- 10
  chart <- hc_dispersion(d)
  expect_match(chart$x$hc_opts$caption$text, "Page-sample estimates", fixed = TRUE)
  expect_match(chart$x$hc_opts$caption$text, "unequal page coverage", fixed = TRUE)
  expect_match(chart$x$hc_opts$caption$text, "outside the listed parts is excluded", fixed = TRUE)
  expect_match(chart$x$hc_opts$series[[3]]$tooltip$pointFormat, "Estimated hits", fixed = TRUE)
  expect_match(chart$x$hc_opts$series[[3]]$tooltip$pointFormat, "25%", fixed = TRUE)
  expect_equal(chartSeriesValues(chart, 3), c(100 / 3, 200 / 3))
  d$estimated[2] <- FALSE
  expect_error(hc_dispersion(d), "sampling indicators are inconsistent")
})

test_that("chart labels are escaped and non-web concordance URLs are not clickable", {
  d <- chartDispersionResult()
  d$variant[1] <- "<script>&\""
  for (i in seq_along(d$distribution)) d$distribution[[i]]$part[1] <- "<img src=x>"
  d$distribution[[1]]$webUIRequestUrl[1] <- "javascript:alert(1)"
  names(d$distribution[[2]]$webUIRequestUrl) <- c("a", "b")
  chart <- hc_dispersion(d)
  expect_equal(chart$x$hc_opts$xAxis$categories[1], "&lt;img src=x&gt;")
  expect_equal(chart$x$hc_opts$series[[2]]$name, "&lt;script&gt;&amp;&quot;")
  expect_null(chart$x$hc_opts$series[[2]]$data[[1]]$webUIRequestUrl)
  expect_equal(chart$x$hc_opts$series[[3]]$data[[1]]$webUIRequestUrl,
    unname(d$distribution[[2]]$webUIRequestUrl[1]))
})

test_that("the chart handles additional variants and keeps calendar years ordered", {
  d <- chartDispersionResult(list(one = c(1, 2, 7), two = c(2, 3, 5), three = c(0, 0, 8)),
    tokens = c(50, 30, 20), labels = c("2023", "2024", "2025"))
  d$partition <- "year"
  chart <- hc_dispersion(d)
  expect_equal(chart$x$hc_opts$xAxis$categories, c("2023", "2024", "2025"))
  expect_equal(length(chart$x$hc_opts$series), 4)
  expect_equal(chartSeriesValues(chart, 4), c(0, 0, 100))
})
