frequencyBarData <- function(query = c("Berlin", "Berlin"),
                             vc = c("award=buchpreis", ""), hits = c(10, 20),
                             total = c(1e5, 1e6), label = c("Book prize", "DeLiKo")) {
  d <- suppressWarnings(ci(data.frame(query = query, vc = vc, totalResults = hits, total = total,
    webUIRequestUrl = paste0("https://example.invalid/?q=", query, "&row=", seq_along(query)))))
  if (!is.null(label)) d$label <- label
  d
}

frequencyBarValues <- function(chart, series, field = "y") {
  vapply(chart$x$hc_opts$series[[series]]$data, function(p) p[[field]], numeric(1))
}

test_that("frequency bars preserve counts, denominators, intervals and links without queries", {
  d <- frequencyBarData()
  before <- serialize(d, NULL)
  local_mocked_bindings(apiCall = function(...) stop("must not query"),
    corpusQuery = function(...) stop("must not query"),
    corpusStats = function(...) stop("must not query"), .package = "RKorAPClient")
  h <- hc_freq_bar_ci(d)
  expect_s3_class(h, "highchart")
  expect_equal(h$x$hc_opts$xAxis$categories, c("Book prize", "DeLiKo"))
  expect_equal(frequencyBarValues(h, 1), c(100, 20))
  expect_equal(frequencyBarValues(h, 2, "low"), d$conf.low * 1e6)
  expect_equal(frequencyBarValues(h, 2, "high"), d$conf.high * 1e6)
  expect_equal(frequencyBarValues(h, 1, "count"), c(10, 20))
  expect_equal(frequencyBarValues(h, 1, "total"), c(1e5, 1e6))
  expect_equal(frequencyBarValues(h, 1, "confHigh"), d$conf.high * 1e6)
  expect_equal(h$x$hc_opts$series[[1]]$data[[2]]$webUIRequestUrl, d$webUIRequestUrl[2])
  expect_equal(h$x$hc_opts$series[[2]]$linkedTo, h$x$hc_opts$series[[1]]$id)
  expect_false(h$x$hc_opts$series[[2]]$enableMouseTracking)
  expect_identical(serialize(d, NULL), before)
})

test_that("queries within a corpus are categories and column options are passed through", {
  d <- frequencyBarData(query = c("Iowa", "Berlin"), vc = rep("award=buchpreis", 2),
                        label = rep("Book prize", 2))
  h <- hc_freq_bar_ci(d, horizontal = FALSE, ylabel = "per million",
    color = "#123456", dataLabels = list(enabled = TRUE))
  expect_equal(h$x$hc_opts$xAxis$categories, c("Iowa", "Berlin"))
  expect_false(h$x$hc_opts$chart$inverted)
  expect_equal(h$x$hc_opts$series[[1]]$type, "column")
  expect_true(h$x$hc_opts$series[[1]]$dataLabels$enabled)
  expect_equal(h$x$hc_opts$yAxis$title$text, "per million")
  expect_equal(h$x$hc_opts$series[[1]]$color, "#123456")
  expect_false(h$x$hc_opts$legend$enabled)
})

test_that("query groups and their intervals align despite reordered and missing pairs", {
  d <- frequencyBarData(query = c("Berlin", "Paris", "Berlin"),
    vc = c("", "award=buchpreis", "award=buchpreis"), hits = c(20, 30, 10),
    total = c(1e6, 1e5, 1e5), label = c("DeLiKo", "Book prize", "Book prize"))
  h <- hc_freq_bar_ci(d)
  expect_equal(h$x$hc_opts$xAxis$categories, c("DeLiKo", "Book prize"))
  expect_equal(h$x$hc_opts$series[[1]]$name, "Berlin")
  expect_equal(h$x$hc_opts$series[[3]]$name, "Paris")
  expect_equal(frequencyBarValues(h, 1), c(20, 100))
  expect_equal(frequencyBarValues(h, 3), c(NA_real_, 300))
  expect_equal(frequencyBarValues(h, 2, "low"), d$conf.low[c(1, 3)] * 1e6)
  expect_equal(frequencyBarValues(h, 4, "high"), c(NA_real_, d$conf.high[2] * 1e6))
  expect_equal(frequencyBarValues(h, 1, "x"), c(0, 1))
  expect_equal(frequencyBarValues(h, 4, "x"), c(0, 1))
  expect_true(h$x$hc_opts$legend$enabled)
  expect_null(h$x$hc_opts$series[[3]]$data[[1]]$webUIRequestUrl)
})

test_that("alternative percentages retain supplied denominators even for a displayed subset", {
  d <- frequencyBarData(query = c("one", "two"), vc = c("", ""), hits = c(30, 70),
                        total = c(100, 100), label = NULL)
  h <- hc_freq_bar_ci(d, as.alternatives = TRUE)
  expect_equal(frequencyBarValues(h, 1), c(30, 70))
  expect_equal(frequencyBarValues(h, 2, "high"), d$conf.high * 100)
  expect_equal(h$x$hc_opts$yAxis$max, 100)
  expect_equal(frequencyBarValues(hc_freq_bar_ci(d[1, ], as.alternatives = TRUE), 1), 30)
  expect_error(hc_freq_bar_ci(ipm(d)), "do not call ipm")
  expect_error(hc_freq_bar_ci(percent(d)), "unconverted proportions")
})

test_that("zero hits retain their interval and undefined denominators are gaps", {
  d <- frequencyBarData(hits = c(0, 0), total = c(1e5, 0))
  h <- hc_freq_bar_ci(d)
  expect_equal(frequencyBarValues(h, 1), c(0, NA_real_))
  expect_gt(h$x$hc_opts$series[[2]]$data[[1]]$high, 0)
  expect_true(is.na(h$x$hc_opts$series[[2]]$data[[2]]$high))
  expect_match(h$x$hc_opts$caption$text, "gaps")
})

test_that("invalid or ambiguous frequency tables are rejected", {
  d <- frequencyBarData()
  expect_error(hc_freq_bar_ci(d[FALSE, ]), "frequencyQuery")
  expect_error(hc_freq_bar_ci(d[, setdiff(names(d), "conf.low")]), "confidence limits")
  expect_error(hc_freq_bar_ci(rbind(d, d[1, ])), "only once")
  expect_error(hc_freq_bar_ci(d, horizontal = NA), "TRUE or FALSE")
  expect_error(hc_freq_bar_ci(d, type = "line"), "managed by")
  expect_error(hc_freq_bar_ci(d, as.alternatives = 1), "TRUE or FALSE")
  expect_error(hc_freq_bar_ci(d, ylabel = c("a", "b")), "single string")
  changed <- d; changed$f[1] <- -1
  expect_error(hc_freq_bar_ci(changed), "non-negative")
  changed <- d; changed$total[1] <- 1
  expect_error(hc_freq_bar_ci(changed), "denominators")
  changed <- d; changed$conf.low[1] <- changed$f[1] + 1
  expect_error(hc_freq_bar_ci(changed), "enclosing")
  changed <- d; changed$conf.high[1] <- Inf
  expect_error(hc_freq_bar_ci(changed), "non-negative")
  changed <- d; changed$query[1] <- NA_character_
  expect_error(hc_freq_bar_ci(changed), "non-empty")
  changed <- d; changed$totalResults[1] <- NA_real_
  expect_true(is.na(frequencyBarValues(hc_freq_bar_ci(changed), 1)[1]))
})

test_that("labels are escaped and only web concordance links are attached", {
  d <- frequencyBarData()
  d$query <- rep("<script>&", 2)
  d$label[1] <- "<img src=x>"
  d$webUIRequestUrl[1] <- "javascript:alert(1)"
  h <- hc_freq_bar_ci(d)
  expect_equal(h$x$hc_opts$title$text, "&lt;script&gt;&amp;")
  expect_equal(h$x$hc_opts$xAxis$categories[1], "&lt;img src=x&gt;")
  expect_null(h$x$hc_opts$series[[1]]$data[[1]]$webUIRequestUrl)
  expect_equal(h$x$hc_opts$series[[1]]$data[[2]]$webUIRequestUrl, d$webUIRequestUrl[2])
  d$label <- NULL
  h <- hc_freq_bar_ci(d)
  expect_equal(h$x$hc_opts$xAxis$categories[2], "(all)")
})
