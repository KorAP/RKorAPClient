test_that("browser localization uses actual chart data and preserves repeated concordance clicks", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed for JavaScript regression checks")
  f <- suppressWarnings(ci(data.frame(query = "rare", vc = "", totalResults = 3, total = 5e10)))
  a <- suppressWarnings(ci(data.frame(query = c("one", "two"), vc = "", totalResults = c(30, 70), total = 100)))
  a$as.alternatives <- TRUE
  y <- f; y$year <- 2023
  d <- tibble::tibble(query = c("<s>", "two"), partition = "source", estimated = c(FALSE, TRUE),
    sampling = list(list(inclusionProbability = 1), list(inclusionProbability = .5)),
    distribution = list(data.frame(part = c("a", "b"), vc = c("a", "b"), tokens = c(80, 20), frequency = c(3, 1), observed = c(3, 1)),
      data.frame(part = c("a", "b"), vc = c("a", "b"), tokens = c(80, 20), frequency = c(2, 4), observed = c(1, 2))))
  charts <- list(frequency = hc_freq_bar_ci(f)$x, alternatives = hc_freq_bar_ci(a)$x,
    year = hc_freq_by_year_ci(y)$x, dispersion = hc_dispersion(d, top = 1)$x,
    frequencyNoCI = hc_freq_bar_ci(f[,setdiff(names(f),c("conf.low","conf.high"))],showCI=FALSE)$x,
    yearNoCI = hc_freq_by_year_ci(y[,setdiff(names(y),c("conf.low","conf.high"))],showCI=FALSE)$x)
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file), add = TRUE)
  writeLines(htmlwidgets:::toJSON(charts), file)
  output <- system2(node, shQuote(c(test_path("js", "chart-locale.cjs"),
    system.file("js", "chart-locale.js", package = "RKorAPClient"), file)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})

test_that("chart language can be fixed for reproducible exports", {
  rlang::local_options(rkorap.chart.language = "de")
  d <- suppressWarnings(ci(data.frame(query = "one", vc = "", totalResults = 3, total = 100)))
  expect_identical(hc_freq_bar_ci(d)$x$hc_opts$chart$korapLocale$language, "de")
  options(rkorap.chart.language = "xx")
  expect_error(hc_freq_bar_ci(d), "auto.*de.*en")
})
