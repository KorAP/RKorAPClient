ciToggleData <- function() suppressWarnings(ci(data.frame(query = "term",
  vc = c("A", "B"), totalResults = c(10, 20), total = c(1e5, 1e6),
  year = c(2023, 2024), webUIRequestUrl = "https://example.invalid/")))

test_that("interactive plots omit intervals without changing frequency values or requiring bounds", {
  d <- ciToggleData()
  bare <- d[, setdiff(names(d), c("conf.low", "conf.high"))]
  for (plot in list(hc_freq_bar_ci, hc_freq_by_year_ci)) {
    full <- plot(d)
    hidden <- plot(bare, showCI = FALSE)
    expect_length(full$x$hc_opts$series, 2)
    expect_length(hidden$x$hc_opts$series, 1)
    getValues <- function(h) vapply(h$x$hc_opts$series[[1]]$data, `[[`, numeric(1), "y")
    expect_equal(getValues(hidden), getValues(full))
    expect_false(any(vapply(hidden$x$hc_opts$series[[1]]$data,
      function(p) grepl("Confidence interval", p$tooltipText, fixed = TRUE), logical(1))))
    expect_error(plot(d, showCI = NA), "showCI must be TRUE or FALSE")
  }
})

test_that("ggplot interval layers are optional and observed bars and curves are preserved", {
  d <- ipm(ciToggleData())
  bare <- d[, setdiff(names(d), c("conf.low", "conf.high"))]
  bar <- ggplot2::ggplot(d, ggplot2::aes(vc, ipm)) + geom_freq_bar_ci()
  noBarCI <- ggplot2::ggplot(bare, ggplot2::aes(vc, ipm)) + geom_freq_bar_ci(showCI = FALSE)
  full <- ggplot2::ggplot_build(bar)$data
  hidden <- ggplot2::ggplot_build(noBarCI)$data
  expect_length(full, 2)
  expect_length(hidden, 1)
  expect_equal(hidden[[1]][c("x", "y", "xmin", "xmax")], full[[1]][c("x", "y", "xmin", "xmax")])
  line <- ggplot2::ggplot(d, ggplot2::aes(year, ipm)) + geom_freq_by_year_ci()
  noLineCI <- ggplot2::ggplot(bare, ggplot2::aes(year, ipm)) + geom_freq_by_year_ci(showCI = FALSE)
  full <- ggplot2::ggplot_build(line)$data
  hidden <- ggplot2::ggplot_build(noLineCI)$data
  expect_length(full, 3)
  expect_length(hidden, 2)
  expect_equal(hidden[[1]][c("x", "y")], full[[2]][c("x", "y")])
  expect_error(geom_freq_bar_ci(showCI = 1), "showCI must be TRUE or FALSE")
  expect_error(geom_freq_by_year_ci(showCI = c(TRUE, FALSE)), "showCI must be TRUE or FALSE")
})
