ggFrequencyBarData <- function() {
  d <- suppressWarnings(ci(data.frame(
    query = c("Berlin", "Paris", "Berlin", "Paris"),
    vc = c("book", "book", "all", "all"), label = c("Book prize", "Book prize", "All", "All"),
    totalResults = c(10, 30, 20, 0), total = c(1e5, 1e5, 1e6, 1e6))))
  ipm(d)
}

test_that("ggplot frequency bars retain scaled values and supplied intervals without queries", {
  local_mocked_bindings(apiCall = function(...) stop("must not query"),
    corpusQuery = function(...) stop("must not query"), .package = "RKorAPClient")
  d <- ggFrequencyBarData()[1:2, ]
  before <- serialize(d, NULL)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = query, y = ipm)) + geom_freq_bar_ci()
  b <- ggplot2::ggplot_build(g)$data
  expect_length(b, 2)
  expect_equal(b[[1]]$y, d$ipm)
  expect_equal(b[[2]]$ymin, d$conf.low)
  expect_equal(b[[2]]$ymax, d$conf.high)
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
  expect_identical(serialize(d, NULL), before)
})

test_that("grouped bars and intervals share their dodge positions", {
  d <- ggFrequencyBarData()[c(3, 1, 4, 2), ]
  g <- ggplot2::ggplot(d, ggplot2::aes(x = label, y = ipm, fill = query)) +
    geom_freq_bar_ci()
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
  expect_equal(b[[1]]$group, b[[2]]$group)
  expect_equal(b[[1]]$y, d$ipm)
  expect_equal(b[[2]]$ymax, d$conf.high)
  expect_true(all(b[[2]]$colour == "black"))
  expect_gt(length(unique(as.numeric(b[[1]]$x))), length(unique(d$label)))

  position <- ggplot2::position_dodge()
  g <- ggplot2::ggplot(d, ggplot2::aes(x = label, y = ipm, fill = query)) +
    geom_freq_bar_ci(position = position, width = .8, errorbar.width = .1)
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
  expect_null(position$width)
})

test_that("colour grouping and explicitly grouped monochrome plots stay aligned", {
  d <- ggFrequencyBarData()
  g <- ggplot2::ggplot(d, ggplot2::aes(label, ipm, colour = query)) +
    geom_freq_bar_ci()
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
  expect_equal(b[[1]]$group, b[[2]]$group)
  g <- ggplot2::ggplot(d, ggplot2::aes(label, ipm, group = query)) +
    geom_freq_bar_ci(fill = "grey70", errorbar.colour = "black")
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
})

test_that("custom bounds, data and appearance can be used without changing the values", {
  d <- ggFrequencyBarData()[1:2, ]
  names(d)[names(d) == "conf.low"] <- "lower"
  names(d)[names(d) == "conf.high"] <- "upper"
  g <- ggplot2::ggplot(mapping = ggplot2::aes(x = query, y = ipm)) +
    geom_freq_bar_ci(ggplot2::aes(ymin = lower, ymax = upper),
      data = d, fill = "grey70", errorbar.colour = "grey20",
      errorbar.params = list(linewidth = .6, alpha = .8))
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(b[[2]]$ymin, d$lower)
  expect_true(all(b[[1]]$fill == "grey70"))
  expect_true(all(b[[2]]$colour == "grey20"))
  expect_true(all(b[[2]]$linewidth == .6))
  expect_true(all(b[[2]]$alpha == .8))
})

test_that("percentages, zero hits and missing observations are not renormalized", {
  raw <- suppressWarnings(ci(data.frame(query = c("one", "two"), totalResults = c(30, 70),
    total = c(100, 100))))
  d <- percent(raw)
  g <- ggplot2::ggplot(d, ggplot2::aes(query, f)) + geom_freq_bar_ci()
  b <- ggplot2::ggplot_build(g)$data
  expect_equal(b[[1]]$y, c(30, 70))
  expect_equal(b[[2]]$ymax, raw$conf.high * 100)

  d <- ggFrequencyBarData()
  d$ipm[2] <- NA_real_; d$conf.low[2] <- NA_real_; d$conf.high[2] <- NA_real_
  g <- ggplot2::ggplot(d, ggplot2::aes(label, ipm, fill = query)) +
    geom_freq_bar_ci(na.rm = TRUE)
  b <- ggplot2::ggplot_build(g)$data
  expect_true(is.na(b[[1]]$y[2]))
  expect_equal(b[[1]]$y[4], 0)
  expect_gt(b[[2]]$ymax[4], 0)
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
})

test_that("flipped plots and facets use ordinary ggplot coordinates", {
  d <- ggFrequencyBarData()
  g <- ggplot2::ggplot(d, ggplot2::aes(label, ipm, fill = query)) +
    geom_freq_bar_ci() + ggplot2::coord_flip() + ggplot2::facet_wrap(~query)
  b <- ggplot2::ggplot_build(g)
  expect_s3_class(g, "ggplot")
  expect_s3_class(ggplot2::ggplotGrob(g), "gtable")
  expect_equal(as.numeric(b$data[[1]]$x), as.numeric(b$data[[2]]$x))
  expect_equal(b$data[[1]]$PANEL, b$data[[2]]$PANEL)
})

test_that("unsafe interval positions and inconsistent layer arguments are rejected", {
  expect_error(geom_freq_bar_ci(position = "stack"), "cannot be stacked")
  expect_error(geom_freq_bar_ci(position = ggplot2::position_fill()), "cannot be stacked")
  expect_error(geom_freq_bar_ci(position = ggplot2::position_dodge2()), "position_dodge")
  expect_error(geom_freq_bar_ci(width = 0), "positive")
  expect_error(geom_freq_bar_ci(errorbar.width = -1), "non-negative")
  expect_error(geom_freq_bar_ci(errorbar.params = list(.5)), "named list")
  expect_error(geom_freq_bar_ci(errorbar.params = list(position = "identity")), "shared")
  expect_error(geom_freq_bar_ci(orientation = "y"), "shared")
  d <- ggFrequencyBarData()[1, ]
  b <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(query, ipm)) +
    geom_freq_bar_ci(position = "identity"))$data
  expect_equal(as.numeric(b[[1]]$x), as.numeric(b[[2]]$x))
})
