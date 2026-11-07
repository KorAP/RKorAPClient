yearSplineData <- function() data.frame(
  year = 2020:2024, frequency = c(1, 8, 2, 6, 3),
  lower = c(.5, 6, 1, 4, 2), upper = c(2, 10, 3, 8, 4),
  webUIRequestUrl = "https://example.invalid/")

yearSplinePlot <- function(d = yearSplineData(), ...) {
  ggplot2::ggplot(d, ggplot2::aes(year, frequency)) +
    geom_freq_by_year_ci(ggplot2::aes(ymin = lower, ymax = upper), ...)
}

test_that("curved year plots retain the observed points and supplied annual limits", {
  d <- yearSplineData()
  b <- ggplot2::ggplot_build(yearSplinePlot(smooth = TRUE))$data
  expect_length(b, 3)
  expect_gt(nrow(b[[2]]), nrow(d))
  expect_equal(b[[3]]$x, d$year)
  expect_equal(b[[3]]$y, d$frequency)
  knots <- match(d$year, b[[2]]$x)
  bounds <- match(d$year, b[[1]]$x)
  expect_equal(b[[2]]$y[knots], d$frequency)
  expect_equal(b[[1]]$ymin[bounds], d$lower)
  expect_equal(b[[1]]$ymax[bounds], d$upper)
  expect_true(all(b[[2]]$y >= 0))
  expect_true(all(b[[1]]$ymin >= 0 & b[[1]]$ymin <= b[[1]]$ymax))
  expect_equal(range(b[[2]]$x), range(d$year))
  expect_true(all(b[[1]]$ymin <= b[[2]]$y & b[[1]]$ymax >= b[[2]]$y))
  zeros <- transform(d, frequency = c(0, 0, 10, 0, 0))
  zeroCurve <- ggplot2::ggplot_build(yearSplinePlot(zeros, smooth = TRUE, showCI = FALSE))$data[[1]]
  expect_true(all(zeroCurve$y >= 0))
})

test_that("spline groups and facets remain separate and themes retain control", {
  d <- rbind(transform(yearSplineData(), term = "a", corpus = "one"),
    transform(yearSplineData(), term = "b", corpus = "one", frequency = frequency + 20,
      lower = lower + 20, upper = upper + 20))
  d <- rbind(d, transform(d, corpus = "two", frequency = frequency * 2,
    lower = lower * 2, upper = upper * 2))
  p <- ggplot2::ggplot(d, ggplot2::aes(year, frequency, colour = term, fill = term)) +
    geom_freq_by_year_ci(ggplot2::aes(ymin = lower, ymax = upper), smooth = TRUE) +
    ggplot2::facet_wrap(~corpus) +
    ggplot2::scale_colour_manual(values = c(a = "red", b = "blue")) +
    ggplot2::scale_fill_manual(values = c(a = "red", b = "blue"))
  b <- ggplot2::ggplot_build(p)$data
  for (panel in unique(b[[2]]$PANEL)) {
    curves <- b[[2]][b[[2]]$PANEL == panel, ]
    expect_length(unique(curves$group), 2)
    expect_equal(sort(unique(curves$colour)), c("blue", "red"))
  }
  expect_equal(nrow(b[[3]]), nrow(d))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("missing observations and missing calendar years leave gaps", {
  d <- yearSplineData()[c(1, 2, 4, 5), ]
  for (observations in list(d, transform(yearSplineData(),
      frequency = replace(frequency, 3, NA_real_)))) {
    b <- ggplot2::ggplot_build(yearSplinePlot(observations, smooth = TRUE, na.rm = TRUE))$data
    for (layer in b[1:2]) {
      expect_length(unique(layer$group), 2)
      expect_false(any(layer$x > 2021 & layer$x < 2023))
    }
  }
})

test_that("one year retains its point and two years give a linear connection", {
  one <- ggplot2::ggplot_build(yearSplinePlot(yearSplineData()[1, ], smooth = TRUE))$data
  expect_equal(nrow(one[[2]]), 0)
  expect_equal(one[[3]]$y, 1)
  two <- ggplot2::ggplot_build(yearSplinePlot(yearSplineData()[1:2, ], smooth = TRUE))$data
  expect_equal(two[[2]]$y, 1 + 7 * (two[[2]]$x - 2020))
})

test_that("log axes and hidden bands keep the original frequency scale", {
  d <- yearSplineData()
  p <- yearSplinePlot(d, smooth = TRUE) + ggplot2::scale_y_log10()
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(b[[3]]$y, log10(d$frequency))
  expect_equal(b[[1]]$ymin[match(d$year, b[[1]]$x)], log10(d$lower))
  expect_equal(b[[1]]$ymax[match(d$year, b[[1]]$x)], log10(d$upper))
  expect_true(all(is.finite(b[[2]]$y)))
  bare <- d[, setdiff(names(d), c("lower", "upper"))]
  hidden <- ggplot2::ggplot_build(yearSplinePlot(bare, smooth = TRUE, showCI = FALSE) +
    ggplot2::scale_y_log10())$data
  expect_length(hidden, 2)
  expect_equal(hidden[[1]]$x, b[[2]]$x)
  expect_equal(hidden[[1]]$y, b[[2]]$y)
  expect_equal(hidden[[2]]$y, log10(d$frequency))
})

test_that("the default remains straight and ambiguous annual groups are rejected", {
  straight <- ggplot2::ggplot_build(yearSplinePlot())$data
  expect_equal(straight[[2]]$x, yearSplineData()$year)
  expect_equal(straight[[2]]$y, yearSplineData()$frequency)
  expect_error(geom_freq_by_year_ci(smooth = NA), "smooth must be TRUE or FALSE")
  expect_error(geom_freq_by_year_ci(smooth = TRUE, stat = "identity"), "stat cannot be overridden")
  d <- rbind(yearSplineData(), yearSplineData()[1, ])
  expect_error(ggplot2::ggplot_build(yearSplinePlot(d, smooth = TRUE)), "one observation per year")
})
