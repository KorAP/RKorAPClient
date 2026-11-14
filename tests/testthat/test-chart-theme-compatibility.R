test_that("the Google theme retains its base style and accepts helper overrides", {
  colors <- c("#123456", "#abcdef")
  theme <- chartGoogleTheme(colors = colors,
    plotOptions = list(errorbar = list(color = "#737373")))

  expect_s3_class(theme, "hc_theme")
  expect_identical(theme$colors, colors)
  expect_identical(theme$chart$style$fontFamily, "Roboto")
  expect_identical(theme$xAxis$gridLineColor, "#F3F3F3")
  expect_identical(theme$plotOptions$errorbar$color, "#737373")
})

test_that("the Google theme selects the new preset API and forwards overrides", {
  requested <- NULL
  local_mocked_bindings(hc_theme = function(preset = NULL, ...) {
    requested <<- preset
    structure(list(...), class = "hc_theme")
  }, .package = "highcharter")

  theme <- chartGoogleTheme(colors = c("red", "blue"),
    plotOptions = list(arearange = list(fillOpacity = 0.15)))

  expect_identical(requested, "google")
  expect_identical(theme$colors, c("red", "blue"))
  expect_identical(theme$plotOptions$arearange$fillOpacity, 0.15)
})
