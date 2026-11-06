test_that("the unchanged highcharter startup theme does not replace helper palettes", {
  # Do not set highcharter.theme here: that hid the startup-theme regression.
  startup <- highcharter::hc_theme(chart = list(backgroundColor = "transparent"),
    colors = getOption("highcharter.color_palette"))
  skip_if_not(identical(getOption("highcharter.theme"), startup),
    "Requires highcharter's unmodified startup theme")
  before <- getOption("highcharter.theme")
  d <- suppressWarnings(ci(data.frame(query = c("one", "two"), vc = "",
    totalResults = c(1, 2), total = 100)))
  expect_identical(hc_freq_bar_ci(d)$x$theme$colors[2], "#E69F00")
  shares <- tibble::tibble(query = "one", partition = "source", estimated = FALSE,
    sampling = list(list(inclusionProbability = 1)),
    distribution = list(data.frame(part = "part", vc = "part", tokens = 100, frequency = 1, observed = 1)))
  expect_identical(hc_dispersion(shares)$x$theme$colors[2], "#E69F00")
  d$year <- c(2023, 2024)
  year <- hc_freq_by_year_ci(d)
  expect_identical(year$x$theme$colors[2], "#FF7F0E")
  expect_length(year$x$theme$colors, 20L)
  expect_identical(getOption("highcharter.theme"), before)
})

test_that("customizations of the startup theme are retained even with its original colours", {
  theme <- highcharter::hc_theme(chart = list(backgroundColor = "transparent",
    style = list(fontFamily = "serif")), colors = getOption("highcharter.color_palette"))
  rlang::local_options(highcharter.theme = theme)
  d <- suppressWarnings(ci(data.frame(query = "one", vc = "", totalResults = 1, total = 100)))
  h <- hc_freq_bar_ci(d)
  expect_identical(h$x$theme$colors, theme$colors)
  expect_identical(h$x$theme$chart$style$fontFamily, "serif")
})

test_that("frequency results retain denominator mode and query labels for all chart helpers", {
  kco <- methods::new("KorAPConnection", apiUrl = "https://example.invalid/",
    KorAPUrl = "https://example.invalid/", authorizationSupported = FALSE, verbose = FALSE)
  local_mocked_bindings(
    corpusQuery = function(kco, query, vc, ...) tibble::tibble(query = unname(query),
      vc = rep(vc, length.out = length(query)), totalResults = c(30, 70)),
    corpusStats = function(kco, vc, ...) tibble::tibble(tokens = rep(1000, length(vc))),
    .package = "RKorAPClient")
  queries <- c(star = "[orth='Nutzer*innen']", colon = "[orth='Nutzer:innen']")
  d <- frequencyQuery(kco, queries, as.alternatives = TRUE)
  expect_identical(d$variant, names(queries))
  expect_identical(d$as.alternatives, c(TRUE, TRUE))
  h <- hc_freq_bar_ci(d)
  expect_identical(h$x$hc_opts$xAxis$categories, list("star", "colon"))
  expect_equal(h$x$hc_opts$series[[1]]$data[[1]]$y, 30)
  expect_equal(hc_freq_bar_ci(d[1, ])$x$hc_opts$series[[1]]$data[[1]]$y, 30)
  expect_error(hc_freq_bar_ci(d, as.alternatives = FALSE), "disagrees with frequencyQuery")
  d$year <- c(2023, 2024)
  year <- hc_freq_by_year_ci(d)
  expect_equal(year$x$hc_opts$series[[1]]$name, "star")
  expect_error(hc_freq_by_year_ci(d, as.alternatives = FALSE), "disagrees with frequencyQuery")
  ordinary <- frequencyQuery(kco, queries)
  expect_error(hc_freq_bar_ci(ordinary, as.alternatives = TRUE), "disagrees with frequencyQuery")
  expect_error(hc_freq_bar_ci(dplyr::bind_rows(d, ordinary)), "one recorded")
})

test_that("shared Highcharts defaults keep colors customizable and use one guarded click handler", {
  d <- suppressWarnings(ci(data.frame(query = c("<s>", "other"), vc = "", totalResults = c(1, 2), total = 100)))
  h <- hc_freq_bar_ci(d) |> highcharter::hc_colors(c("red", "blue"))
  expect_identical(h$x$hc_opts$colors, c("red", "blue"))
  expect_null(h$x$hc_opts$series[[1]][["color"]])
  expect_identical(h$x$hc_opts$series[[1]]$colorIndex, 0L)
  d$year <- c(2023, 2024)
  year <- hc_freq_by_year_ci(d)
  expect_identical(h$x$hc_opts$plotOptions$series$point$events,
    year$x$hc_opts$plotOptions$series$point$events)
  click <- as.character(h$x$hc_opts$plotOptions$series$point$events$click)
  expect_match(click, "window.open(this.webUIRequestUrl, 'korap')", fixed = TRUE)
  expect_false(grepl("w.location", click, fixed = TRUE))
  expect_false(grepl("noopener", click, fixed = TRUE))
  expect_match(as.character(year$x$hc_opts$legend$labelFormatter), "&lt;", fixed = TRUE)
})

test_that("global and subsequently applied themes control palettes and foreground styling", {
  theme <- highcharter::hc_theme(colors = c("#F18700", "#1E2662"),
    chart = list(backgroundColor = "#2a2a2a", style = list(fontFamily = "Fira Sans Condensed")),
    plotOptions = list(series = list(dataLabels = list(color = "white")), errorbar = list(color = "white")))
  rlang::local_options(highcharter.theme = theme)
  d <- suppressWarnings(ci(data.frame(query = c("one", "two"), vc = "", totalResults = c(1, 2), total = 100)))
  frequency <- hc_freq_bar_ci(d)
  d$year <- c(2023, 2024)
  year <- hc_freq_by_year_ci(d)
  shares <- tibble::tibble(query = "one", partition = "source", estimated = FALSE,
    sampling = list(list(inclusionProbability = 1)),
    distribution = list(data.frame(part = "part", vc = "part", tokens = 100, frequency = 1, observed = 1)))
  dispersion <- hc_dispersion(shares)
  for (h in list(frequency, year, dispersion)) {
    expect_identical(h$x$theme$colors, theme$colors)
    expect_identical(h$x$theme$chart$style$fontFamily, "Fira Sans Condensed")
    expect_null(h$x$hc_opts[["colors"]])
    expect_null(h$x$hc_opts$chart$style$fontFamily)
    expect_identical(h$x$theme$plotOptions$errorbar$color, "white")
    replacement <- highcharter::hc_theme(colors = c("#000000", "#737373"),
      chart = list(backgroundColor = "white", style = list(fontFamily = "serif")))
    changed <- highcharter::hc_add_theme(h, replacement)
    expect_identical(changed$x$theme$colors, replacement$colors)
    expect_identical(changed$x$theme$chart$style$fontFamily, "serif")
  }
  expect_null(frequency$x$hc_opts$series[[2]][["color"]])
  expect_null(dispersion$x$hc_opts$plotOptions$series$dataLabels$style$color)
  expect_null(dispersion$x$hc_opts$xAxis$labels$style$fontSize)
})
