ecSpec <- function(e) e$jsHooks$render[[1]]$data

ecYearData <- function(queries = c("one", "two"), years = 2001:2004) {
  d <- expand.grid(year = years, query = queries, stringsAsFactors = FALSE)
  d$vc <- paste("pubDate in", d$year)
  d$totalResults <- seq_len(nrow(d)) * 10
  d$total <- 1e6
  d$webUIRequestUrl <- paste0("https://example.invalid/?row=", seq_len(nrow(d)))
  suppressWarnings(ci(d))
}

ecBarData <- function(queries = c("Berlin", "Paris"), corpora = c("A", "B")) {
  d <- expand.grid(query = queries, vc = corpora, stringsAsFactors = FALSE)
  d$totalResults <- c(1, 40, 30, 400)[seq_len(nrow(d))]
  d$total <- c(1e7, 1e6, 1e6, 1e6)[seq_len(nrow(d))]
  d$webUIRequestUrl <- paste0("https://example.invalid/?row=", seq_len(nrow(d)))
  suppressWarnings(ci(d))
}

ecDispersionData <- function(labels = c("star", "colon"), parts = c("a", "b", "c")) {
  tibble::tibble(query = labels, variant = labels, partition = "source", estimated = FALSE,
    sampling = rep(list(list(inclusionProbability = 1)), length(labels)),
    distribution = rep(list(data.frame(part = parts, vc = parts, tokens = c(500, 300, 200),
      frequency = c(5, 3, 2), observed = c(5, 3, 2),
      webUIRequestUrl = paste0("https://example.invalid/?part=", 1:3))), length(labels)))
}

test_that("ECharts plots show the values, intervals, links and texts of the Highcharts plots", {
  skip_if_not_installed("echarts4r")
  compare <- function(ec, hc, fields) {
    series <- Filter(function(s) is.null(s$linkedTo), hc$x$hc_opts$series)
    expect_equal(vapply(ec[seq_along(series)], `[[`, "", "name"), vapply(series, `[[`, "", "name"))
    for (i in seq_along(series)) for (field in fields) {
      expect_equal(lapply(ec[[i]]$data, function(p) p$korap[[field]]),
        lapply(series[[i]]$data, `[[`, field), info = paste(i, field))
    }
    for (i in seq_along(series)) expect_equal(lapply(ec[[i]]$data, `[[`, "webUIRequestUrl"),
      lapply(series[[i]]$data, `[[`, "webUIRequestUrl"))
  }
  frequency <- c("y", "count", "total", "confLow", "confHigh", "korapTooltipName", "korapTooltipUnit")
  for (alternatives in c(TRUE, FALSE)) {
    d <- ecYearData(); d$as.alternatives <- alternatives
    compare(ecSpec(ec_freq_by_year_ci(d))$option$series, hc_freq_by_year_ci(d), setdiff(frequency, "y"))
    expect_equal(lapply(ecSpec(ec_freq_by_year_ci(d))$option$series[[1]]$data, function(p) p$korap$y),
      lapply(hc_freq_by_year_ci(d)$x$hc_opts$series[[1]]$data, `[[`, "y"))
    b <- ecBarData(); b$as.alternatives <- alternatives
    compare(ecSpec(ec_freq_bar_ci(b))$option$series, hc_freq_bar_ci(b), frequency)
    expect_equal(ecSpec(ec_freq_bar_ci(b))$option$series[[3]]$data[[2]][[2]],
      b$conf.low[3] * if (alternatives) 100 else 1e6)
  }
  compare(ecSpec(ec_dispersion(ecDispersionData(), top = 1))$option$series, hc_dispersion(ecDispersionData(), top = 1),
    c("y", "frequency", "tokens", "perMillion"))
})

test_that("ECharts plots apply the same checks and honour showCI and the logarithmic axis", {
  skip_if_not_installed("echarts4r")
  d <- ecBarData()
  expect_error(ec_freq_bar_ci(ipm(d)), "do not call ipm")
  expect_error(ec_freq_bar_ci(d, data = 1), "managed by ec_freq_bar_ci")
  expect_error(ec_freq_by_year_ci(ecYearData(), stack = "x"), "managed by ec_freq_by_year_ci")
  expect_error(ec_freq_by_year_ci(transform(ecYearData(), year = year + .5)), "whole calendar years")
  expect_error(ec_dispersion(ecDispersionData(), top = 0), "top must be")
  bare <- d[setdiff(names(d), c("conf.low", "conf.high"))]
  expect_length(ecSpec(ec_freq_bar_ci(bare, showCI = FALSE))$option$series, 2)
  expect_length(ecSpec(ec_freq_bar_ci(d))$option$series, 4)
  expect_length(ecSpec(ec_freq_by_year_ci(ecYearData()))$option$series, 6)
  expect_length(ecSpec(ec_freq_by_year_ci(ecYearData(), showCI = FALSE))$option$series, 2)
  base <- ecSpec(ec_freq_bar_ci(d, log = TRUE))$option$xAxis$startValue
  expect_true(base < min(d$conf.low[d$conf.low > 0]) * 1e6)
  expect_identical(ecSpec(ec_freq_bar_ci(d, horizontal = FALSE))$option$xAxis$type, "category")
  expect_identical(ecSpec(ec_freq_bar_ci(d, barGap = "50%"))$option$series[[1]]$barGap, "50%")
})

test_that("query expressions and metadata are passed as data and not as chart options", {
  skip_if_not_installed("echarts4r")
  for (e in list(ec_freq_by_year_ci(ecYearData("window.HACKED=1")), ec_freq_bar_ci(ecBarData("window.HACKED=1")),
      ec_dispersion(ecDispersionData("window.HACKED=1")))) {
    expect_length(e$x$opts, 0)
    expect_match(as.character(htmlwidgets:::toJSON(e$jsHooks)), "window.HACKED=1", fixed = TRUE)
  }
})

test_that("personal ECharts themes can be a name, a list or a JSON file", {
  skip_if_not_installed("echarts4r")
  d <- ecBarData()
  expect_identical(ec_freq_bar_ci(d)$x$theme, "")
  rlang::local_options(rkorap.chart.theme = "dark")
  expect_identical(ec_freq_bar_ci(d)$x$theme, "dark")
  options(rkorap.chart.theme = list(color = "#003366", backgroundColor = "#FBF7EE"))
  theme <- ec_freq_bar_ci(d)$x$customTheme
  expect_identical(jsonlite::fromJSON(theme, simplifyVector = FALSE)$color, list("#003366"))
  file <- withr::local_tempfile(fileext = ".json", lines = theme)
  options(rkorap.chart.theme = file)
  expect_identical(jsonlite::fromJSON(ec_freq_bar_ci(d)$x$customTheme)$backgroundColor, "#FBF7EE")
  options(rkorap.chart.theme = 1)
  expect_error(ec_freq_bar_ci(d), "rkorap.chart.theme must be")
})

test_that("rendered ECharts plots are safe, localized, clickable and switchable", {
  skip_if_not(identical(Sys.getenv("RKORAP_BROWSER_TESTS"), "true"),
    "Set RKORAP_BROWSER_TESTS=true to run headless browser regressions")
  skip_if_not_installed("echarts4r")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome or Chromium is required")
  directory <- withr::local_tempdir()
  browser <- chromote::Chromote$new()
  on.exit(browser$close(), add = TRUE)
  session <- browser$new_session(width = 900, height = 600)
  evaluate <- function(expression) {
    result <- session$Runtime$evaluate(expression, returnByValue = TRUE, awaitPromise = TRUE)
    expect_null(result$exceptionDetails, info = expression)
    result$result$value
  }
  render <- function(...) {
    file <- tempfile("chart-", tmpdir = directory, fileext = ".html")
    htmltools::save_html(htmltools::tagList(...), file, libdir = "lib")
    session$Page$navigate(paste0("file://", utils::URLencode(normalizePath(file))))
    expect_true(evaluate("new Promise((resolve, reject) => {
      const deadline = Date.now() + 10000;
      (function check() {
        const all = window.echarts ? [...document.querySelectorAll('.echarts4r')] : [];
        const charts = all.map(el => echarts.getInstanceByDom(el));
        if (charts.length && charts.every(c => c && c.getOption() && c.getOption().series.length)) {
          window.charts = charts; window.c = charts[0]; window.opened = [];
          window.open = (url, target) => { window.opened.push([url, target]); return { focus() {} }; };
          setTimeout(() => resolve(true), 1200);
        } else if (Date.now() > deadline) reject(new Error('Chart did not render'));
        else setTimeout(check, 25);
      })();
    })"))
  }
  move <- function(at) {
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = at[[1]], y = at[[2]])
    Sys.sleep(.4)
  }
  click <- function(at) {
    move(at)
    for (type in c("mousePressed", "mouseReleased"))
      session$Input$dispatchMouseEvent(type = type, x = at[[1]], y = at[[2]], button = "left", clickCount = 1)
    Sys.sleep(.6)
  }
  # Position in the page, not only in the chart.
  pixel <- function(series, value, chart = 0) evaluate(sprintf("(() => { const c = charts[%d];
    const p = c.convertToPixel({seriesIndex: %d}, %s), b = c.getDom().getBoundingClientRect();
    return [p[0] + b.left, p[1] + b.top]; })()", chart, series, value))
  tooltip <- function() evaluate("[...document.querySelectorAll('.echarts4r > div')].map(d => d.innerText).join('|')")
  options <- function(x) evaluate(sprintf("JSON.stringify(%s)", x))
  selected <- function() unlist(jsonlite::fromJSON(options("c.getOption().legend[0].selected")))

  rlang::local_options(rkorap.chart.language = "de")
  evil <- c("window.HACKED=1", "<img src=x onerror=\"window.XSS=1\">")
  years <- ecYearData(evil)
  years[years$year == 2003, c("f", "conf.low", "conf.high")] <- NA
  render(ec_freq_by_year_ci(years, smooth = TRUE))
  at <- pixel(0, "['2002-01-01', c.getOption().series[0].data[1].value[1]]")
  move(at)
  shown <- tooltip()
  expect_match(shown, "2002", fixed = TRUE)
  expect_match(shown, "Relative Häufigkeit", fixed = TRUE)
  expect_match(shown, "<img src=x", fixed = TRUE)
  expect_equal(evaluate("document.querySelectorAll('.echarts4r img').length"), 0)
  click(at)
  expect_identical(options("opened"), '[["https://example.invalid/?row=2","korap"]]')
  expect_identical(evaluate("String(window.HACKED) + String(window.XSS)"), "undefinedundefined")
  # The gap breaks curve and band; the years are not added up by the stacked band.
  expect_identical(evaluate("c.getOption().series[0].data[2].value[1]"), "-")
  expect_identical(evaluate("c.getOption().series[2].data[2].value[1]"), "-")
  expect_true(evaluate("c.getModel().getComponent('xAxis').axis.scale.getExtent()[1] < new Date('2005-01-01').getTime()"))
  expect_identical(evaluate("c.getVisual({seriesIndex: 1}, 'color')"), "#FF7F0E")
  expect_identical(evaluate("c.getVisual({seriesIndex: 5}, 'color')"), "#FF7F0E")

  # Real clicks on the buttons next to the legend.
  button <- function(text) evaluate(sprintf("(() => {
    let found; c.getZr().storage.traverse(function (el) {
      if (el.style && el.style.text === '%s') { const r = el.getBoundingRect().clone(); r.applyTransform(el.transform || [1,0,0,1,0,0]); found = r; } });
    const b = c.getDom().getBoundingClientRect();
    return [b.left + found.x + found.width / 2, b.top + found.y + found.height / 2]; })()", text))
  click(button("Keine"))
  expect_identical(unname(selected()), c(FALSE, FALSE))
  click(button("Keine"))
  expect_identical(unname(selected()), c(FALSE, FALSE))
  click(button("Alle"))
  expect_identical(unname(selected()), c(TRUE, TRUE))
  evaluate(sprintf("c.dispatchAction({type: 'legendUnSelect', name: '%s'}); 1", evil[1]))
  Sys.sleep(.6)
  click(button("Keine"))
  expect_identical(unname(selected()), c(FALSE, FALSE))

  # Each whisker keeps to its own bar, in both orientations and with a hidden group.
  rlang::local_options(rkorap.chart.language = "en")
  for (horizontal in c(TRUE, FALSE)) for (hidden in c(FALSE, TRUE)) {
    render(ec_freq_bar_ci(ecBarData(), horizontal = horizontal))
    if (hidden) { evaluate("c.dispatchAction({type: 'legendUnSelect', name: 'Berlin'}); 1"); Sys.sleep(1.2) }
    offsets <- evaluate(sprintf("(() => {
      const bar = [], whisker = [], across = %s ? 'y' : 'x', size = %s ? 'height' : 'width';
      c.getModel().eachSeries(function (s) {
        const data = s.getData(); if (!data.count() || c.isDisposed()) return;
        data.eachItemGraphicEl(function (el) {
          const r = el.getBoundingRect().clone();
          (s.subType === 'bar' ? bar : whisker).push(r[across] + r[size] / 2);
        });
      });
      return [bar, whisker]; })()", tolower(horizontal), tolower(horizontal)))
    expect_length(offsets[[1]], if (hidden) 2 else 4)
    expect_equal(sort(unlist(offsets[[1]])), sort(unlist(offsets[[2]])), tolerance = 1e-2)
  }
  # Values below 1 on a logarithmic axis start at the axis, too.
  render(ec_freq_bar_ci(ecBarData(), log = TRUE))
  starts <- unlist(evaluate("(() => { const x = []; c.getModel().getSeriesByIndex(0).getData().eachItemGraphicEl(el => {
    const r = el.getBoundingRect(); x.push(Math.round(Math.min(r.x, r.x + r.width))); }); return x; })()"))
  expect_length(unique(starts), 1)

  # Localized dispersion chart, several charts on a page, themes and echarts4r customization.
  rlang::local_options(rkorap.chart.language = "de")
  custom <- ec_freq_bar_ci(ecBarData()) |> echarts4r::e_theme("dark") |>
    echarts4r::e_title("Eigener Titel") |> echarts4r::e_legend(bottom = 0)
  render(ec_dispersion(ecDispersionData(), top = 1), custom)
  expect_length(evaluate("charts.map(c => c.getOption().series.length)"), 2)
  expect_identical(evaluate("c.getOption().title[0].text"), "Verteilung nach Quelle")
  expect_match(evaluate("c.getOption().title[1].text"), "Teilkorpora einzeln dargestellt", fixed = TRUE)
  expect_identical(evaluate("c.getOption().series[0].name"), "Korpustokens")
  expect_identical(evaluate("c.getOption().yAxis[0].data[1]"), "Übrige 2 Teilkorpora")
  expect_identical(evaluate("c.getVisual({seriesIndex: 2}, 'color')"), "#E69F00")
  move(pixel(1, "[10, 0]"))
  expect_match(tooltip(), "Anteil an der Gesamthäufigkeit", fixed = TRUE)
  expect_identical(evaluate("charts[1].getOption().title[0].text"), "Eigener Titel")
  expect_identical(evaluate("charts[1].getOption().legend[0].selector[1].title"), "Keine")
  expect_false(identical(evaluate("charts[1].getVisual({seriesIndex: 0}, 'color')"), "#1F77B4"))
})
