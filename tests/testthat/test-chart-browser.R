# Opt in with RKORAP_BROWSER_TESTS=true. Use a dedicated headless process,
# bundled widget scripts and local HTML; never reuse the user's browser.
test_that("rendered tooltips include corpus headers and use the intended palettes", {
  skip_if_not(identical(Sys.getenv("RKORAP_BROWSER_TESTS"), "true"),
    "Set RKORAP_BROWSER_TESTS=true to run headless browser regressions")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome or Chromium is required")
  rlang::local_options(highcharter.google_fonts = FALSE)
  directory <- tempfile("chart-browser-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  browser <- chromote::Chromote$new()
  on.exit(browser$close(), add = TRUE, after = FALSE)
  session <- browser$new_session()

  render <- function(chart) {
    file <- tempfile("chart-", tmpdir = directory, fileext = ".html")
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session$Page$navigate(paste0("file://", utils::URLencode(normalizePath(file))))
    ready <- session$Runtime$evaluate(
      "new Promise((resolve, reject) => {
        const deadline = Date.now() + 10000;
        function check() {
          if (window.Highcharts && Highcharts.charts.some(c => c && c.hasLoaded)) resolve(true);
          else if (Date.now() > deadline) reject(new Error('Chart did not render'));
          else setTimeout(check, 25);
        }
        check();
      })", awaitPromise = TRUE, returnByValue = TRUE)
    expect_null(ready$exceptionDetails)
    expect_true(ready$result$value)
  }
  evaluate <- function(expression) {
    result <- session$Runtime$evaluate(expression, returnByValue = TRUE)
    expect_null(result$exceptionDetails)
    result$result$value
  }
  tooltip <- function(series, point) evaluate(sprintf(
    "(() => {
      const chart = Highcharts.charts.find(c => c && c.hasLoaded);
      chart.tooltip.refresh(chart.series[%d].points[%d]);
      return { text: chart.tooltip.label.element.textContent, hidden: chart.tooltip.isHidden };
    })()", series, point))

  d <- suppressWarnings(ci(data.frame(query = rep(c("star", "colon"), each = 2),
    vc = rep(c("award=buchpreis", ""), 2), totalResults = c(10, 20, 30, 40), total = 1000,
    label = rep(c("Book prize", "<img src=x>"), 2))))
  rlang::local_options(rkorap.chart.language = "en")
  render(hc_freq_bar_ci(d))
  shown <- tooltip(0, 0)
  expect_false(shown$hidden)
  expect_true(startsWith(shown$text, "Book prize"), info = shown$text)
  expect_match(shown$text, "Absolute frequency", fixed = TRUE)
  escaped <- tooltip(0, 1)
  expect_true(startsWith(escaped$text, "<img src=x>"), info = escaped$text)
  expect_equal(evaluate("document.querySelectorAll('.highcharts-tooltip img').length"), 0)

  startup <- highcharter::hc_theme(chart = list(backgroundColor = "transparent"),
    colors = getOption("highcharter.color_palette"))
  # Actual series colours, with the option left as highcharter initialized it.
  if (identical(getOption("highcharter.theme"), startup)) {
    expect_identical(evaluate("Highcharts.charts.find(Boolean).series[2].points[0].graphic.attr('fill')"), "#E69F00")
  }

  rlang::local_options(rkorap.chart.language = "de")
  distribution <- data.frame(part = c("Quelle A", "Quelle B", "Quelle C"),
    vc = c("source=a", "source=b", "source=c"), tokens = c(500, 300, 200),
    frequency = c(5, 3, 2), observed = c(5, 3, 2))
  dispersion <- tibble::tibble(query = c("star", "colon"), partition = "source", estimated = FALSE,
    sampling = list(list(inclusionProbability = 1), list(inclusionProbability = 1)),
    distribution = list(distribution, distribution))
  render(hc_dispersion(dispersion, top = 1))
  for (series in 0:1) {
    shown <- tooltip(series, 1)
    expect_false(shown$hidden)
    expect_true(startsWith(shown$text, "Übrige 2 Teilkorpora"), info = shown$text)
  }
  expect_match(shown$text, "Absolute Häufigkeit", fixed = TRUE)
  if (identical(getOption("highcharter.theme"), startup)) {
    expect_identical(evaluate("Highcharts.charts.find(Boolean).series[2].points[1].graphic.attr('fill')"), "#E69F00")
  }

  # The shared year tooltip has a different context and must keep its year header.
  rlang::local_options(rkorap.chart.language = "en")
  d$year <- rep(c(2023, 2024), 2)
  d$vc <- rep(c("pubDate in 2023", "pubDate in 2024"), 2)
  d$label <- NULL
  render(hc_freq_by_year_ci(d))
  text <- evaluate("(() => {
    const c = Highcharts.charts.find(Boolean);
    c.tooltip.refresh([c.series[0].points[0], c.series[2].points[0]]);
    return c.tooltip.label.element.textContent;
  })()")
  expect_true(startsWith(text, "2023"), info = text)
  if (identical(getOption("highcharter.theme"), startup)) {
    expect_identical(evaluate("Highcharts.charts.find(Boolean).series[2].graph.attr('stroke')"), "#FF7F0E")
  }

  lemmas <- c("zutiefst", "präzise", "entscheidend", "unterstreichen", "zeigen")
  multi <- suppressWarnings(ci(data.frame(query = rep(lemmas, each = 2),
    condition = rep(lemmas, each = 2), year = rep(2019:2020, 5),
    vc = rep(c("pubDate in 2019", "pubDate in 2020"), 5),
    totalResults = rep(c(5126, 9457, 57183, 17530, 453473), each = 2), total = 751802824)))
  for (language in c("de", "en")) for (showCI in c(TRUE, FALSE)) {
    options(rkorap.chart.language = language)
    render(hc_freq_by_year_ci(multi, smooth = TRUE, showCI = showCI))
    layout <- evaluate("(() => {
      const chart = Highcharts.charts.find(Boolean);
      const series = chart.series.filter(s => s.name !== 'ci');
      chart.tooltip.refresh(series.map(s => s.points[1]));
      const walker = document.createTreeWalker(chart.tooltip.label.element, NodeFilter.SHOW_TEXT);
      const nodes = [];
      while (walker.nextNode()) {
        const text = walker.currentNode.textContent.replace(/\\u200b/g, '').trim();
        if (!text || text === '●') continue;
        const range = document.createRange(); range.selectNodeContents(walker.currentNode);
        nodes.push({text, top: range.getBoundingClientRect().top});
      }
      return series.map(s => {
        const i = nodes.findIndex(n => n.text === s.name);
        return {name: s.name, found: i >= 0, top: i >= 0 ? nodes[i].top : null,
          previousTop: i > 0 ? nodes[i - 1].top : null,
          previousText: i > 0 ? nodes[i - 1].text : null};
      });
    })()")
    expect_length(layout, length(lemmas))
    for (entry in layout) {
      expect_true(entry$found, info = entry$name)
      # Inspect real text positions: a missing <br/> put the next lemma on
      # the same line as the previous interval (or corpus size without CIs).
      expect_true(entry$top > entry$previousTop + 5,
        info = paste(language, showCI, entry$name, "after", entry$previousText))
    }
  }
})
