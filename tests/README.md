# Chart regression checks

Run the chart tests, including rendering in Chrome or Chromium:

```sh
RKORAP_BROWSER_TESTS=true Rscript -e 'devtools::test(filter = "chart|hc-|show-ci|geom-freq")'
```

The browser test additionally needs the suggested `chromote` package and a local
Chrome or Chromium installation. It is opt-in and does not run in ordinary CRAN
checks. It starts a separate headless browser and closes that process on exit.
Fixtures use local HTML and the Highcharts scripts bundled with highcharter;
external fonts are disabled, and no corpus queries are made.

`test-chart-browser.R` reads the rendered SVG tooltip text after showing a real
Highcharts tooltip. It checks corpus headers, escaped labels, German labels for
combined subcorpora and the shared year tooltip. It also checks rendered series
colours under highcharter's unmodified startup theme. These assertions complement
the R-option and JavaScript-model tests; they do not emulate tooltip formatting.
For multi-query year plots, text positions must place each query on a new line
after the preceding block, in German and English, with and without intervals.

Keep the default-theme checks in a fresh R session without setting
`highcharter.theme`: setting a test theme would hide regressions caused by
highcharter's automatic startup option. Tests with custom themes separately
verify that explicit palettes and typography still take precedence.
