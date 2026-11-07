# Test suites

## Offline HTTP pilot

Run the fixture and local-transport tests without contacting a KorAP server:

```sh
RKORAP_LLM_MODELS="" Rscript -e 'devtools::test(filter = "http-fixtures|http-transport|^corpusStats$|^KorAPConnection$")'
```

Install the suggested `vcr` (>= 2.0.0), `webfakes` and `callr` packages to run both
parts. `test-http-fixtures.R` covers connection initialization, count queries,
a two-page fetch, continuation and response caching. It uses synthetic response
files with the real `apiCall()`, URL construction and JSON parsing. Connectivity
probing is replaced only inside that context, the cache test uses a temporary
directory, and an unexpected request fails without falling back to the internet.
vcr uses httr2's mocking hook directly. Cassettes match HTTP method and URI, use
JSON serialization and run with `record = "none"`; tests never record implicitly.
The four formerly live `test-corpusStats.R` cases now use synthetic responses
too, checking exact sizes, both output formats, URL encoding and printing.
`test-KorAPConnection.R` also runs offline: HTTP errors use small per-scenario
cassettes, resolver/transport failures are injected at the curl boundary, and
credential tests use the real keyring front end with an in-memory backend.
Cache tests use temporary directories, including when clearing the cache.

`test-http-transport.R` uses temporary local `webfakes` servers on automatically
allocated ports to exercise actual httr2 retries and curl timeouts. Server
processes are stopped after each test. These tests need loopback networking,
but no internet, and are skipped on CRAN; the fixture tests still run there.

The pilot has also been run in a Linux network namespace with no external
network interface, with loopback enabled for the local-server tests.

Run the deliberately separate live integration tests explicitly:

```sh
RKORAP_LIVE_TESTS=true RKORAP_LLM_MODELS="" Rscript -e 'devtools::test(filter = "^live-http$")'
```

These check real server compatibility, including corpus sizes, and retain the
former successive `maxFetch` scenario. They are skipped by default and on CRAN.
They use no access token or local response cache. The original connection-message,
basic count, maxFetch and corpus-statistics cases use this fixture/live split.
`checkKorapHttpWorkflow()` applies the same positive assertions offline and live
to initialization, search, two-page fetching, positions and corpus sizes in both
output formats. Exact synthetic counts, error responses and credential behavior
remain offline-only checks.

This is the first migration step: **other legacy test files still contain live
requests**. An unfiltered `devtools::test()` is not yet an offline-only suite.
Do not replace the remaining tests with skips; migrate their HTTP responses and
retain useful end-to-end assertions as the following step.

The synthetic cassette and regeneration instructions are in
[`fixtures/http-pilot/README.md`](testthat/fixtures/http-pilot/README.md).
Recording new live responses is never an automatic part of these tests.

Pilot validation in the development environment:

- 179 fixture assertions pass, including 83 for connection behavior, 24 for corpus
  statistics, and shared workflow assertions. They also check that
  mock options and vcr configuration are restored after success and failure.
- Seven local-transport assertions pass with external networking unavailable
  in a Linux network namespace.
- The broader selected offline regression run passes 668 assertions, including
  dispersion, chart, interrupt and retry tests.
- A fresh installed-package run passes all 179 assertions with
  `NOT_CRAN=false` and no networking; local-server and live tests are skipped.

These are observed timings, not a performance guarantee or a benchmark of the
entire legacy suite. The live checks were not enabled for these offline runs.

After splitting cassettes per scenario, the expanded 179-assertion fixture
subset took 1.55 seconds on the first run and 0.90 seconds warm in CRAN mode.
This excludes R startup and
dependency installation; CRAN machines may differ. It is the time for this
subset, not the whole package suite or the elapsed time of live tests.
Local-server tests add no CRAN runtime.

An explicit benchmark records one anonymous metadata-only query and two result
pages in a temporary directory, then replays the same workflow three times and
checks that all 51 returned matches are identical. It disables response caching
and rejects actual HTTP transfers during replay; recordings are removed on exit.
It never runs during package checks:

```sh
RKORAP_LIVE_TESTS=true Rscript tools/benchmark-http-replay.R /tmp/korap-http-timings.csv
```

A vcr development measurement took 11.09 seconds live (including recording),
0.95 seconds on the first replay and 0.17–0.20 seconds on subsequent replays.
This illustrates avoided network waiting, not a guaranteed speedup or a
comparison of entire test suites.

## Chart regression checks

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
