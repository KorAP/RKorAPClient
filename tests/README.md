# Test suites

## Offline tests

```sh
Rscript -e 'devtools::test()'
```

The standard suite runs offline. Install the suggested `vcr` (>= 2.0.0),
`webfakes`, `callr` and `withr` packages. Synthetic vcr cassettes exercise the real
request construction, JSON parsing, pagination, metadata, annotations and
collocation workflows. Exact counts and simulated failures are checked offline.
Cassettes match method and URI and use `record = "none"`; unexpected requests
fail instead of recording or contacting a server. External curl transfers are
also rejected at suite teardown, including errors caught by client code.

Caches use temporary directories. Credential tests use an in-memory keyring
backend; environment variables and mocks are restored afterwards. No corpus
texts or credentials are recorded. Following
[httr2's testing guidance](https://httr2.r-lib.org/reference/with_mocked_responses.html),
vcr uses httr2's mock interface directly, without another HTTP adapter.

`test-http-transport.R` additionally exercises real retries and timeouts using
local `webfakes` servers on automatically allocated ports. Processes are stopped
after each test. These tests need loopback networking, but no internet; they are
skipped on CRAN. CI runs them separately after the package check.

## Live integration tests

```sh
RKORAP_LIVE_TESTS=true RKORAP_LLM_MODELS="" Rscript -e 'devtools::test(filter = "^live-http$")'
```

Five opt-in tests check initialization, query encodings, URL import, fetching,
corpus sizes, text metadata, annotations and collocation analysis against KorAP.
Shared positive assertions run offline and live; exact synthetic values and
simulated errors remain offline-only. The live tests use anonymous connections
without response caching, and a Creative Commons subcorpus for text content.
They are skipped by default and on CRAN, or when the server is unavailable.

Documentation-model tests are also opt-in. `RKORAP_LLM_MODELS=default` selects the
default models; a comma-separated list selects specific models. These tests send
the README to the configured providers. Unset or empty selection disables them.

## Fixtures and timing

Regeneration instructions are in
[`fixtures/http-pilot/README.md`](testthat/fixtures/http-pilot/README.md).
Neither standard tests nor package checks record new responses.

Measured locally with vcr 2.1.0 and R 4.6.2:

- Full developer suite: 1,909 assertions, 21.7 seconds, with external networking
  unavailable and loopback enabled.
- Full CRAN-mode suite: 1,902 assertions, 18.6 seconds, with no network interfaces;
  no unmocked HTTP transfers occurred. Local-server, browser and live tests skip.
- Built-package check: 1,781 assertions, 18.2 seconds; four additional README
  consistency tests skip because the source README is excluded from the archive.
- Five explicit live tests: 47 assertions, 86.4 seconds.

The earlier 2–3 second measurement covered only a 281-assertion subset, not the
full suite. The latest migration enables 57 formerly CRAN-skipped cases with
320 assertions, taking 6.6 seconds within the measured CRAN run. This is added
coverage, not a controlled before/after comparison of full-suite runtime.
Timings exclude dependency installation and depend on the machine and server.

A separate benchmark records an anonymous metadata query and two result pages
in a temporary directory, replays the same workflow three times and compares all
51 matches. It disables caching and rejects HTTP transfers during replay;
recordings are removed on exit. It never runs during package checks:

```sh
RKORAP_LIVE_TESTS=true Rscript tools/benchmark-http-replay.R /tmp/korap-http-timings.csv
```

One development measurement took 11.09 seconds live (including recording),
0.95 seconds on the first replay and 0.17–0.20 seconds on later replays.

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
