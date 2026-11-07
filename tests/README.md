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

The suite sets a temporary `R.cache.rootPath`, removes it at teardown and
restores the previous option. This also isolates caches whose subdirectory
looks like an absolute path. Credential tests use an in-memory keyring backend;
environment variables and mocks are restored afterwards. No corpus
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
Shared positive assertions run offline and live, including a query that omits
`fields` and verifies current topic-domain metadata. Exact synthetic values and
simulated errors remain offline-only. The live tests use anonymous connections
without response caching, and a Creative Commons subcorpus for text content.
They are skipped by default and on CRAN, or when the server is unavailable.

The GitLab `live-korap-http-tests` job is prepared for a dedicated scheduled
pipeline with `RKORAP_LIVE_TESTS=true`, for example weekly. It permits failures
without failing the pipeline and disables model tests. The ordinary check and
model jobs are excluded from that dedicated pipeline. No schedule is created by
the YAML file; its interval must be configured in GitLab after merging.

Documentation-model tests are also opt-in. `RKORAP_LLM_MODELS=default` selects the
default models; a comma-separated list selects specific models. These tests send
the README to the configured providers. Unset or empty selection disables them.

## Fixtures and timing

Regeneration instructions are in
[`fixtures/http-pilot/README.md`](testthat/fixtures/http-pilot/README.md).
Neither standard tests nor package checks record new responses.

Measured locally with vcr 2.1.0 and R 4.6.2:

- Full CRAN-mode source suite: 1,792 assertions, 18.8 seconds, with no network
  interfaces. Temporary cache roots were removed, the original option restored
  and the home cache unchanged. Local-server, browser and live tests skip.
- Built-package `R CMD check --as-cran`: 1,671 assertions, about 35 seconds in
  the test phase. Four source-README consistency tests additionally skip because
  the README is excluded from the archive. The check has no errors or warnings;
  one NOTE concerns the development version. The locally missing optional `sf`
  package was not required; online incoming checks were disabled.
- Six additional local-transport assertions run with loopback networking only.
- The five live scenarios remain opt-in. The added default-metadata contract
  also passed against the public KorAP server.

The earlier 2–3 second measurement covered only a subset, not the full suite.
Timings exclude dependency installation and depend on the machine and server.
Request guards now signal missing fixtures as errors rather than counting each
successful guard as an assertion; assertion totals therefore differ from the
previous migration report.

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
