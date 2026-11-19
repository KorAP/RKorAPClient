## RKorAPClient 1.5.1

This maintenance update is submitted shortly after 1.5.0 primarily to keep
RKorAPClient compatible with the upcoming highcharter release. highcharter's
new theme API replaces hc_theme_google(...) with hc_theme("google", ...).
RKorAPClient now detects the available API and supports both the current CRAN
version and the development version of highcharter. No dependency minimum is
raised and no new package dependencies are introduced.

The update also clarifies the separate highcharter and Highcharts licenses,
fixes handling of expired access tokens and cache persistence, and displays
maintenance notices from KorAP servers that provide a public schedule.

Tests run offline by default. Live-server, documentation-model and browser
checks require explicit opt-in and are not run during CRAN checks.

## Test environments and results

* Local Fedora 45, R 4.6.2 (2026-10-29), 2026-11-19:
  `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes after replacing the
  outdated workshop reference with the LREC paper and CLARIN chapter in the
  package documentation.
* Full source test suite with CRAN highcharter 0.9.5: 1,895 assertions passed,
  no failures or warnings. Six opt-in browser/live tests skipped.
* Development highcharter 0.12.6.1 (commit
  e97f907268ba6c9912d409b36cbe4645049b1cc0): `R CMD check --as-cran` passed
  with 0 errors, 0 warnings and 0 notes. The offline suite, headless Chrome
  chart tests and separate README consistency checks passed.
* GitHub Actions passed on `d4b2935`, the commit immediately before the
  version bump. R-CMD-check passed on Windows and macOS with R-release and
  on Ubuntu with R-release, R-devel and R-oldrel-1. The source-tests,
  test-coverage and document workflows also passed. The package code and
  help pages are identical in 1.5.1.
  (<https://github.com/KorAP/RKorAPClient/actions/runs/44495558138>)
* Windows Server 2022 (win-builder): R-release (4.6.2 ucrt) reported
  `Status: OK` for the archive with the first literature URL fix. R-devel
  (2026-11-17 r90926 ucrt) reported `Status: OK` for the initial archive.
  Both Windows checks predate the final bibliography edits.
* macOS R-release (macOS builder): upload attempted on 2026-11-19, but the
  service returned HTTP 502 Bad Gateway; no result is available yet.
