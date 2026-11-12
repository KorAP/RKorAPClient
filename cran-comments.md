## Notes on 1.5.0

* feature release: metadata-based dispersion analysis and frequency-chart helpers
* no new required package dependencies; additional suggested packages support
  offline, local-server and browser tests
* tests run offline by default; real-server and documentation-model tests
  require explicit opt-in, and browser tests are not run on CRAN

## Test environments for 1.5.0

* local Fedora 45 with R 4.6.2 (2026-10-29)
* local `R CMD check --as-cran`: no errors, warnings or notes, including
  online incoming checks and PDF/HTML manuals, with all suggested packages installed
* source tests, including local HTTP and headless chart checks: passed
* five live KorAP integration scenarios: passed
* win-builder: R 4.6.2 (2026-10-29 ucrt) and R-devel
  (2026-11-11 r90876 ucrt), Windows Server 2022: Status OK for both
* GitHub Actions (`--as-cran --no-manual`): macOS Tahoe 26.7.2 (ARM64)
  with R 4.6.2, Windows R-release,
  and Ubuntu R-release, R-devel and R-oldrel-1: all passed
  (<https://github.com/KorAP/RKorAPClient/actions/runs/43198024806>)
* the CI run checked commit 5b699bb, immediately before the version bump;
  package code, examples and manual pages are unchanged in 1.5.0
* the additional macOS-builder result is still pending; macOS validation
  is covered by the successful GitHub Actions check
