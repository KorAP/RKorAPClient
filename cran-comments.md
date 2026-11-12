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
* win-builder and current macOS/Windows checks are pending
