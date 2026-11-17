# Existing API fixtures cover corpus traffic only. Maintenance tests explicitly
# opt in and mock the independent public schedule endpoint.
withr::local_options(list(rkorap.maintenance.notices = FALSE),
  .local_envir = testthat::teardown_env())
