maintenanceSchedule <- function(now = 1800000000) {
  list(schema_version = 1L, state = "ok", time_zone = "Europe/Berlin",
    last_success = now, valid_until = now + 86400,
    events = list(list(start = "2027-01-15T09:00:00+0100",
      end = "2027-01-15T11:00:00+0100", start_epoch = now + 100,
      end_epoch = now + 200, all_day = 0L, ongoing = 0L)))
}

localMaintenance <- function(now = 1800000000, .env = parent.frame()) {
  clean <- function() {
    rm(list = ls(.maintenanceCache, all.names = TRUE), envir = .maintenanceCache)
    rm(list = ls(.maintenanceSeen, all.names = TRUE), envir = .maintenanceSeen)
  }
  clean()
  withr::defer(clean(), envir = .env)
  local_mocked_bindings(maintenanceNow = function() now,
    .package = "RKorAPClient", .env = .env)
  offlineKorapConnection(KorAPUrl = "https://korap.example.invalid/instance/wiki/")
}

maintenanceResponse <- function(data = maintenanceSchedule(), status = 200L) {
  httr2::response(status, headers = list(`content-type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")))
}

test_that("maintenance transport preserves instance paths and sends no credentials", {
  kco <- localMaintenance()
  kco@accessToken <- "private-token"
  kco@oauthClient <- list(private = "oauth-client")
  seen <- NULL
  info <- httr2::with_mocked_responses(function(req) {
    seen <<- req
    maintenanceResponse()
  }, maintenanceInfo(kco))
  expect_identical(seen$url, "https://korap.example.invalid/instance/wiki/maintenance.json")
  expect_null(seen$headers$Authorization)
  expect_null(seen$policies$auth_sign)
  expect_identical(info$state, "ok")
  expect_equal(nrow(info$events), 1L)
  expect_false(info$events$ongoing)
  expect_equal(seen$options$timeout_ms, 3000)
})

test_that("maintenance caches expire without reusing query caches or hiding failures", {
  kco <- localMaintenance()
  transfers <- 0L
  httr2::with_mocked_responses(function(req) {
    transfers <<- transfers + 1L
    maintenanceResponse()
  }, {
    maintenanceInfo(kco)
    maintenanceInfo(kco)
    expect_equal(transfers, 1L)
    maintenanceInfo(kco, refresh = TRUE)
    expect_equal(transfers, 2L)
    kco@KorAPUrl <- "https://other.example.invalid/"
    maintenanceInfo(kco)
    expect_equal(transfers, 3L)
  })
  local_mocked_bindings(maintenanceNow = function() 1800000101,
    .package = "RKorAPClient")
  expect_true(maintenanceInfo(kco)$events$ongoing)
  local_mocked_bindings(maintenanceNow = function() 1800000301,
    .package = "RKorAPClient")
  info <- httr2::with_mocked_responses(function(req) stop("offline"), maintenanceInfo(kco))
  expect_identical(info$state, "stale")
  expect_equal(nrow(info$events), 0L) # expired window, even in a cached schedule
  local_mocked_bindings(maintenanceNow = function() 1800086401,
    .package = "RKorAPClient")
  info <- httr2::with_mocked_responses(function(req) stop("offline"), maintenanceInfo(kco))
  expect_identical(info$state, "unknown")
})

test_that("missing, malformed and unavailable endpoints do not break queries", {
  kco <- localMaintenance()
  for (status in c(404L, 410L, 500L)) {
    info <- httr2::with_mocked_responses(function(req) maintenanceResponse(status = status),
      maintenanceInfo(kco, refresh = TRUE))
    expect_identical(info$state, if (status == 500L) "unknown" else "unsupported")
  }
  bad <- maintenanceSchedule()
  bad$events[[1]]$start <- "<script>private text</script>"
  info <- httr2::with_mocked_responses(function(req) maintenanceResponse(bad),
    maintenanceInfo(kco, refresh = TRUE))
  expect_identical(info$state, "unknown")
  good <- maintenanceSchedule()
  good$events <- list()
  info <- httr2::with_mocked_responses(function(req) maintenanceResponse(good),
    maintenanceInfo(kco, refresh = TRUE))
  expect_identical(info$state, "ok")
  expect_equal(nrow(info$events), 0L)
})

test_that("a failed or cold refresh preserves known future events until their validity ends", {
  kco <- localMaintenance()
  data <- maintenanceSchedule()
  data$events[[1]]$start_epoch <- 1800003600
  data$events[[1]]$end_epoch <- 1800007200
  httr2::with_mocked_responses(function(req) maintenanceResponse(data), maintenanceInfo(kco))
  local_mocked_bindings(maintenanceNow = function() 1800000301, .package = "RKorAPClient")
  for (response in list(maintenanceResponse(status = 503L),
      maintenanceResponse(list(schema_version = 1L, state = "unknown", time_zone = "UTC", events = list())))) {
    info <- httr2::with_mocked_responses(function(req) response, maintenanceInfo(kco, refresh = TRUE))
    expect_identical(info$state, "stale")
    expect_equal(nrow(info$events), 1L)
  }
  # A valid empty schedule, e.g. after a cancellation, clears the old notice.
  data$events <- list()
  info <- httr2::with_mocked_responses(function(req) maintenanceResponse(data), maintenanceInfo(kco, refresh = TRUE))
  expect_identical(info$state, "ok")
  expect_equal(nrow(info$events), 0L)
})

test_that("all-day notices use exclusive end dates across daylight-saving transitions", {
  kco <- localMaintenance()
  epoch <- function(date) as.numeric(as.POSIXct(date, tz = "Europe/Berlin"))
  local_mocked_bindings(maintenanceNow = function() epoch("2026-03-27"), .package = "RKorAPClient")
  data <- maintenanceSchedule(epoch("2026-03-27"))
  data$events[[1]] <- list(start = "2026-03-28T00:00:00+0100", end = "2026-03-30T00:00:00+0200",
    start_epoch = epoch("2026-03-28"), end_epoch = epoch("2026-03-30"), all_day = 1L)
  withr::local_options(list(rkorap.maintenance.notices = TRUE))
  httr2::with_mocked_responses(function(req) maintenanceResponse(data),
    expect_message(maintenanceNotices(kco), "2026-03-28 – 2026-03-29", fixed = TRUE))
})

test_that("maintenance notices are messages, opt out, and announce each phase only once", {
  kco <- localMaintenance()
  withr::local_options(list(warn = 2, rkorap.maintenance.notices = TRUE))
  notices <- list()
  httr2::with_mocked_responses(function(req) maintenanceResponse(), {
    withCallingHandlers({
      maintenanceNotices(kco)
      maintenanceNotices(kco)
      local_mocked_bindings(maintenanceNow = function() 1800000101,
        .package = "RKorAPClient")
      maintenanceNotices(kco)
      maintenanceNotices(kco)
    }, korap_maintenance_notice = function(m) {
      notices[[length(notices) + 1L]] <<- m
      invokeRestart("muffleMessage")
    })
  })
  expect_length(notices, 2L)
  expect_match(conditionMessage(notices[[1]]), "Planned system maintenance")
  expect_match(conditionMessage(notices[[2]]), "Ongoing system maintenance")
  expect_s3_class(notices[[1]], "message")
  expect_identical(notices[[1]]$instance, kco@KorAPUrl)
  withr::local_options(list(rkorap.maintenance.notices = FALSE))
  local_mocked_bindings(fetchMaintenance = function(...) stop("must not fetch"),
    .package = "RKorAPClient")
  expect_message(maintenanceNotices(kco), NA)
})

test_that("automatic notices precede even a failed welcome request", {
  kco <- localMaintenance()
  withr::local_options(list(rkorap.maintenance.notices = TRUE))
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  paths <- character()
  notices <- list()
  httr2::with_mocked_responses(function(req) {
    paths <<- c(paths, req$url)
    if (endsWith(req$url, "maintenance.json")) maintenanceResponse() else maintenanceResponse(status = 401L)
  }, {
    suppressMessages(withCallingHandlers({
      connection <- KorAPConnection(KorAPUrl = kco@KorAPUrl, accessToken = NULL,
        cache = FALSE, verbose = FALSE)
    }, korap_maintenance_notice = function(m) {
      notices[[length(notices) + 1L]] <<- m
    }))
    expect_null(connection@welcome)
  })
  expect_length(notices, 1L)
  expect_identical(paths, c(paste0(kco@KorAPUrl, "maintenance.json"), kco@apiUrl))
})
