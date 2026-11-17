# Maintenance schedules are short-lived, public instance information. Keep
# them separate from query caches and credentials, and never persist them.
.maintenanceCache <- new.env(parent = emptyenv())
.maintenanceSeen <- new.env(parent = emptyenv())
maintenanceNow <- function() as.numeric(Sys.time())

emptyMaintenanceEvents <- function() {
  data.frame(start = character(), end = character(), start_epoch = numeric(),
    end_epoch = numeric(), all_day = logical(), ongoing = logical())
}

unknownMaintenance <- function(state = "unknown") {
  list(state = state, time_zone = "UTC", last_success = NA_real_,
    valid_until = NA_real_, events = emptyMaintenanceEvents())
}

parseMaintenance <- function(data) {
  scalarNumber <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  scalarText <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  stopifnot(is.list(data), identical(data$schema_version, 1L),
    scalarText(data$state), data$state %in% c("ok", "stale", "unknown"),
    scalarText(data$time_zone), data$time_zone %in% c("UTC", OlsonNames()),
    is.list(data$events), length(data$events) <= 1000L)
  result <- unknownMaintenance(data$state)
  result$time_zone <- data$time_zone
  if (data$state == "unknown") return(result)
  stopifnot(scalarNumber(data$last_success), scalarNumber(data$valid_until),
    data$valid_until >= data$last_success)
  result$last_success <- data$last_success
  result$valid_until <- data$valid_until
  rows <- lapply(data$events, function(event) {
    stopifnot(is.list(event), scalarText(event$start), scalarText(event$end),
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]+(Z|[+-][0-9:]+)$", event$start),
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]+(Z|[+-][0-9:]+)$", event$end),
      scalarNumber(event$start_epoch), scalarNumber(event$end_epoch),
      event$end_epoch >= event$start_epoch,
      is.numeric(event$all_day) || is.logical(event$all_day),
      length(event$all_day) == 1L, !is.na(event$all_day), event$all_day %in% c(0, 1))
    data.frame(start = event$start, end = event$end,
      start_epoch = event$start_epoch, end_epoch = event$end_epoch,
      all_day = as.logical(event$all_day), ongoing = FALSE)
  })
  if (length(rows)) result$events <- do.call(rbind, rows)
  result
}

fetchMaintenance <- function(kco) {
  req <- httr2::request(kco@KorAPUrl) |>
    httr2::req_url_path_append("maintenance.json") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent(kco@userAgent) |>
    httr2::req_timeout(3) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- performRequest(req)
  if (httr2::resp_status(resp) %in% c(404L, 410L)) return(unknownMaintenance("unsupported"))
  stopifnot(httr2::resp_status(resp) == 200L)
  parseMaintenance(httr2::resp_body_json(resp, simplifyVector = FALSE))
}

#' Inspect planned maintenance of a KorAP instance
#'
#' Retrieve the public schedule from the selected instance's Kalamar frontend.
#' This optional endpoint requires no authorization. Instances without the
#' endpoint remain usable. Calendar failures never prevent corpus queries.
#'
#' @param kco A [KorAPConnection()] object.
#' @param refresh Bypass the five-minute in-session schedule cache.
#' @return A list with `state` (`"ok"`, `"stale"`, `"unknown"` or
#'   `"unsupported"`), `time_zone`, numeric Unix timestamps `last_success` and
#'   `valid_until`, and an `events` data frame. Events contain ISO 8601 `start`
#'   and `end`, Unix `start_epoch` and `end_epoch`, and logical `all_day` and
#'   `ongoing` columns. All-day end dates are exclusive. An empty schedule with
#'   state `"unknown"` does not establish that no maintenance is planned.
#' @details
#' Before server requests, the client checks for notices at most once every
#' five minutes per instance. Each maintenance window is announced once per
#' R session, and again when it starts. Notices are messages of class
#' `korap_maintenance_notice`, including when `verbose = FALSE`; they do not
#' abort scripts using `options(warn = 2)`. Set
#' `options(rkorap.maintenance.notices = FALSE)` to disable automatic checks and
#' messages. Explicit `maintenanceInfo()` calls still work.
#'
#' No access token or private calendar URL is sent to the schedule endpoint.
#' Cached schedules are kept in memory only. On refresh failure, a previous
#' schedule can be used as stale until the server's `valid_until` timestamp.
#' @export
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' maintenanceInfo(kco)$events
#' }
maintenanceInfo <- function(kco, refresh = FALSE) {
  stopifnot(methods::is(kco, "KorAPConnection"), is.logical(refresh),
    length(refresh) == 1L, !is.na(refresh))
  key <- kco@KorAPUrl
  now <- maintenanceNow()
  previous <- .maintenanceCache[[key]]
  if (refresh || is.null(previous) || now - previous$checked_at >= 300) {
    info <- tryCatch(fetchMaintenance(kco), error = function(e) NULL)
    if ((is.null(info) || info$state == "unknown") && !is.null(previous) &&
        is.finite(previous$info$valid_until) && now <= previous$info$valid_until) {
      info <- previous$info
      info$state <- "stale"
    }
    if (is.null(info)) info <- unknownMaintenance()
    previous <- list(checked_at = now, info = info)
    .maintenanceCache[[key]] <- previous
  }
  info <- previous$info
  if (is.finite(info$valid_until) && now > info$valid_until) info <- unknownMaintenance()
  info$events <- info$events[info$events$end_epoch > now, , drop = FALSE]
  info$events$ongoing <- info$events$start_epoch <= now
  info
}

maintenanceNotices <- function(kco) {
  if (!isTRUE(getOption("rkorap.maintenance.notices", TRUE))) return(invisible(NULL))
  info <- maintenanceInfo(kco)
  for (i in seq_len(nrow(info$events))) {
    event <- info$events[i, , drop = FALSE]
    key <- paste(kco@KorAPUrl, event$start_epoch, event$end_epoch,
      event$all_day, event$ongoing, sep = "|")
    if (!is.null(.maintenanceSeen[[key]])) next
    .maintenanceSeen[[key]] <- TRUE
    date <- function(epoch, all_day) format(as.POSIXct(epoch, origin = "1970-01-01", tz = "UTC"),
      if (all_day) "%Y-%m-%d" else "%Y-%m-%d %H:%M %Z", tz = info$time_zone)
    # One second before the exclusive all-day end gives its last calendar date,
    # including across daylight-saving transitions.
    from <- date(event$start_epoch, event$all_day)
    to <- date(event$end_epoch - as.integer(event$all_day), event$all_day)
    text <- paste0("KorAP: ", if (event$ongoing) "Ongoing" else "Planned",
      " system maintenance at ", kco@KorAPUrl, ": ", from, " \u2013 ", to,
      " (", info$time_zone, "). The service may be temporarily unavailable.",
      if (info$state == "stale") " Based on a cached maintenance schedule." else "", "\n")
    message(structure(list(message = text, call = NULL, event = event,
      instance = kco@KorAPUrl, state = info$state),
      class = c("korap_maintenance_notice", "message", "condition")))
  }
  invisible(info)
}
