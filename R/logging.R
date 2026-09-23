#' Logging utilities for RKorAPClient
#'
#' This module provides centralized logging functions used throughout the package
#' for progress reporting and ETA calculations.

#' Log informational messages with optional coloring
#'
#' @param v logical flag indicating whether to output the message
#' @param ... message components to concatenate and display
#' @keywords internal
log_info <- function(v, ...) {
  if (!v) {
    return(invisible())
  }
  text <- paste0(...)
  # colour the text, but not the line break, lest the colour bleed into what follows
  newline <- grepl("\n$", text)
  cat(ansi(sub("\n$", "", text), "green"), if (newline) "\n", sep = "")
}

#' Format duration in seconds to human-readable format
#'
#' Converts a duration in seconds to a formatted string with days, hours, minutes, and seconds.
#' Used for ETA calculations and progress reporting.
#'
#' @param seconds numeric duration in seconds
#' @return character string with formatted duration
#' @keywords internal
#' @examples
#' \dontrun{
#' format_duration(3661) # "01h 01m 01s"
#' format_duration(86461) # "1d 00h 01m 01s"
#' }
format_duration <- function(seconds) {
  if (is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("00s")
  }

  days <- floor(seconds / (24 * 3600))
  seconds <- seconds %% (24 * 3600)
  hours <- floor(seconds / 3600)
  seconds <- seconds %% 3600
  minutes <- floor(seconds / 60)
  seconds <- floor(seconds %% 60)

  paste0(
    if (days > 0) paste0(days, "d ") else "",
    if (hours > 0 || days > 0) paste0(sprintf("%02d", hours), "h ") else "",
    if (minutes > 0 || hours > 0 || days > 0) paste0(sprintf("%02d", minutes), "m ") else "",
    paste0(sprintf("%02d", seconds), "s")
  )
}

#' Calculate and format ETA for batch operations
#'
#' Helper function to calculate estimated time of arrival based on elapsed time
#' and progress through a batch operation.
#'
#' @param current_item current item number (1-based)
#' @param total_items total number of items to process
#' @param start_time POSIXct start time of the operation
#' @return character string with formatted ETA and completion time or empty string if not calculable
#' @keywords internal
calculate_eta <- function(current_item, total_items, start_time) {
  if (current_item <= 1 || total_items <= 1) {
    return("")
  }

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (elapsed_time <= 0) {
    return("")
  }

  avg_time_per_item <- elapsed_time / (current_item - 1)
  remaining_items <- total_items - current_item + 1
  eta_seconds <- avg_time_per_item * remaining_items
  estimated_completion_time <- Sys.time() + eta_seconds
  completion_time_str <- format(estimated_completion_time, "%Y-%m-%d %H:%M:%S")

  paste0(". ETA: ", format_duration(eta_seconds), " (", completion_time_str, ")")
}

#' Whether verbose output may carry ANSI colours
#'
#' Colours only help on a terminal or in the RStudio console; written to a file
#' or a knitr document the escape sequences are just noise. The `NO_COLOR`
#' convention (<https://no-color.org>) switches them off everywhere.
#'
#' @return logical
#' @noRd
use_colour <- function() {
  if (nzchar(Sys.getenv("NO_COLOR"))) {
    return(FALSE)
  }
  isatty(stdout()) || (Sys.getenv("RSTUDIO") == "1" && interactive())
}

#' Wrap text in an ANSI style if colours are in use
#'
#' @param text character
#' @param style one of the styles below
#' @return character
#' @noRd
ansi <- function(text, style = c("green", "red", "yellow", "dim", "bold")) {
  if (!use_colour()) {
    return(text)
  }
  code <- switch(match.arg(style),
    green = "32", red = "31", yellow = "33", dim = "2", bold = "1"
  )
  paste0("\033[", code, "m", text, "\033[0m")
}

#' A symbol, with an ASCII fallback for non-UTF-8 consoles
#' @noRd
log_symbol <- function(name = c("tick", "cross", "warning")) {
  utf8 <- isTRUE(l10n_info()$`UTF-8`)
  switch(match.arg(name),
    tick = if (utf8) "\u2714" else "v",
    cross = if (utf8) "\u2716" else "x",
    warning = "!"
  )
}

#' Shorten strings to a display width, cutting out the middle
#'
#' Virtual corpus definitions compared with each other tend to share their
#' beginning and differ at the end (`... & pubDate in 2010`), so both ends are
#' kept.
#' @noRd
truncate_display <- function(x, width) {
  ellipsis <- if (isTRUE(l10n_info()$`UTF-8`)) "\u2026" else "..."
  keep <- width - nchar(ellipsis, type = "width")
  too_long <- nchar(x, type = "width") > width
  head_length <- ceiling(keep / 2)
  tail_length <- keep - head_length
  x[too_long] <- paste0(
    substr(x[too_long], 1, head_length),
    ellipsis,
    substr(x[too_long], nchar(x[too_long]) - tail_length + 1, nchar(x[too_long]))
  )
  x
}

#' Factor out the beginning that all labels share
#'
#' A series of virtual corpora like `textType = /Zeit.*/ & pubDate in 2010`,
#' `... 2011`, ... is shown more clearly by naming the shared part once and
#' listing only what differs. The prefix is cut back to a word boundary, so
#' that no word is split.
#'
#' @param x character vector
#' @param min_width only labels wider than this are worth shortening, and only
#'   if what remains of them fits within it; otherwise they read better in full
#' @return list with the `prefix` ("" if not worth factoring out) and the
#'   remaining `labels`, marked with an ellipsis where something was removed
#' @noRd
factor_common_prefix <- function(x, min_width = 24) {
  none <- list(prefix = "", labels = x)
  if (length(unique(x)) < 2 || max(nchar(x, type = "width")) <= min_width) {
    return(none)
  }
  chars <- strsplit(x, "")
  shortest <- min(lengths(chars))
  n <- 0
  while (n < shortest && length(unique(vapply(chars, `[`, "", n + 1))) == 1) {
    n <- n + 1
  }
  prefix <- sub("\\S*$", "", substr(x[1], 1, n))
  if (nchar(prefix) < 8 || max(nchar(x, type = "width")) - nchar(prefix) > min_width) {
    return(none)
  }
  ellipsis <- if (isTRUE(l10n_info()$`UTF-8`)) "\u2026" else "..."
  list(
    prefix = paste0(trimws(prefix, "right"), " ", ellipsis),
    labels = paste0(ellipsis, substring(x, nchar(prefix) + 1))
  )
}

#' Pad strings to a common display width (unlike formatC, counts wide characters correctly)
#' @noRd
pad_display <- function(x, width, right = FALSE) {
  padding <- strrep(" ", pmax(0, width - nchar(x, type = "width")))
  if (right) paste0(padding, x) else paste0(x, padding)
}

#' Format a duration compactly, without leading zeros: "4.8s", "18s", "3m 05s", "1h 02m"
#'
#' @param seconds numeric
#' @param precise show tenths of seconds below one minute
#' @noRd
format_duration_short <- function(seconds, precise = FALSE) {
  if (is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("?")
  }
  if (seconds < 60) {
    return(if (precise) sprintf("%.1fs", seconds) else sprintf("%.0fs", seconds))
  }
  seconds <- round(seconds)
  if (seconds < 3600) {
    return(sprintf("%dm %02ds", seconds %/% 60, seconds %% 60))
  }
  if (seconds < 86400) {
    return(sprintf("%dh %02dm", seconds %/% 3600, (seconds %% 3600) %/% 60))
  }
  sprintf("%dd %02dh", seconds %/% 86400, (seconds %% 86400) %/% 3600)
}

#' Estimated remaining time after `done` of `total` items have finished
#'
#' Items answered from the cache say nothing about how long the remaining ones
#' will take, so only the others are averaged.
#'
#' @param durations numeric durations of the finished items
#' @param cached logical, whether each finished item came from the cache
#' @param total total number of items
#' @return a string like "~18s left", with the clock time of completion added for
#'   waits beyond ten minutes, or "" where there is nothing to estimate
#' @noRd
format_remaining <- function(durations, cached, total) {
  remaining <- total - length(durations)
  measured <- durations[!cached]
  if (remaining < 1 || length(measured) == 0) {
    return("")
  }
  eta <- mean(measured) * remaining
  if (eta < 1) {
    return("")
  }
  paste0(
    "~", format_duration_short(eta), " left",
    if (eta > 600) paste0(" (until ", format(Sys.time() + eta, "%H:%M"), ")") else ""
  )
}

#' Column layout for a table of progress rows, one row per item
#'
#' @param total number of rows
#' @param ... character vectors of all row labels, one per column, known up front
#'   so that the columns can be aligned
#' @return a list with the counter and column widths
#' @noRd
progress_layout <- function(total, ...) {
  widths <- vapply(list(...), function(column) max(nchar(column, type = "width"), 1), numeric(1))
  # counter, gaps, and result, time and remark need about 50 characters
  budget <- max(20, getOption("width", 80) - 50 - 2 * nchar(total))
  # shrink the widest column until the row fits, but not below 12 characters
  while (sum(widths) > budget && max(widths) > 12) {
    widest <- which.max(widths)
    widths[widest] <- max(12, widths[widest] - (sum(widths) - budget))
  }
  list(total = total, widths = widths)
}

#' Start a progress row: counter and labels, printed before the item runs, so
#' that it is visible what is being waited for
#' @noRd
progress_row_start <- function(verbose, layout, i, ...) {
  if (!verbose) {
    return(invisible())
  }
  labels <- mapply(function(label, width) {
    pad_display(truncate_display(label, width), width)
  }, list(...), layout$widths)
  counter <- sprintf("[%*d/%d]", nchar(layout$total), i, layout$total)
  cat("  ", ansi(counter, "dim"), "  ", paste(labels, collapse = "  "), sep = "")
}

#' Finish a progress row with the result, the time taken and a remark
#'
#' @param result what came out, e.g. "31,883 hits", already formatted
#' @param status "ok", "cached", "incomplete" or "failed"
#' @param remark further text, e.g. the remaining time
#' @param width display width the result is right aligned to, enough for the
#'   largest count it can plausibly show
#' @noRd
progress_row_end <- function(verbose, result, seconds, status = "ok", remark = "", width = 18) {
  if (!verbose) {
    return(invisible())
  }
  result <- pad_display(result, width, right = TRUE)
  cat(
    "  ", switch(status, failed = ansi(result, "red"), result),
    "  ", ansi(pad_display(format_duration_short(seconds, precise = TRUE), 6, right = TRUE), "dim"),
    switch(status,
      cached = paste0("  ", ansi("cached", "dim")),
      incomplete = paste0("  ", ansi("incomplete", "yellow")),
      ""
    ),
    if (nzchar(remark)) paste0("  ", ansi(remark, "dim")) else "",
    "\n",
    sep = ""
  )
}

#' Summary line after a batch of progress rows
#'
#' @param what plural noun for the items, e.g. "queries"
#' @param status character vector with the status of each item
#' @noRd
progress_summary <- function(verbose, what, status, seconds) {
  if (!verbose) {
    return(invisible())
  }
  counts <- c(
    cached = sum(status == "cached"),
    incomplete = sum(status == "incomplete"),
    failed = sum(status == "failed")
  )
  counts <- counts[counts > 0]
  symbol <- if ("failed" %in% names(counts)) {
    ansi(log_symbol("cross"), "red")
  } else if ("incomplete" %in% names(counts)) {
    ansi(log_symbol("warning"), "yellow")
  } else {
    ansi(log_symbol("tick"), "green")
  }
  cat(
    symbol, " ", length(status), " ", what, " in ", format_duration_short(seconds),
    if (length(counts) > 0) paste0(" (", paste(counts, names(counts), collapse = ", "), ")") else "",
    "\n",
    sep = ""
  )
}

#' Format a count with thousands separators
#' @noRd
format_count <- function(n) {
  # "d" would coerce to integer, and a corpus can hold more than 2^31 tokens
  formatC(n, format = "f", digits = 0, big.mark = ",")
}
