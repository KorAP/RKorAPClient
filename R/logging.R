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
  green <- "\033[32m"
  reset <- "\033[0m"
  cat(ifelse(v, paste0(green, ..., reset), ""))
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
