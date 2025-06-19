#' Logging utilities for RKorAPClient
#'
#' This module provides centralized logging functions used throughout the package
#' for progress reporting and ETA calculations.

#' Log informational messages with optional coloring
#' @importFrom stats median
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

#' Calculate sophisticated ETA using median of recent non-cached times
#'
#' Advanced ETA calculation that excludes cached responses and uses median
#' of recent timing data for more stable estimates. This is particularly
#' useful for operations where some responses may be cached and much faster.
#'
#' @param individual_times numeric vector of individual item processing times
#' @param current_item current item number (1-based)
#' @param total_items total number of items to process
#' @param cache_threshold minimum time in seconds to consider as non-cached (default: 0.1)
#' @param window_size number of recent non-cached times to use for median calculation (default: 5)
#' @return list with eta_seconds, estimated_completion_time, and is_cached flag
#' @keywords internal
calculate_sophisticated_eta <- function(individual_times, current_item, total_items, 
                                       cache_threshold = 0.1, window_size = 5) {
  if (current_item < 2) {
    return(list(eta_seconds = NA, estimated_completion_time = NA, is_cached = FALSE))
  }
  
  # Get times up to current item
  current_times <- individual_times[1:current_item]
  current_time <- individual_times[current_item]
  is_cached <- current_time < cache_threshold
  
  # Use recent non-cached times for better ETA estimates
  # Exclude very fast responses as likely cached
  non_cached_times <- current_times[current_times >= cache_threshold]
  
  if (length(non_cached_times) >= 1) {
    # Use median of recent non-cached times for more stable estimates
    recent_window <- min(window_size, length(non_cached_times))
    recent_times <- tail(non_cached_times, recent_window)
    time_per_item <- median(recent_times)
    
    remaining_items <- total_items - current_item
    eta_seconds <- time_per_item * remaining_items
    estimated_completion_time <- Sys.time() + eta_seconds
    
    return(list(
      eta_seconds = eta_seconds,
      estimated_completion_time = estimated_completion_time,
      is_cached = is_cached
    ))
  } else {
    # All responses so far appear cached
    return(list(eta_seconds = NA, estimated_completion_time = NA, is_cached = is_cached))
  }
}

#' Format ETA information for display
#'
#' Helper function to format ETA information consistently across different methods.
#'
#' @param eta_seconds numeric ETA in seconds (can be NA)
#' @param estimated_completion_time POSIXct estimated completion time (can be NA)
#' @return character string with formatted ETA or empty string if NA
#' @keywords internal
format_eta_display <- function(eta_seconds, estimated_completion_time) {
  if (is.na(eta_seconds) || is.na(estimated_completion_time)) {
    return("")
  }
  
  completion_time_str <- format(estimated_completion_time, "%Y-%m-%d %H:%M:%S")
  paste0(", ETA: ", format_duration(eta_seconds), " (", completion_time_str, ")")
}

#' Get cache indicator string
#'
#' Helper function to generate cache indicator for logging.
#'
#' @param is_cached logical indicating if the item was cached
#' @param cache_threshold minimum time threshold for non-cached items
#' @return character string with cache indicator or empty string
#' @keywords internal
get_cache_indicator <- function(is_cached, cache_threshold = 0.1) {
  if (is_cached) " [cached]" else ""
}
