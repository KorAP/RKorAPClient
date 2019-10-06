
#' Convert corpus frequency table to instances per million.
#'
#' Convenience function for converting frequency tables to instances per
#' million.
#'
#' Given a table with columns \code{f}, \code{conf.low}, and \code{conf.high}, \code{ipm} ads a \code{column ipm}
#' und multiplies conf.low and \code{conf.high} with 10^6.
#'
#' @param df table returned from \code{\link{frequencyQuery}}
#'
#' @return original table with additional column \code{ipm} and converted columns \code{conf.low} and \code{conf.high}
#' @export
#'
#' @importFrom dplyr .data
#'
#' @examples
#' new("KorAPConnection") %>% frequencyQuery("Test", paste0("pubDate in ", 2000:2002)) %>% ipm()
ipm <- function(df) {
  df %>%
    mutate(ipm = .data$f * 10^6, conf.low = .data$conf.low * 10^6, conf.high = .data$conf.high * 10^6)
}


