
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


#' Plot frequency by year graphs with confidence intervals
#'
#' Convenience function for plotting typical frequency by year graphs with confidence intervals using ggplot2.
#'
#' @examples
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
#' expand_grid(condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"),
#'             year = (2002:2018)) %>%
#'   cbind(frequencyQuery(kco, "[tt/l=Heuschrecke]",
#'                             paste0(.$condition," & pubDate in ", .$year)))  %>%
#'   ipm() %>%
#'   ggplot(aes(year, ipm, fill = condition, color = condition, ymin = conf.low, ymax = conf.high)) +
#'   geom_freq_by_year_ci()
#'
#' @importFrom ggplot2 geom_ribbon geom_line geom_point theme element_text scale_x_continuous
#'
#' @export
geom_freq_by_year_ci <- function() {
  list(
    geom_ribbon(alpha = .3, linetype = 0, show.legend = FALSE),
    geom_line(),
    geom_point(),
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1 + floor(((x[2]-x[1])/30)))))
}

