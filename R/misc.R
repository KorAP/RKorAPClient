
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
geom_freq_by_year_ci <- function(mapping = NULL) {
  list(
    geom_ribbon(alpha = .3, linetype = 0, show.legend = FALSE),
    geom_line(),
    geom_click_point(mapping),
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1 + floor(((x[2]-x[1])/30)))))
}

GeomClickPoint <- ggproto(
  "GeomPoint",
  GeomPoint,
  required_aes = c("x", "y"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5, url = NA
  ),
  extra_params = c("na.rm", "url"),
  draw_panel = function(data, panel_params,
                        coord, na.rm = FALSE, showpoints = TRUE, url = NULL) {
    GeomPoint$draw_panel(data, panel_params, coord, na.rm = na.rm)
  }
)

geom_click_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, url = NA, ...) {
  layer(
    geom = GeomClickPoint, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

addKorAPHyperlinks <- function(p) {
  pattern <- "webUIRequestUrl: ([^<]+)"
  for(i in grep("webUIRequestUrl", p$x$data)) {
    x <- p[["x"]][["data"]][[i]][["text"]]
    m <- regexpr(pattern, x)
    matches <- sub("webUIRequestUrl: ", "", regmatches(x, m))
    p$x$data[[i]]$customdata <- matches
    p[["x"]][["data"]][[i]][["text"]] <- sub("webUIRequestUrl:[^<]*<br ?/?>", "", p[["x"]][["data"]][[i]][["text"]] )
  }
  onRender(p, "function(el, x) { el.on('plotly_click', function(d) { var url=d.points[0].customdata; if(url) { window.open(url, 'korap') } })}")
}
