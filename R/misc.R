
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


## Mute notes: "Undefined global functions or variables:"
globalVariables(c("conf.high", "conf.low", "onRender", "webUIRequestUrl"))


#' Experimental: Plot frequency by year graphs with confidence intervals
#'
#' Experimental convenience function for plotting typical frequency by year graphs with confidence intervals using ggplot2.
#' \bold{Warning:} This function may be moved to a new package.
#'
#' @param mapping	Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ...	 Other arguments passed to geom_ribbon, geom_line, and geom_click_point.
#'
#' @examples
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
#' expand_grid(condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"),
#'             year = (2002:2018)) %>%
#'   cbind(frequencyQuery(kco, "[tt/l=Heuschrecke]",
#'                             paste0(.$condition," & pubDate in ", .$year)))  %>%
#'   ipm() %>%
#'   ggplot(aes(year, ipm, fill = condition, color = condition)) +
#'   geom_freq_by_year_ci()
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme element_text scale_x_continuous
#'
#' @export
geom_freq_by_year_ci <- function(mapping = aes(ymin=conf.low, ymax=conf.high), ...) {
  list(
    geom_ribbon(mapping,
                alpha = .3, linetype = 0, show.legend = FALSE, ...),
    geom_line(...),
    geom_click_point(aes(url=webUIRequestUrl), ...),
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(0.8, 0.2)),
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1 + floor(((x[2]-x[1])/30)))))
}

#' @importFrom ggplot2 ggproto aes GeomPoint
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

#' @importFrom ggplot2 layer
geom_click_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, url = NA, ...) {
  layer(
    geom = GeomClickPoint, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @importFrom htmlwidgets onRender
tooltip2hyperlink <- function(p, attribute="webUIRequestUrl") {
  pattern <- paste0(attribute, ": ([^<]+)")
  for(i in grep(attribute, p$x$data)) {
    x <- p[["x"]][["data"]][[i]][["text"]]
    m <- regexpr(pattern, x)
    matches <- sub(paste0(attribute, ": "), "", regmatches(x, m))
    p$x$data[[i]]$customdata <- matches
    p[["x"]][["data"]][[i]][["text"]] <- sub(paste0(attribute, ":[^<]*<br ?/?>"), "", p[["x"]][["data"]][[i]][["text"]] )
  }
  onRender(p, "function(el, x) { el.on('plotly_click', function(d) { var url=d.points[0].customdata; if(url) { window.open(url, 'korap') } })}")
}

#' Experimental: Convert ggplot2 to plotly with hyperlinks to KorAP queries
#'
#' \code{RKorAPClient::ggplotly} converts a \code{ggplot2::ggplot()} object to a plotly
#' object with hyperlinks from data points to corresponding KorAP queries.
#' \bold{Warning:} This function may be moved to a new package.
#'
#' @param p a ggplot object.
#' @param tooltip a character vector specifying which aesthetic mappings to show
#'   in the tooltip. If you want hyperlinks to KorAP queries you need to include
#'   \code{"url"} here.
#' @param ... Other arguments passed to \code{plotly::ggplotly}
#'
#' @examples
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
#' g <- expand_grid(condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"),
#'             year = (2002:2018)) %>%
#'   cbind(frequencyQuery(kco, "[tt/l=Heuschrecke]",
#'                        paste0(.$condition," & pubDate in ", .$year)))  %>%
#'   ipm() %>%
#'   ggplot(aes(year, ipm, fill = condition, color = condition)) +
#' ##  theme_light(base_size = 20) +
#'   geom_freq_by_year_ci()
#' p <- ggplotly(g)
#' print(p)
#' ## saveWidget(p, paste0(tmpdir(), "heuschrecke.html")
#'
#'
#' @importFrom plotly ggplotly
#' @importFrom htmlwidgets saveWidget
#' @export
ggplotly <- function(p = ggplot2::last_plot(), tooltip = c("x", "y", "colour", "url"), ...) {
  pp <- plotly::ggplotly(p = p, tooltip = tooltip, ...)
  tooltip2hyperlink(pp)
}