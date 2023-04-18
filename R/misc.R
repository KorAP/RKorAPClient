#' Misc functions
#'
#' @name misc-functions
NULL
#' NULL

#' Convert corpus frequency table to instances per million.
#'
#' Convenience function for converting frequency tables to instances per
#' million.
#'
#' Given a table with columns `f`, `conf.low`, and `conf.high`, `ipm` ads a `column ipm`
#' und multiplies conf.low and `conf.high` with 10^6.
#'
#' @param df table returned from [frequencyQuery()]
#'
#' @return original table with additional column `ipm` and converted columns `conf.low` and `conf.high`
#' @export
#'
#' @rdname misc-functions
#' @importFrom dplyr .data
#'
#' @examples
#' \dontrun{
#'
#' new("KorAPConnection") %>% frequencyQuery("Test", paste0("pubDate in ", 2000:2002)) %>% ipm()
#' }
ipm <- function(df) {
  df %>%
    mutate(ipm = .data$f * 10^6, conf.low = .data$conf.low * 10^6, conf.high = .data$conf.high * 10^6)
}

#' Convert corpus frequency table of alternatives to percent
#'
#' Convenience function for converting frequency tables of alternative variants
#' (generated with `as.alternatives=TRUE`) to percent.
#'
#' @param df table returned from [frequencyQuery()]
#'
#' @return original table with converted columns `f`, `conf.low` and `conf.high`
#' @export
#'
#' @importFrom dplyr .data
#'
#' @rdname misc-functions
#' @examples
#' \dontrun{
#'
#' new("KorAPConnection") %>%
#'     frequencyQuery(c("Tollpatsch", "Tolpatsch"),
#'     vc=paste0("pubDate in ", 2000:2002),
#'     as.alternatives = TRUE) %>%
#'   percent()
#' }
percent <- function(df) {
  df %>%
    mutate(f = .data$f * 10^2, conf.low = .data$conf.low * 10^2, conf.high = .data$conf.high * 10^2)
}

#' Convert query or vc strings to plot labels
#'
#' Converts a vector of query or vc strings to typically appropriate legend labels
#' by clipping off prefixes and suffixes that are common to all query strings.
#'
#' @param data string or vector of query or vc definition strings
#' @param pubDateOnly discard all but the publication date
#' @param excludePubDate discard publication date constraints
#' @return string or vector of strings with clipped off common prefixes and suffixes
#'
#' @rdname misc-functions
#'
#' @examples
#' queryStringToLabel(paste("textType = /Zeit.*/ & pubDate in", c(2010:2019)))
#' queryStringToLabel(c("[marmot/m=mood:subj]", "[marmot/m=mood:ind]"))
#' queryStringToLabel(c("wegen dem [tt/p=NN]", "wegen des [tt/p=NN]"))
#'
#' @importFrom PTXQC lcpCount
#' @importFrom PTXQC lcsCount
#'
#' @export
queryStringToLabel <- function(data, pubDateOnly = FALSE, excludePubDate = FALSE) {
  if (pubDateOnly) {
    data <-substring(data, regexpr("(pub|creation)Date", data)+7)
  } else if(excludePubDate) {
    data <-substring(data, 1, regexpr("(pub|creation)Date", data))
  }
  leftCommon = lcpCount(data)
  while (leftCommon > 0 && grepl("[[:alnum:]/=.*!]", substring(data[1], leftCommon, leftCommon))) {
    leftCommon <- leftCommon - 1
  }
  rightCommon = lcsCount(data)
  while (rightCommon > 0 && grepl("[[:alnum:]/=.*!]", substring(data[1], 1+nchar(data[1]) - rightCommon, 1+nchar(data[1]) - rightCommon))) {
    rightCommon <- rightCommon - 1
  }
  substring(data, leftCommon + 1, nchar(data) - rightCommon)
}


## Mute notes: "Undefined global functions or variables:"
globalVariables(c("conf.high", "conf.low", "onRender", "webUIRequestUrl"))


#' Experimental: Plot frequency by year graphs with confidence intervals
#'
#' Experimental convenience function for plotting typical frequency by year graphs with confidence intervals using ggplot2.
#' **Warning:** This function may be moved to a new package.
#'
#' @param mapping	Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ...	 Other arguments passed to geom_ribbon, geom_line, and geom_click_point.
#'
#' @rdname misc-functions
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
#'
#' expand_grid(condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"),
#'             year = (2005:2011)) %>%
#'   cbind(frequencyQuery(kco, "[tt/l=Heuschrecke]",
#'                             paste0(.$condition," & pubDate in ", .$year)))  %>%
#'   ipm() %>%
#'   ggplot(aes(year, ipm, fill = condition, color = condition)) +
#'   geom_freq_by_year_ci()
#' }
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

#'
#' @importFrom ggplot2 ggproto aes GeomPoint
#'
GeomClickPoint <- ggplot2::ggproto(
  "GeomPoint",
  ggplot2::GeomPoint,
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

