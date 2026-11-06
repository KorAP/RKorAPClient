#' Plot frequency bars with confidence intervals using ggplot2
#'
#' Add a column layer and aligned confidence-interval error bars to a ggplot.
#' Like [geom_freq_by_year_ci()], this helper uses the values already mapped
#' in the plot: convert a [frequencyQuery()] result with [ipm()] first, or with
#' [percent()] for alternatives. It does not query KorAP or rescale the data.
#'
#' @param mapping Error-bar bounds, created with [ggplot2::aes()]. Defaults to
#'   ymin = conf.low and ymax = conf.high. Set x, y, fill and group in the
#'   main [ggplot2::ggplot()] mapping so both layers inherit them.
#' @param data Optional data shared by both layers; otherwise they inherit the
#'   plot data.
#' @param position "dodge" (default), "identity", or a
#'   [ggplot2::position_dodge()] or [ggplot2::position_identity()] object.
#'   Both layers use the same position adjustment. An unspecified dodge width
#'   is set to the bar width.
#' @param width Bar width, default 0.9.
#' @param errorbar.width Width of the error-bar caps, default 0.25.
#' @param errorbar.colour Optional fixed error-bar colour. The default NULL
#'   inherits the plot's colour mapping, or uses black if none is mapped.
#' @param errorbar.params Named list of additional error-bar appearance options,
#'   for example list(linewidth = 0.4, alpha = 0.8).
#' @param na.rm Remove missing values without a warning, passed to both layers.
#' @param ... Additional appearance options passed to [ggplot2::geom_col()],
#'   such as fill, colour, alpha or linewidth.
#' @return A list of two ggplot2 layers, added to a plot with +.
#' @details
#' Map fill or group to a query or corpus variable for grouped comparisons.
#' Map group explicitly when fixed fill or colour settings override a grouping
#' aesthetic, so both layers retain the same groups.
#' Stacked bars are not supported because independently calculated confidence
#' limits cannot be stacked. Missing observations are not converted to zeros;
#' use explicit missing rows to reserve slots for absent query/corpus pairs.
#'
#' Use [ggplot2::coord_flip()] for horizontal bars. Themes, facets, scales and
#' [ggplot2::ggsave()] work as usual, including PDF and SVG output.
#' @seealso [hc_freq_bar_ci()] for interactive charts;
#'   [geom_freq_by_year_ci()] for time series.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' kco <- KorAPConnection("https://korap.dnb.de")
#' frequencyQuery(kco, "Berlin",
#'   vc = c(Buchpreis = "award=buchpreis", DeLiKo = "")) |>
#'   ipm() |>
#'   ggplot(aes(x = label, y = ipm)) +
#'   geom_freq_bar_ci() +
#'   theme_bw()
#'
#' frequencyQuery(kco, c("Berlin", "Paris"),
#'   vc = c(Buchpreis = "award=buchpreis", DeLiKo = ""), expand = TRUE) |>
#'   ipm() |>
#'   ggplot(aes(x = label, y = ipm, fill = query)) +
#'   geom_freq_bar_ci() +
#'   coord_flip()
#' }
geom_freq_bar_ci <- function(mapping = ggplot2::aes(ymin = conf.low, ymax = conf.high),
                             data = NULL, position = "dodge", width = 0.9,
                             errorbar.width = 0.25, errorbar.colour = NULL,
                             errorbar.params = list(), na.rm = FALSE, ...) {
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0 ||
      !is.numeric(errorbar.width) || length(errorbar.width) != 1L ||
      !is.finite(errorbar.width) || errorbar.width < 0) {
    stop("width must be positive and errorbar.width must be non-negative.", call. = FALSE)
  }
  if (identical(position, "dodge")) {
    position <- ggplot2::position_dodge(width = width)
  } else if (identical(position, "identity")) {
    position <- ggplot2::position_identity()
  }
  if (inherits(position, "PositionDodge2") ||
      (!inherits(position, "PositionDodge") && !inherits(position, "PositionIdentity"))) {
    stop("Use position_dodge() or position_identity(); independently calculated intervals cannot be stacked.", call. = FALSE)
  }
  if (inherits(position, "PositionDodge") && is.null(position$width)) {
    parent <- position
    position <- ggplot2::ggproto(NULL, parent, width = width)
  }
  if (!is.list(errorbar.params) || (length(errorbar.params) &&
      (is.null(names(errorbar.params)) || anyNA(names(errorbar.params)) ||
       any(!nzchar(names(errorbar.params)))))) {
    stop("errorbar.params must be a named list.", call. = FALSE)
  }
  protected <- c("data", "mapping", "position", "stat", "inherit.aes", "na.rm", "orientation")
  if (any(protected %in% names(errorbar.params)) ||
      any(c("stat", "inherit.aes", "orientation") %in% names(list(...)))) {
    stop("Data, mappings and position adjustments must stay shared by both layers.", call. = FALSE)
  }
  params <- list(mapping = mapping, data = data, position = position,
    width = errorbar.width, na.rm = na.rm, show.legend = FALSE)
  if (!is.null(errorbar.colour)) params$colour <- errorbar.colour
  params <- utils::modifyList(params, errorbar.params)
  list(
    ggplot2::geom_col(data = data, position = position, width = width, na.rm = na.rm, ...),
    do.call(ggplot2::geom_errorbar, params)
  )
}
