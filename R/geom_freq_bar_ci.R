#' Plot frequency bars with confidence intervals using ggplot2
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Add a column layer and aligned confidence-interval error bars to a ggplot.
#' Like [geom_freq_by_year_ci()], this helper uses the values already mapped
#' in the plot: convert a [frequencyQuery()] result with [ipm()] first, or with
#' [percent()] for alternatives. It does not query KorAP or rescale the data.
#'
#' @param mapping Error-bar bounds, created with [ggplot2::aes()]. Defaults to
#'   ymin = conf.low and ymax = conf.high (xmin and xmax for orientation = "y"). Set x, y, fill and group in the
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
#'   inherits the plot's colour mapping, or uses a neutral grey if none is mapped.
#'   With ggplot2 4, the default grey adapts to the theme's background.
#' @param errorbar.params Named list of additional error-bar appearance options,
#'   for example list(linewidth = 0.4, alpha = 0.8).
#' @param na.rm Remove missing values without a warning, passed to both layers.
#' @param orientation "x" (default) for a categorical x axis and vertical
#'   intervals; "y" for a categorical y axis and horizontal intervals. Both
#'   layers use the same orientation. An implicit horizontal mapping is rejected.
#' @param showCI Show confidence intervals (default TRUE). FALSE adds only the
#'   bar layer and does not require interval bounds.
#' @param ... Additional appearance options passed to [ggplot2::geom_col()],
#'   such as fill, colour, alpha or linewidth.
#' @return A list of ggplot2 layers, added to a plot with +.
#' @details
#' Map fill or group to a query or corpus variable for grouped comparisons.
#' Map group explicitly when fixed fill or colour settings override a grouping
#' aesthetic, so both layers retain the same groups.
#' Stacked bars are not supported because independently calculated confidence
#' limits cannot be stacked. Missing observations are not converted to zeros;
#' use explicit missing rows to reserve slots for absent query/corpus pairs.
#'
#' Use orientation = "y" with aes(x = ipm, y = label), or use
#' [ggplot2::coord_flip()] with the default mapping for horizontal bars. Themes, facets, scales and
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
geom_freq_bar_ci <- function(mapping = NULL,
                             data = NULL, position = "dodge", width = 0.9,
                             errorbar.width = 0.25, errorbar.colour = NULL,
                             errorbar.params = list(), na.rm = FALSE,
                             orientation = "x", showCI = TRUE, ...) {
  validateShowCI(showCI)
  orientation <- match.arg(orientation, c("x", "y"))
  if (is.null(mapping)) mapping <- if (orientation == "y")
    ggplot2::aes(xmin = conf.low, xmax = conf.high) else ggplot2::aes(ymin = conf.low, ymax = conf.high)
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
    width = errorbar.width, na.rm = na.rm, show.legend = FALSE, orientation = orientation)
  if (!is.null(errorbar.colour)) params$colour <- errorbar.colour
  params <- utils::modifyList(params, errorbar.params)
  stat <- ggplot2::ggproto(NULL, ggplot2::StatIdentity,
    setup_params = function(data, params) {
      if (orientation == "x" && inherits(data$y, "mapped_discrete") && !inherits(data$x, "mapped_discrete")) {
        stop("Use orientation = 'y' for aes(x = frequency, y = category), or use coord_flip() with a categorical x axis.", call. = FALSE)
      }
      params
    })
  bar <- ggplot2::geom_col(data = data, position = position, width = width, na.rm = na.rm,
    orientation = orientation, stat = stat, ...)
  if (!showCI) return(list(bar))
  interval <- do.call(ggplot2::geom_errorbar, params)
  defaults <- ggplot2::GeomErrorbar$default_aes
  if ("from_theme" %in% getNamespaceExports("ggplot2")) {
    defaults$colour <- ggplot2::aes(colour = ggplot2::from_theme(
      if (is.null(colour)) chartIntervalColour(paper) else colour))$colour
  } else defaults$colour <- "#737373"
  interval$geom <- ggplot2::ggproto(NULL, ggplot2::GeomErrorbar, default_aes = defaults)
  list(
    bar,
    interval
  )
}

# Theme variables evaluated by ggplot2::from_theme().
utils::globalVariables(c("colour", "paper"))
