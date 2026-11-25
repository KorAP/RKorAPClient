#' Plot distributions as box plots using ECharts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Box plots of any numeric values, such as the frequencies in single texts from
#' [textFrequencyQuery()], with localized tooltips and outliers linked to KorAP.
#'
#' @param df Data frame with one row per observation. If it has a column
#'   `webUIRequestUrl`, clicks on outliers open the corresponding search in KorAP.
#' @param value Column (or expression of columns) with the values to be plotted.
#' @param category Column to use for the categories on the axis. Factor levels
#'   determine the order, otherwise the order of occurrence.
#' @param group Optional column to split each category by, shown in colour and
#'   with a legend.
#' @param facet Optional column to draw separate panels by, side by side and with
#'   value axes of their own, for values of different magnitude.
#' @param pointLabel Optional column with the names of observations to be shown in
#'   the tooltips of outliers, such as titles of texts.
#' @param outliers Draw values beyond the whiskers as points. If `FALSE`, the value
#'   axes end at the whiskers and the number of outliers is given in the caption.
#' @param horizontal Draw horizontal instead of vertical boxes.
#' @param ylabel Label of the value axis.
#' @param unit Unit shown with the values in the tooltips, `""` for none.
#' @details
#' Boxes show the quartiles and the median as computed by [grDevices::boxplot.stats()],
#' whiskers extend to the most extreme value within 1.5 times the interquartile
#' range. Missing values are left out with a warning.
#'
#' If at least three quarters of the values are equal, the box and its whiskers
#' shrink to a line. This is typical of words that do not occur in most texts:
#' all texts with hits are outliers then.
#' @return An echarts4r htmlwidget, customizable with echarts4r functions.
#' @inheritSection ec_freq_by_year_ci Themes
#' @inheritSection ec_freq_by_year_ci Untrusted text
#' @inheritSection hc_freq_bar_ci Display language
#' @family echarts-helpers
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @export
#' @examples
#' \dontrun{
#' KorAPConnection("https://korap.dnb.de") |>
#'   textFrequencyQuery(c("Himmel", "Hölle"),
#'     vc = c(Nominees = "award = buchpreis & pubDate in 2020",
#'            Others = "award != buchpreis & pubDate in 2020 & textSigle = /.*05/")) |>
#'   ec_boxplot(f * 1e6, query, group = label, pointLabel = title, ylabel = "IPM")
#' }
ec_boxplot <- function(df, value, category = "", group = NULL, facet = NULL, pointLabel = NULL,
                       outliers = TRUE, horizontal = FALSE, ylabel = "", unit = ylabel) {
  requireEcharts()
  if (!is.data.frame(df) || !nrow(df)) stop("df must be a data frame with at least one row.", call. = FALSE)
  for (flag in list(outliers, horizontal)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("outliers and horizontal must be TRUE or FALSE.", call. = FALSE)
    }
  }
  for (string in list(ylabel, unit)) {
    if (!is.character(string) || length(string) != 1L || is.na(string)) {
      stop("ylabel and unit must be single strings.", call. = FALSE)
    }
  }
  column <- function(quo, name) {
    x <- if (quo_is_null(quo)) "" else eval_tidy(quo, df)
    if (is.character(x) && length(x) == 1L && nrow(df) > 1L && x %in% names(df)) {
      stop(name, " must be a column name without quotes: ", x, call. = FALSE)
    }
    if (length(x) == 1L) x <- rep(x, nrow(df))
    if (!is.atomic(x) || length(x) != nrow(df)) stop(name, " must be a column of df.", call. = FALSE)
    if (anyNA(x) && !name %in% c("value", "pointLabel")) stop(name, " must not be missing.", call. = FALSE)
    if (is.factor(x)) droplevels(x) else if (name == "value") x else factor(x, unique(x))
  }
  y <- column(enquo(value), "value")
  if (!is.numeric(y) || any(is.infinite(y))) stop("value must be finite and numeric.", call. = FALSE)
  category <- column(enquo(category), "category")
  grouped <- !quo_is_null(enquo(group))
  group <- column(enquo(group), "group")
  faceted <- !quo_is_null(enquo(facet))
  facet <- column(enquo(facet), "facet")
  pointLabel <- as.character(column(enquo(pointLabel), "pointLabel"))
  if (anyNA(y)) warning(sum(is.na(y)), " missing values are left out.", call. = FALSE)
  if (all(is.na(y))) stop("value has no values other than NA.", call. = FALSE)

  hidden <- 0L
  series <- list()
  for (i in seq_len(nlevels(facet))) for (j in seq_len(nlevels(group))) {
    boxes <- list()
    points <- list()
    for (k in seq_len(nlevels(category))) {
      rows <- which(facet == levels(facet)[i] & group == levels(group)[j] &
        category == levels(category)[k] & !is.na(y))
      if (!length(rows)) next
      stats <- grDevices::boxplot.stats(y[rows])$stats
      name <- if (nzchar(levels(category)[k])) levels(category)[k] else levels(facet)[i]
      common <- list(korapTooltipGroup = if (grouped) levels(group)[j], korapTooltipUnit = unit)
      boxes <- c(boxes, list(list(name = name, value = as.list(c(k - 1, stats, 0)),
        korap = c(list(korapTooltipType = "box", count = length(rows), stats = as.list(stats)), common))))
      beyond <- rows[y[rows] < stats[1] | y[rows] > stats[5]]
      if (!outliers) hidden <- hidden + length(beyond)
      # the last value tells points from boxes
      else points <- c(points, lapply(beyond, function(r) {
        labelled <- !is.na(pointLabel[r]) && nzchar(pointLabel[r])
        point <- list(name = if (labelled) pointLabel[r] else name, value = as.list(c(k - 1, rep(y[r], 5), 1)),
          korap = c(list(korapTooltipType = "outlier", y = y[r],
            korapTooltipCategory = if (labelled) name), common))
        if ("webUIRequestUrl" %in% names(df)) point$webUIRequestUrl <- chartUrl(as.character(df$webUIRequestUrl[r]))
        point
      }))
    }
    series <- c(series, list(list(type = "custom", name = if (grouped) levels(group)[j] else "value",
      xAxisIndex = i - 1L, yAxisIndex = i - 1L, clip = TRUE, animation = FALSE,
      encode = if (horizontal) list(y = 0, x = as.list(1:5)) else list(x = 0, y = as.list(1:5)),
      data = c(boxes, points))))
  }

  share <- 100 / nlevels(facet)
  top <- if (faceted) 50 else 40
  grids <- lapply(seq_len(nlevels(facet)), function(i) list(
    left = paste0((i - 1) * share + 1, "%"), width = paste0(share - 2, "%"), top = top, bottom = 30,
    # an axis title that only one panel has must not make that panel smaller
    outerBoundsMode = "same", outerBoundsContain = if (horizontal) "all" else "axisLabel"))
  values <- lapply(seq_len(nlevels(facet)), function(i) list(type = "value", gridIndex = i - 1L,
    boundaryGap = list(0, "3%"),
    # once is enough next to each other
    name = if (horizontal || i == 1L) ylabel else "",
    nameLocation = if (horizontal) "middle" else "end", nameGap = if (horizontal) 28 else 12))
  categories <- lapply(seq_len(nlevels(facet)), function(i) list(type = "category", gridIndex = i - 1L,
    data = as.list(levels(category)), inverse = horizontal, axisTick = list(show = FALSE),
    axisLabel = list(interval = 0, width = 180, overflow = if (horizontal) "truncate" else "break")))
  # the second title is reserved for the caption
  titles <- c(list(list(text = "", left = "center"), list(text = "")), if (faceted) lapply(seq_len(nlevels(facet)),
    function(i) list(text = levels(facet)[i], left = paste0((i - .5) * share, "%"), top = 8,
      textAlign = "center", textStyle = list(fontSize = 14))))
  korapEchart(list(
    title = titles,
    grid = grids,
    xAxis = if (horizontal) values else categories,
    yAxis = if (horizontal) categories else values,
    series = series
  ), list(kind = "box", hidden = hidden, outliers = outliers), chartPalette(nlevels(group)), "item",
    horizontal = horizontal,
    height = if (horizontal) max(380, 170 + nlevels(category) * (20 * nlevels(group) + 16)) else NULL,
    frequencySeries = nlevels(group), categories = nlevels(category) * nlevels(facet))
}
