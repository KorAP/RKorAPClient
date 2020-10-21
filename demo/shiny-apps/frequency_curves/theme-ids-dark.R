#' Dark IDS theme for highcharts
#'
#' @param ... Named argument to modify the theme
#'
#' @examples
#'
#' highcharts_demo() %>%
#'   hc_add_theme(hc_theme_ids_dark())
#' @export
hc_theme_ids_dark <- function(...) {
  theme <-
    list(
      colors = c('#EB7C31', "#1F77B4", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"),
#      colors = c(
#        '#EB7C31', "#9BAD0B", "#2b908f", "#90ee7e", "#f45b5b", "#7798BF",
#        "#aaeeee", "#ff0066", "#eeaaee", "#55BF3B"
#      ),
      chart = list(
        backgroundColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 1, y2 = 0),
          stops = list(
            list(0, "#2a2a2b"),
            list(1, "#3e3e3e")
          )
        ),
        style = list(
          fontFamily = "Roboto Condensed",
          fontFamily = '"Univers LT Std 47 Cn Lt", "Roboto Condensed", "Unica One", sans-serif-condensed, sans',
          fontSize = "12pt"
        ),
        plotBorderColor = "#606063"
      ),
      title = list(
        style = list(
          color = "#E0E0E3",
          fontSize = "20px"
        )
      ),
      subtitle = list(
        style = list(
          color = "#E0E0E3",
          fontSize = "14pt"
        )
      ),
      xAxis = list(
        gridLineColor = "#707073",
        labels = list(
          style = list(
            color = "#E0E0E3",
            fontSize = "12pt"
          )
        ),
        lineColor = "#707073",
        minorGridLineColor = "#505053",
        tickColor = "#707073",
        title = list(
          style = list(
            color = "#A0A0A3",
            fontSize = "12pt"
          )
        )
      ),
      yAxis = list(
        gridLineColor = "#707073",
        labels = list(
          style = list(
            color = "#E0E0E3",
            fontSize = "12pt"
          )
        ),
        lineColor = "#707073",
        minorGridLineColor = "#505053",
        tickColor = "#707073",
        tickWidth = 1,
        title = list(
          style = list(
            color = "#A0A0A3",
            fontSize = "12pt"
          )
        )
      ),
      tooltip = list(
        backgroundColor = "rgba(0, 0, 0, 0.85)",
        style = list(
          color = "#E0E0E0",
          fontSize = "11pt"
        )
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(
            color = "#B0B0B3",
            fontSize = "13pt"
          ),
          marker = list(
            lineColor = "#333"
          )
        ),
        boxplot = list(
          fillColor = "#505053"
        ),
        candlestick = list(
          lineColor = "white"
        ),
        errorbar = list(
          color = "white"
        )
      ),
      legend = list(
        itemStyle = list(
          color = "#E0E0E3"
        ),
        itemHoverStyle = list(
          color = "#FFF"
        ),
        itemHiddenStyle = list(
          color = "#606063"
        )
      ),
      credits = list(
        style = list(
          color = "#666"
        )
      ),
      labels = list(
        style = list(
          color = "#707073"
        )
      ),

      drilldown = list(
        activeAxisLabelStyle = list(
          color = "#F0F0F3"
        ),
        activeDataLabelStyle = list(
          color = "#F0F0F3"
        )
      ),

      navigation = list(
        buttonOptions = list(
          symbolStroke = "#DDDDDD",
          theme = list(
            fill = "#505053"
          )
        )
      ),

      rangeSelector = list(
        buttonTheme = list(
          fill = "#505053",
          stroke = "#000000",
          style = list(
            color = "#CCC"
          ),
          states = list(
            hover = list(
              fill = "#707073",
              stroke = "#000000",
              style = list(
                color = "white"
              )
            ),
            select = list(
              fill = "#000003",
              stroke = "#000000",
              style = list(
                color = "white"
              )
            )
          )
        ),
        inputBoxBorderColor = "#505053",
        inputStyle = list(
          backgroundColor = "#333",
          color = "silver"
        ),
        labelStyle = list(
          color = "silver"
        )
      ),

      navigator = list(
        handles = list(
          backgroundColor = "#666",
          borderColor = "#AAA"
        ),
        outlineColor = "#CCC",
        maskFill = "rgba(255,255,255,0.1)",
        series = list(
          color = "#7798BF",
          lineColor = "#A6C7ED"
        ),
        xAxis = list(
          gridLineColor = "#505053"
        )
      ),

      scrollbar = list(
        barBackgroundColor = "#808083",
        barBorderColor = "#808083",
        buttonArrowColor = "#CCC",
        buttonBackgroundColor = "#606063",
        buttonBorderColor = "#606063",
        rifleColor = "#FFF",
        trackBackgroundColor = "#404043",
        trackBorderColor = "#404043"
      ),

      legendBackgroundColor = "rgba(0, 0, 0, 0)",
      background2 = "#233238",
      dataLabelsColor = "#233238",
      textColor = "#34495e",
      maskColor = "rgba(255,255,255,0.3)",
      contrastTextColor = "#F0F0F3"
    )

  theme <- structure(theme, class = "hc_theme")

  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }

  theme
}
