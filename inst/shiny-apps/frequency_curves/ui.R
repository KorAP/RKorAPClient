library(shiny)
library(highcharter)
library(RKorAPClient)
library(utils)
library(stringr)
library()
options(shiny.autoreload = TRUE)


shinyUI(
  fluidPage(
    title = "Wortfrequenzverläufe in DeReKo",
    tags$link(rel = "stylesheet", type = "text/css", href = "frequency_curves.css"),
    tags$head(tags$script(src = "enter-button.js")),
    fluidPage(
      titlePanel("Wortfrequenzverläufe im Deutschen Referenzkorpus"),
      fluidRow(
        height = 2,
        class = "panel",
        column(width = 4, textInput("q", label = "Wortform oder Suchausdruck",
                                    placeholder = "Mit Komma getrennte Liste")),
        column(
          width = 1,
          offset = 0,
          style = "margin-top: 25px;",
          actionButton("goButton", "Mit KorAP suchen", icon("search"), class = "btn btn-primary")
        ),
        column(width = 2, offset=3,
          sliderInput("from", "Von", sep = "",
                    min = 1949, max = 2021,
                    value = 2005)
        ),
        column(width = 2, offset=0,
               sliderInput("to", "Bis", sep = "",
                           min = 1949, max = 2021,
                           value = 2021)
        )

      ),
      fluidRow(height = 10,
               highchartOutput("hcontainer", height = "600px"))
    )
  )
)
