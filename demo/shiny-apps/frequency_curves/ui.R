library(shiny)
library(highcharter)
library(RKorAPClient)
library(utils)
library(stringr)
library(idsThemeR)
#source("theme-ids-dark.R")

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
          style = "margin-top: 25px;",
          actionButton("goButton", "Mit KorAP suchen", icon("search"), class = "btn btn-primary")
        )
      ),
      fluidRow(height = 10,
               highchartOutput("hcontainer", height = "600px"))
    )
  )
)
