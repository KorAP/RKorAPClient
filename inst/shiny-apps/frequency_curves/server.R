hc_theme <- if(require(idsThemeR)) {
  idsThemeR::hc_theme_ids_light()
} else {
  hc_theme_hcrt()
}
rsr <- new("KorAPConnection", verbose = TRUE, accessToken = NULL)
vc <- "(textType = /Zeit.*/ | textTypeRef=Plenarprotokoll) & availability!=QAO-NC-LOC:ids & creationDate in"
years <- c(2005:2020)
from <- 2005
to <- 2020
query <- "Aluhut"
logfile <- file("frequency_curves.log", open = "a")

# override log.info in RKorAPClient to get some progress info
log.info <- function(v,  ...) {
  original = paste0(...)
  detail <- if (str_detect(original, "Searching.*in ([0-9]{4})")) {
    str_replace(original, ".*in ([0-9]{4}).*", "Suche in \\1")
  } else if (str_detect(original, "Getting size.*in ([0-9]{4})")) {
    str_replace(original, ".*in ([0-9]{4}).*", "Korpusgröße \\1")
  } else {
    "Randverteilung"
  }
  incProgress(1 / (2 * length(query) * length(years) + length(years)), detail = detail)
  cat(original, file = logfile)
  flush(logfile)
}

assignInNamespace("log.info", log.info, "RKorAPClient")

plotHighchart <- function(query = c("Tolpatsch", "Tollpatsch"),
                          vc = "(textType = /Zeit.*/ | textTypeRef=Plenarprotokoll) & availability!=QAO-NC-LOC:ids & creationDate in"
,
                          years = years,
                          as.alternatives = F,
                          conf.level = 0.95,
                          kco = rsr) {
  hc <- frequencyQuery(kco,
                       query,
                       paste(vc, years),
                       as.alternatives = as.alternatives) %>%
    hc_freq_by_year_ci(as.alternatives, smooth = T) %>%
    hc_yAxis(title = list(text = "Instanzen pro Million Wörter")) %>%
    hc_add_theme(hc_theme) %>%
    hc_caption(text = paste(
      "Frequenzverläufe (mit 95%-Konfidenzbändern) im",
      "<a href='http://www.dereko.de'>Deutschen Referenzkorpus DeReKo</a>",
      "(virtuelles Korpus: <a href='https://korap.ids-mannheim.de/doc/corpus'>DeReKo-KorAP-2021-I</a>",
      "eingegrenzt auf Zeitungen, Zeitschriften und Plenarprotokolle).",
      "Klicken sie die einzelnen Datenpunkte an, um entsprechende KorAP-Suchen zu starten."
      ))

  hc
}

generateHighchart <- function(wordParam, from=2005, to=2020) {
  years <<- c(from:to)
  if (wordParam != "") {
    query <<- strsplit(wordParam, " *, *")
    withProgress(message = 'Berechnung läuft: ', value = 0, {
      hc <- plotHighchart(query, vc , years)
    })
    hc
  }
}


function(input, output, session) {
  observe({
    queryParams <- parseQueryString(session$clientData$url_search)
    if (!is.null(queryParams[['from']])) {
      from <- queryParams[['from']]
      updateSliderInput(session, "from", value = from)
    } else {
      from <- 2005
    }
    if (!is.null(queryParams[['to']])) {
      to <- queryParams[['to']]
      updateSliderInput(session, "to", value = to)
    } else {
      to <- 2020
    }
    if (!is.null(queryParams[['q']])) {
      paramWord <- queryParams[['q']]
      updateTextInput(session, "q", value = paramWord)
      output$hcontainer <-
        renderHighchart(generateHighchart(paramWord, from, to))
    }
  })

  observeEvent(input$goButton,
               {
                 output$hcontainer <-
                   renderHighchart(generateHighchart(isolate(input$q), isolate(input$from), isolate(input$to)))
               })

}
