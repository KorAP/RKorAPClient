library(RKorAPClient)

displayKwics <- function(q, htmlFile = file.path(tempfile(fileext = ".html"))) {
  cat(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <!DOCTYPE html>
    <html>
     <header>
      <style>
            body {
              font-family: "Lato", sans-serif;
              font-size: 10pt;
            }
            div.kwic {
                display: table-row;
                line-height: 180%;
            }
            div.kwic:nth-child(even) {background: #EEE}
            div.kwic:nth-child(odd) {background: #FFF}
            .sigle {
                font-size: 90%;
                font-family: "Courier New", monospace;
                display: table-cell;
                text-align: right;
                padding-right: 1ex;
            }
            .context-left {
                display: table-cell;
                text-align: right;
                overflow: hidden;
                white-space: nowrap;
            }
            .context-right {
                display: table-cell;
                text-align: left;
                white-space: nowrap;
                overflow: hidden;
            }
            .match {
                display: table-cell;
                padding-left: 1ex;
                padding-right: 1ex;
                text-align: center;
            }
      </style>
    </header>
    <body>
      <table>',
    paste0(
      "<div class='kwic'><span class='sigle'>",
      q@collectedMatches$textSigle,
      "</span> ",
      as.character(q@collectedMatches$snippet),
      "</div>"
    ),
    '      </table>
    </body>
</html>',
    file = htmlFile,
    sep = "\n"
  )
  viewer <- getOption("viewer")
  viewer(htmlFile)
}

kco <- new("KorAPConnection", verbose = TRUE)
if (is.null(kco@accessToken)) {
  message(
    paste0(
      "In order to receive KWICSs also from corpora with restricted licenses, you need an access token.\n",
      "To generate an access token, login to KorAP and navigite to KorAP's OAuth settings <",
      kco@KorAPUrl,
      "settings/oauth#page-top>"
    )
  )
}
q <- corpusQuery(kco, "Ameisenplage", metadataOnly = F) %>%
  fetchAll() %>%
  displayKwics()
