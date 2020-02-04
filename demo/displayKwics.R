library(RKorAPClient)

displayKwics <- function(q,
                         htmlFile = file.path(tempfile(fileext = ".html"))
                         ) {
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
            }
            div.kwic:nth-child(even) {background: #EEE}
            div.kwic:nth-child(odd) {background: #FFF}
            .sigle {
                font-size: 90%;
                font-family: "M+1M",  "Latin Modern Mono", courier;
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
        paste0("<div class='kwic'><span class='sigle'>", q@collectedMatches$textSigle, "</span> ", as.character(q@collectedMatches$snippet), "</div>"),
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
  message("To receive KWICSs also from corpora with restricted licenses, you need an access token which you can currently only get as a beta tester. If you have an access token, you also need to persist it with 'persistAccessToken()'.")
}
corpusQuery(kco, "Ameisenplage", metadataOnly=F) %>%
    fetchNext(maxFetch=500) %>%
    displayKwics()


