#!/usr/bin/env Rscript
#
# Visualize frequencies of alternative query terms in relation to other variables
# specified in virtual corpus definitions.
#
library(RKorAPClient)
library(vcd)
library(tibble)
library(dplyr)

mosaicplot <- function(query, vc, kco = KorAPConnection(verbose = TRUE)) {
  frequencyQuery(
    query = query,
    vc = vc,
    kco,
    expand = TRUE,
    as.alternatives = TRUE
  ) %>%
  mutate(alternative = queryStringToLabel(query), condition = queryStringToLabel(vc)) %>%
    { . ->> queryResult } %>%
    { xtabs(totalResults ~ condition + alternative, .)} %>%
    vcd::mosaic(shade = TRUE) # , labeling = labeling_border(rot_labels = c(45,0,0,0), just_labels = c("left", "center", "center", "right")))
  queryResult
}
queryResult <- mosaicplot(c("[marmot/m=mood:subj]", "[marmot/m=mood:ind]"), c("textDomain=Wirtschaft", "textDomain=Kultur", "textDomain=Sport"))
#mosaicplot(c("Asylbewerber", "Asylwerber"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("wegen dem [tt/p=NN]", "wegen des [tt/p=NN]"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin"))
#mosaicplot(c("Tomaten", "Paradeiser"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin", 'pubPlaceKey=AT'))

