#!/usr/bin/env Rscript
#
# Visualize frequencies of alternative query terms in relation to other variables
# specified in virtual corpus definitions.
#
library(RKorAPClient)
library(vcd)
library(tibble)
library(dplyr)
library(PTXQC)

queryStringToLabel <- function(data) {
  leftCommon = lcpCount(data)
  while (leftCommon > 0 && grepl("[[:alpha:]]", substring(data[1], leftCommon, leftCommon))) {
    leftCommon <- leftCommon - 1
  }
  rightCommon = lcsCount(data)
  while (rightCommon > 0 && grepl("[[:alpha:]]", substring(data[1], rightCommon, rightCommon))) {
    rightCommon <- rightCommon - 1
  }
  substring(data, leftCommon + 1, nchar(data) - rightCommon)
}

mosaicplot <- function(query, vc, kco = new("KorAPConnection", verbose = TRUE)) {
  frequencyQuery(
    query = query,
    vc = vc,
    kco,
    expand = TRUE,
    as.alternatives = TRUE
  ) %>%
  mutate(alternative = queryStringToLabel(query), condition = queryStringToLabel(vc)) %>%
    { . ->> df } %>%
    { xtabs(totalResults ~ condition + alternative, .)} %>%
    vcd::mosaic(shade = TRUE) # , labeling = labeling_border(rot_labels = c(45,0,0,0), just_labels = c("left", "center", "center", "right")))
  df
}
df <- mosaicplot(c("[marmot/m=mood:subj]", "[marmot/m=mood:ind]"), c("textDomain=Wirtschaft", "textDomain=Kultur", "textDomain=Sport"))
#mosaicplot(c("Asylbewerber", "Asylwerber"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("wegen dem [tt/p=NN]", "wegen des [tt/p=NN]"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin"))
#mosaicplot(c("Tomaten", "Paradeiser"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin", 'pubPlaceKey=AT'))

