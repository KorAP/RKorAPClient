#!/usr/bin/env Rscript
#
# Visualize frequencies of alternative query terms depending on other variables
# specified in virtual corpus definitions.
#
library(RKorAPClient)
library(vcd)
library(tibble)
library(dplyr)

lcPrefixLen <- function(data) {
  which.max(apply(do.call(rbind, lapply(
    strsplit(data, ''), `length<-`, nchar(data[1])
  )), 2, function(i)
    ! length(unique(i)) == 1))
}

queryStringToLabel <- function(data) {
  gsub("[\\[\\]]", "", substring(data, lcPrefixLen(data)), perl = TRUE)
}

mosaicplot <- function(query, vc, kco = new("KorAPConnection", verbose = TRUE)) {
  frequencyQuery(
    query = query,
    vc = vc,
    kco,
    expand = TRUE,
    as.alternatives = TRUE
  ) %>%
  mutate(query=queryStringToLabel(query), vc=queryStringToLabel(vc)) %>%
  { . ->> df } %>%
  select(query, vc, totalResults) %>%
  tidyr::pivot_wider(names_from = vc, values_from = totalResults) %>%
  mutate_if(is.integer, as.double) %>%
  remove_rownames() %>%
  column_to_rownames("query") %>%
  data.matrix() %>%
  t() %>%
  vcd::mosaic(shade = TRUE)
  df
}
df <- mosaicplot(c("[marmot/m=mood:subj]", "[marmot/m=mood:ind]"), c("textDomain=Wirtschaft", "textDomain=Kultur", "textDomain=Sport"))
#mosaicplot(c("Asylbewerber", "Asylwerber"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("wegen dem [tt/p=NN]", "wegen des [tt/p=NN]"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin"))
#mosaicplot(c("Tomaten", "Paradeiser"), c("pubPlaceKey=DE", "pubPlaceKey=AT"))
#mosaicplot(c("Samstag", "Sonnabend"), c("pubPlace=Hamburg", "pubPlace=Berlin", 'pubPlaceKey=AT'))

