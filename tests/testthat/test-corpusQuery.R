test_that("frequencyQuery can be cached", {
  skip_if_offline()
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  frequencyQuery(kco, "Ameisenplage", "pubDate since 2014")
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "cached")
})

test_that("collocationScoreQuery works", {
  skip_if_offline()
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  df <- collocationScoreQuery(kco,"Ameisenplage", "heimgesucht", leftContextSize=0, rightContextSize=1)
  expect_gt(df$logDice, 1)
  expect_equal(df$ll, ll(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$pmi, pmi(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi2, mi2(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi3, mi3(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$logDice, logDice(df$O1, df$O2, df$O, df$N, df$E, df$w))
})

test_that("Cache depends on indexRevision (handling also NULL values)", {
  skip_if_offline()
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  kco@indexRevision <- NULL
  frequencyQuery(kco, "Ameisenplage", "pubDate since 2014")
  kco@indexRevision <- as.character(Sys.time())
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "took ")
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "cached")
})

test_that("corpusQuery returns results", {
  skip_if_offline()
  q <- new("KorAPConnection") %>%
    corpusQuery("Ameisenplage")
  expect_gt(q@totalResults, 0)
})

test_that("corpusQuery can handle latin1 encoded umlauts in query and vc", {
  query <- "Ameisenb\xe4r"
  Encoding(query)="latin1"
  vc <- "pubPlace=N\xfcrnberg"
  Encoding(vc)="latin1"

  skip_if_offline()
  q <- new("KorAPConnection") %>%
    corpusQuery(query, vc=vc)
  expect_gt(q@totalResults, 0)
})

test_that("corpusQuery can handle utf8 encoded umlauts in query and vc", {
  query <- "Ameisenb\xc3\xa4r"
  Encoding(query)="UTF-8"
  vc <- "pubPlace=N\xc3\xbcrnberg"
  Encoding(vc)="UTF-8"

  skip_if_offline()
  q <- new("KorAPConnection") %>%
    corpusQuery(query, vc=vc)
  expect_gt(q@totalResults, 0)
})

test_that("fetchAll fetches all results", {
  skip_if_offline()
  q <- new("KorAPConnection", verbose = TRUE) %>%
    corpusQuery("Ameisenplage", vc = "pubDate since 2014")
  expectedResults <- q@totalResults
  matches <- fetchAll(q)@collectedMatches
  expect_equal(nrow(matches), expectedResults)
})

test_that("fetchAll fetches textClass metadatum", {
  skip_if_offline()
  q <- new("KorAPConnection", verbose = TRUE) %>%
    corpusQuery("Ameisenplage", vc = "pubDate since 2014")
  expectedResults <- q@totalResults
  matches <- fetchAll(q)@collectedMatches
  expect_true(any(grepl("wissenschaft ", matches$textClass)))
  expect_true(any(grepl(" populaerwissenschaft", matches$textClass)))
  expect_true(any(grepl("kultur literatur", matches$textClass)))
})

test_that("Uncached query for non-matching search string return 0 results", {
  skip_if_offline()
  q <- new("KorAPConnection", cache = FALSE) %>% corpusQuery("Xmeisenplagx")
  expect_equal( q@totalResults, 0)
})

test_that("Empty query result is printable", {
  skip_if_offline()
  q <- new("KorAPConnection", cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Xmeisenplagx", vc = "pubDate in 2012") %>%
    fetchNext()
  expect_output(print(q), "Xmeisenplagx.*pubDate in 2012")
})

test_that("Non-empty query result is printable", {
  skip_if_offline()
  q <- new("KorAPConnection", cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Ameisenplage", "pubDate since 2014", fields=c("textSigle")) %>%
    fetchRest()
  expect_output(print(q), "Ameisenplage.*pubDate since 2014")
})

test_that("Query from KorAP URL returns as many results as corresponding direct query", {
  skip_if_offline()
  kco <- new("KorAPConnection")
  r1 <- corpusQuery(kco, KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameisenplage&cq=pubDate+since+2014&ql=poliqarp")@totalResults
  r2 <- corpusQuery(kco, "Ameisenplage", "pubDate since 2014")@totalResults
  expect_equal(r1, r2)
})

test_that("Typo in query causes error", {
  skip_if_offline()
  kco <- new("KorAPConnection", verbose = TRUE)
  expect_message(kco %>% corpusQuery("[[xx"), "unbalanced")
})
