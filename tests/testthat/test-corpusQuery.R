test_that("corpusQuery returns results", {
  q <- new("KorAPConnection") %>%
    corpusQuery("Ameisenplage")
  expect_gt(q@totalResults, 0)
})

test_that("fetchAll fetches all results", {
  q <- new("KorAPConnection", verbose = TRUE) %>%
    corpusQuery("Ameisenplage", vc = "pubDate since 2014")
  expectedResults <- q@totalResults
  matches <- fetchAll(q)@collectedMatches
  expect_equal(nrow(matches), expectedResults)
})

test_that("Uncached query for non-matching search string return 0 results", {
  q <- new("KorAPConnection", cache = FALSE) %>% corpusQuery("Xmeisenplagx")
  expect_equal( q@totalResults, 0)
})

test_that("Empty query result is printable", {
  q <- new("KorAPConnection", cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Xmeisenplagx", vc = "pubDate in 2012") %>%
    fetchNext()
  expect_output(print(q), "Xmeisenplagx.*pubDate in 2012")
})

test_that("Non-empty query result is printable", {
  q <- new("KorAPConnection", cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Ameisenplage", "pubDate since 2014", fields=c("sigle")) %>%
    fetchRest()
  expect_output(print(q), "Ameisenplage.*pubDate since 2014")
})

test_that("Query from KorAP URL returns as many results as corresponding direct query", {
  kco <- new("KorAPConnection")
  r1 <- corpusQuery(kco, KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameisenplage&cq=pubDate+since+2014&ql=poliqarp")@totalResults
  r2 <- corpusQuery(kco, "Ameisenplage", "pubDate since 2014")@totalResults
  expect_equal(r1, r2)
})

test_that("Typo in query causes error", {
  kco <- new("KorAPConnection", verbose = TRUE)
  expect_error(kco %>% corpusQuery("[[xx"), "unbalanced")
})
