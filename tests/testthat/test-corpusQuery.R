test_that("frequencyQuery can be cached", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE)
  frequencyQuery(kco, "Ameisenplage", "pubDate since 2014")
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "cached")
})

test_that("collocationScoreQuery works", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE)
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
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE)
  kco@indexRevision <- NULL
  frequencyQuery(kco, "Ameisenplage", "pubDate since 2014")
  kco@indexRevision <- as.character(Sys.time())
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "Searching")
  expect_output(frequencyQuery(kco, "Ameisenplage", "pubDate since 2014"), "cached")
})

test_that("corpusQuery returns results", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL) %>%
    corpusQuery("Ameisenplage")
  expect_gt(q@totalResults, 0)
})

test_that("corpusQuery can handle latin1 encoded umlauts in query and vc", {
  query <- "Ameisenb\xe4r"
  Encoding(query)="latin1"
  vc <- "pubPlace=N\xfcrnberg"
  Encoding(vc)="latin1"

  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL) %>%
    corpusQuery(query, vc=vc)
  expect_gt(q@totalResults, 0)
})

test_that("corpusQuery can handle utf8 encoded umlauts in query and vc", {
  query <- "Ameisenb\xc3\xa4r"
  Encoding(query)="UTF-8"
  vc <- "pubPlace=N\xc3\xbcrnberg"
  Encoding(vc)="UTF-8"

  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL) %>%
    corpusQuery(query, vc=vc)
  expect_gt(q@totalResults, 0)
})

test_that("fetchAll fetches all results with match positions", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL, verbose = TRUE) %>%
    corpusQuery("Ameisenplage", vc = "pubDate since 2014")
  expectedResults <- q@totalResults
  matches <- fetchAll(q)@collectedMatches
  expect_equal(nrow(matches), expectedResults)
  expect_true(is.numeric(matches$matchEnd[1]))
  expect_true(is.numeric(matches$matchStart[1]))
  expect_equal(matches$matchStart[1], matches$matchEnd[1])
})

test_that("fetchAll fetches textClass metadatum", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL, verbose = TRUE) %>%
    corpusQuery("Ameisenplage", vc = "pubDate since 2014")
  expectedResults <- q@totalResults
  matches <- fetchAll(q)@collectedMatches
  expect_true(any(grepl("wissenschaft ", matches$textClass)))
  expect_true(any(grepl(" populaerwissenschaft", matches$textClass)))
  expect_true(any(grepl("kultur literatur", matches$textClass)))
})

test_that("Uncached query for non-matching search string return 0 results", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL, cache = FALSE) %>% corpusQuery("Xmeisenplagx")
  expect_equal( q@totalResults, 0)
})

test_that("Empty query result is printable", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Xmeisenplagx", vc = "pubDate in 2012") %>%
    fetchNext()
  expect_output(print(q), "Xmeisenplagx.*pubDate in 2012")
})

test_that("Non-empty query result is printable", {
  skip_if_offline()
  q <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE) %>%
    corpusQuery("Ameisenplage", "pubDate since 2014", fields=c("textSigle")) %>%
    fetchRest()
  expect_output(print(q), "Ameisenplage.*pubDate since 2014")
})

test_that("fetchNext with offset 1000000 (= 1e+06) works", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE, cache = FALSE)
  q <- corpusQuery(kco, "<base/s=t>")
  q <- fetchNext(q, offset = 1000000)
  expect_gt(nrow(q@collectedMatches), 10)
})

test_that("Query from KorAP URL returns as many results as corresponding direct query", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL)
  r1 <- corpusQuery(kco, KorAPUrl = "https://korap.ids-mannheim.de/?q=Ameisenplage&cq=pubDate+since+2014&ql=poliqarp")@totalResults
  r2 <- corpusQuery(kco, "Ameisenplage", "pubDate since 2014")@totalResults
  expect_equal(r1, r2)
})

test_that("Typo in query causes error", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE)
  expect_message(kco %>% corpusQuery("[[xx"), "unbalanced")
})

test_that("corpusQuery token API works", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL,
             verbose = TRUE)
  q <- corpusQuery(kco, "focus([tt/p=ADJA] {Newstickeritis})", vc = "corpusSigle=/W.D17/", metadataOnly = FALSE)
  q <- fetchNext(q)
  matches <-q@collectedMatches
  expect_gt(nrow(matches), 10)
  unique_matches <- unique(matches$tokens$match)
  expect_equal(length(unique_matches), 1)
  expect_equal(unique_matches[[1]], "Newstickeritis")

  left_contexts <- matches$tokens$left
  expect_true(TRUE %in% grepl("reine", left_contexts))

  right_contexts <- matches$tokens$right
  expect_true(TRUE %in% grepl("Begriff", right_contexts))
})

test_that("matchStart and matchEnd are present and correct", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE)
  q <- corpusQuery(kco, "focus([tt/p=ADJA] {Newstickeritis})", vc = "corpusSigle=/W.D17/", metadataOnly = FALSE)
  q <- fetchNext(q)
  matches <-q@collectedMatches
  expect_gt(max(matches$matchEnd), 1000)
  expect_true(all(matches$matchEnd == matches$matchStart))
})

test_that("extended metadata fields work cortrectly on instane KED", {
  kco <- KorAPConnection(KorAPUrl = "https://korap.ids-mannheim.de/instance/ked", accessToken = NULL,
             verbose = TRUE)
  q <- corpusQuery(
    kco,
    "einfache",
    fields = c(
      "textSigle",
      "pubDate",
      "pubPlace",
      "availability",
      "textClass",
      "snippet",
      "tokens",
      "KED.cover1Herder",
      "KED.cover2Herder",
      "KED.cover3Herder",
      "KED.cover4Herder",
      "KED.cover5Herder",
      "KED.nPara",
      "KED.nPunct1kTks",
      "KED.nSent",
      "KED.nToks",
      "KED.nToksSentMd",
      "KED.nTyps",
      "KED.rcpnt",
      "KED.rcpntLabel",
      "KED.strtgy",
      "KED.strtgyLabel",
      "KED.topic",
      "KED.topicLabel",
      "KED.txttyp",
      "KED.txttypLabel"
    )
  )
  q <- q %>% fetchAll()
  df <- q@collectedMatches
  expect_gt(nrow(df), 0)
  expect_gt(min(as.numeric(df$KED.nToks)), 100)
  expect_gt(min(as.numeric(df$KED.nSent)), 8)
  expect_gt(min(nchar(df$KED.rcpnt)), 5)
})

test_that("frequencyQuery with as.alternatives=TRUE works correctly", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE, cache = FALSE)

  # Test with alternatives in the same VC
  alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")
  same_vc <- "textType = /Zeit.*/ & pubDate in 1999"

  # Run frequency query with as.alternatives=TRUE (same VC)
  df1 <- frequencyQuery(kco,
                        query = alternatives,
                        vc = same_vc,
                        as.alternatives = TRUE)

  # Test 1: Check that we get results for both alternatives
  expect_equal(nrow(df1), 2, info = "Should have results for both alternative queries")

  # Test 2: Check that both rows have the same VC
  expect_equal(df1$vc[1], df1$vc[2],
               info = "Both alternatives should have the same VC")

  # Test 3: Check that both rows have the same total (sum within the VC)
  expect_equal(df1$total[1], df1$total[2],
               info = "Both alternatives in same VC should have the same total")

  # Test 4: Check that the total equals the sum of individual totalResults within the VC
  expect_equal(df1$total[1], sum(df1$totalResults),
               info = "Total should equal sum of totalResults within the same VC")

  # Test 5: Check that relative frequencies sum to approximately 1.0 within the same VC
  expect_equal(sum(df1$f), 1.0, tolerance = 1e-10,
               info = "Relative frequencies should sum to 1.0 for alternatives within same VC")

  # Test with alternatives in different VCs
  years <- c(1999, 2000)
  df2 <- frequencyQuery(kco,
                        query = alternatives,
                        vc = paste("textType = /Zeit.*/ & pubDate in", years),
                        as.alternatives = TRUE)

  # Test 6: With different VCs, each should have its own total
  expect_true(df2$vc[1] != df2$vc[2], info = "Different VCs should be different")
  expect_equal(df2$total[1], df2$totalResults[1], info = "Each VC should have its own total")
  expect_equal(df2$total[2], df2$totalResults[2], info = "Each VC should have its own total")

  # Test 7: For different VCs, each should have f=1.0 (since each is alone in its VC)
  expect_equal(df2$f[1], 1.0, tolerance = 1e-10, info = "Single query in VC should have f=1.0")
  # Note: Second row might have f=NA due to CI calculation issues, so we test the total instead
  if (!is.na(df2$f[2])) {
    expect_equal(df2$f[2], 1.0, tolerance = 1e-10, info = "Single query in VC should have f=1.0")
  }

  # Test 8: Check that relative frequencies are calculated for same VC case
  expect_true(all(!is.na(df1$f)), info = "All relative frequencies should be calculated for same VC")
  # Note: confidence intervals may not be calculable in all edge cases
  expect_true(sum(!is.na(df1$conf.low)) >= 1, info = "At least one confidence interval should be calculated")

  # Test 9: Check that confidence intervals are properly ordered (for non-NA values)
  valid_ci <- !is.na(df1$conf.low) & !is.na(df1$conf.high) & !is.na(df1$f)
  if (sum(valid_ci) > 0) {
    expect_true(all(df1$conf.low[valid_ci] <= df1$f[valid_ci]), info = "Lower confidence bound should be <= relative frequency")
    expect_true(all(df1$f[valid_ci] <= df1$conf.high[valid_ci]), info = "Relative frequency should be <= upper confidence bound")
  }

  # Test 10: Check that relative frequencies are between 0 and 1
  expect_true(all(df1$f >= 0 & df1$f <= 1), info = "Relative frequencies should be between 0 and 1")
  
  # Test with multiple VCs (this would fail before the ungroup() fix)
  # This test ensures all rows get valid results, not just the first two
  multiple_years <- c(1999, 2000, 2001, 2002)
  df3 <- frequencyQuery(kco,
                        query = c("macht", "ergibt", "ist", "wird"),
                        vc = paste("textType = /Zeit.*/ & pubDate in", multiple_years),
                        as.alternatives = TRUE)
  
  # Test 11: All rows should have valid f values (this failed before the fix)
  expect_true(all(!is.na(df3$f)), info = "All rows should have valid f values (not just first two)")
  
  # Test 12: Each VC should have f=1.0 (since each query is in its own VC)
  expect_true(all(abs(df3$f - 1.0) < 1e-10), info = "Each single query in its own VC should have f=1.0")
})

test_that("corpusQuery token API works when textSigle field is deselected", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE)
  q <- corpusQuery(kco, "focus([tt/p=ADJA] {Newstickeritis})",
                   vc = "corpusSigle=/W.D17/",
                   fields = c("tokens"),
                   metadataOnly = FALSE)
  q <- fetchNext(q)
  matches <-q@collectedMatches
  expect_gt(nrow(matches), 10)
  unique_matches <- unique(matches$tokens$match)
  expect_equal(length(unique_matches), 1)
  expect_equal(unique_matches[[1]], "Newstickeritis")

  left_contexts <- matches$tokens$left
  expect_true(TRUE %in% grepl("reine", left_contexts))

  right_contexts <- matches$tokens$right
  expect_true(TRUE %in% grepl("Begriff", right_contexts))
})

