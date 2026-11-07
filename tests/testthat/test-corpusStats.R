test_that("corpusStats parses subcorpus sizes and encodes its request offline", {
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    vc <- "pubDate since 2020 & pubDate until 2021"
    stats <- corpusStats(kco, vc)
    expect_s4_class(stats, "KorAPCorpusStats")
    expect_equal(c(stats@documents, stats@tokens, stats@sentences, stats@paragraphs),
      c(100, 25000, 1200, 400))
    expect_identical(stats@vc, vc)
    expect_identical(httr2::url_parse(tail(state$requests, 1))$query$cq, vc)
    expect_match(stats@webUIRequestUrl, "%26", fixed = TRUE)
    expect_identical(httr2::url_parse(stats@webUIRequestUrl)$query$cq, vc)
  })
})

test_that("corpusStats returns the same parsed sizes as a data frame offline", {
  withKorapFixtureApi(function(state) {
    kco <- suppressMessages(fixtureKorapConnection())
    vc <- "pubDate since 2020 & pubDate until 2021"
    stats <- corpusStats(kco, vc, as.df = TRUE)
    expect_s3_class(stats, "data.frame")
    expect_equal(nrow(stats), 1)
    expect_equal(unlist(stats[c("documents", "tokens", "sentences", "paragraphs")],
      use.names = FALSE), c(100, 25000, 1200, 400))
    expect_identical(stats$vc, vc)
    expect_match(stats$webUIRequestUrl, "%26", fixed = TRUE)
  })
})

test_that("printing corpusStats for the whole corpus works offline", {
  withKorapFixtureApi(function(state) {
    stats <- corpusStats(suppressMessages(fixtureKorapConnection()))
    expect_equal(stats@tokens, 250000)
    expect_identical(httr2::url_parse(tail(state$requests, 1))$query$cq, "")
    expect_output(print(stats), "The whole corpus.*250,000 tokens")
  })
})

test_that("printing corpusStats for a subcorpus works offline", {
  withKorapFixtureApi(function(state) {
    stats <- corpusStats(suppressMessages(fixtureKorapConnection()),
      "pubDate since 2020 & pubDate until 2021")
    expect_output(print(stats), "The virtual corpus described by")
    expect_output(print(stats), "25,000 tokens")
  })
})
