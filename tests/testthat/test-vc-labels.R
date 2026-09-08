test_that("vcLabels prefers the names a vector was given", {
  vcLabels <- RKorAPClient:::vcLabels

  expect_equal(
    vcLabels(c(before = "pubDate until 2009", since = "pubDate since 2010")),
    c("before", "since")
  )
  # nothing to prefer, so nothing is invented
  expect_null(vcLabels(c("pubDate until 2009", "pubDate since 2010")))
  # a partly named vector is filled in from the definitions
  expect_equal(
    vcLabels(c(before = "pubDate until 2009", "pubDate since 2010")),
    c("before", "since 2010")
  )
})

test_that("vcLabelsOrGuess always gives a label", {
  expect_equal(
    RKorAPClient:::vcLabelsOrGuess(c("pubDate until 2009", "pubDate since 2010")),
    RKorAPClient:::queryStringToLabel(c("pubDate until 2009", "pubDate since 2010"))
  )
})

test_that("the query functions label virtual corpora by their names", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  vcs <- c(before = "pubDate until 2009", since = "pubDate since 2010")

  expect_equal(frequencyQuery(kco, "Ameisenplage", vcs)$label, c("before", "since"))
  expect_equal(corpusStats(kco, vc = vcs, as.df = TRUE)$label, c("before", "since"))
  expect_equal(
    collocationScoreQuery(kco, "Grund", "triftiger", vc = vcs)$label,
    c("before", "since")
  )
})

test_that("an unnamed vector leaves the result as it was", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  vcs <- c("pubDate until 2009", "pubDate since 2010")

  # no column appears where there is nothing to put in it
  expect_false("label" %in% names(frequencyQuery(kco, "Ameisenplage", vcs)))
  expect_false("label" %in% names(corpusStats(kco, vc = vcs, as.df = TRUE)))
  # collocationScoreQuery has always labelled, and keeps deriving one
  expect_equal(
    collocationScoreQuery(kco, "Grund", "triftiger", vc = vcs)$label,
    RKorAPClient:::queryStringToLabel(vcs)
  )
})

test_that("corpusStats does not hide the labels in row names", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, verbose = FALSE)
  stats <- corpusStats(
    kco,
    vc = c(before = "pubDate until 2009", since = "pubDate since 2010"),
    as.df = TRUE
  )
  # row names are lost by the first bind_rows(), a column is not
  expect_equal(rownames(stats), c("1", "2"))
  expect_true("label" %in% names(stats))
})
