test_that("collocationScoreQuery works", {
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  df <- collocationScoreQuery(kco, "Ameisenplage", "heimgesucht", leftContextSize=0, rightContextSize=1)
  expect_gt(df$logDice, 1)
  expect_equal(df$ll, ll(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$pmi, pmi(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi2, mi2(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi3, mi3(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$logDice, logDice(df$O1, df$O2, df$O, df$N, df$E, df$w))
})

test_that("collocationAnalysis works", {
  kco <- new("KorAPConnection",
             accessToken = NULL,
             verbose = TRUE)
  df <-
    collocationAnalysis(
      kco,
      "focus([tt/p=ADJA] {Newstickeritis})",
      vc = "corpusSigle=/W.D17/",
      leftContextSize = 1,
      rightContextSize = 0,
      searchHitsSampleLimit = 100,
      topCollocatesLimit = 1,
      exactFrequencies = FALSE
    )
  expect_gt(df$O, df$E)
  expect_gt(df$logDice, 1)
})

test_that("collocationAnalysis on unaccounted strings does not error out", {
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  df <- collocationAnalysis(kco, "XXXXXXXXAmeisenplage")
  testthat::expect_equal(nrow(df), 0)
})
