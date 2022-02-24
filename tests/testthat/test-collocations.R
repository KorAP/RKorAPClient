test_that("collocationScoreQuery works", {
  skip_if_offline()
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  df <- collocationScoreQuery(kco, "Ameisenplage", "heimgesucht", leftContextSize=0, rightContextSize=1)
  expect_gt(df$logDice, 1)
  expect_equal(df$ll, ll(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$pmi, pmi(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi2, mi2(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi3, mi3(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$logDice, logDice(df$O1, df$O2, df$O, df$N, df$E, df$w))
})


test_that("collocationAnalysis works and warns about missing token", {
  skip_if_offline()
  kco <- new("KorAPConnection",
               accessToken = NULL,
               verbose = TRUE)
    expect_warning(
      df <-
        collocationAnalysis(
          kco,
          "focus([tt/p=ADJA] {Newstickeritis})",
          vc = "corpusSigle=/W.D17/",
          leftContextSize = 1,
          rightContextSize = 0,
          searchHitsSampleLimit = 100,
          topCollocatesLimit = 1,
          exactFrequencies = FALSE,
          maxRecurse = 2
        ),
      "access token"
    )
  expect_gt(df$O, df$E)
  expect_gt(df$logDice, 1)
})

test_that("collocationAnalysis on unaccounted strings does not error out", {
  skip_if_offline()
  kco <- new("KorAPConnection", accessToken = NULL, verbose = TRUE)
  expect_warning(
    df <- collocationAnalysis(kco, "XXXXXXXXAmeisenplage", vc=c("corpusSigle=/WDD17/", "corpusSigle=/WUD17/"), maxRecurse = 2),
    "access token"
  )
  testthat::expect_equal(nrow(df), 0)
})

test_that("temoveWithinSpanWorks", {
  expect_equal(
    removeWithinSpan("contains(<base/s=s>, (machen []{0,1} aufmerksam | aufmerksam []{0,1} machen))", "base/s=s"),
    "(machen []{0,1} aufmerksam | aufmerksam []{0,1} machen)")
})
