test_that("collocationScoreQuery works", {
  skip_if_offline()
  kco <- new("KorAPConnection", accessToken = NULL, cache = TRUE, verbose = TRUE)
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
          leftContextSize = 1,
          rightContextSize = 0,
        ),
      "access token"
    )
  expect_gt(df$O, df$E)
  expect_gt(df$logDice, -1)
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

test_that("removeWithinSpanWorks", {
  expect_equal(
    removeWithinSpan("contains(<base/s=s>, (machen []{0,1} aufmerksam | aufmerksam []{0,1} machen))", "base/s=s"),
    "(machen []{0,1} aufmerksam | aufmerksam []{0,1} machen)")
})


test_that("mergeDuplicateCollocatesWorksAsExpected", {
  ldf <- tibble(
    node = c("focus(in [tt/p=NN] {[tt/l=nehmen]})"),
    collocate = c("Anspruch"),
    label = c(""),
    vc = c(""),
    query = c("Anspruch focus(in [tt/p=NN] {[tt/l=nehmen]})"),
    webUIRequestUrl = c(
      "https://korap.ids-mannheim.de/?q=Anspruch%20focus%28in%20%5btt%2fp%3dNN%5d%20%7b%5btt%2fl%3dnehmen%5d%7d%29&ql=poliqarp"
    ),
    w = c(1),
    leftContextSize = c(1),
    rightContextSize = c(0),
    N = c(23578528381.5),
    O = c(0.5),
    O1 = c(1168410.5),
    O2 = c(1296870.5),
    E = c(64.2651265093014),
    pmi = c(11.9173498777957),
    mi2 = c(29.8406639214616),
    mi3 = c(47.7639779651274),
    logDice = c(11.6899933757298),
    ll = c(3717716.74208791)
  )
  rdf <- tibble(
    node = c("focus({[tt/l=nehmen] in} [tt/p=NN])"),
    collocate = c("Anspruch"),
    label = c(""),
    vc = c(""),
    query = c("focus({[tt/l=nehmen] in} [tt/p=NN]) Anspruch"),
    webUIRequestUrl = c(
      "https://korap.ids-mannheim.de/?q=focus%28%7b%5btt%2fl%3dnehmen%5d%20in%7d%20%5btt%2fp%3dNN%5d%29%20Anspruch&ql=poliqarp"
    ),
    w = c(1),
    leftContextSize = c(0),
    rightContextSize = c(1),
    N = c(23578528381.5),
    O = c(0.5),
    O1 = c(17077.5),
    O2 = c(1296870.5),
    E = c(0.939299756346416),
    pmi = c(7.99469408391783),
    mi2 = c(15.8990457079122),
    mi3 = c(23.8033973319065),
    logDice = c(2.57887487309409),
    ll = c(2181.35986032019)
  )
  merged <- mergeDuplicateCollocates(ldf, rdf, smoothingConstant = 0.5)
  expect_equal(merged$O, 0.5)
  expect_equal(merged$O1, 1185487.5)
  expect_equal(merged$O2, 1296870.5)
  expect_equal(merged$query, "Anspruch focus(in [tt/p=NN] {[tt/l=nehmen]}) | focus({[tt/l=nehmen] in} [tt/p=NN]) Anspruch")
})
