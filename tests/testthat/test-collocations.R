test_that("collocationScoreQuery works", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = TRUE)
  df <- collocationScoreQuery(kco, "Ameisenplage", "heimgesucht", leftContextSize = 0, rightContextSize = 1)
  expect_gt(df$logDice, 1)
  expect_equal(df$ll, ll(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$pmi, pmi(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi2, mi2(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$mi3, mi3(df$O1, df$O2, df$O, df$N, df$E, df$w))
  expect_equal(df$logDice, logDice(df$O1, df$O2, df$O, df$N, df$E, df$w))
})

test_that("collocationScoreQuery expands collocates and virtual corpora", {
  kco <- methods::new(
    "KorAPConnection",
    apiUrl = "https://example.test/",
    KorAPUrl = "https://example.test/"
  )

  fake_frequency_query <- function(kco, query, vc = "", ...) {
    expand <- length(query) != length(vc)
    combinations <- if (expand) {
      tidyr::expand_grid(query = query, vc = vc)
    } else {
      tibble::tibble(query = query, vc = vc)
    }

    combinations |>
      dplyr::mutate(
        totalResults = 10,
        webUIRequestUrl = paste0("https://example.test/", seq_len(dplyr::n())),
        total = 1000
      )
  }

  testthat::local_mocked_bindings(
    frequencyQuery = fake_frequency_query,
    .package = "RKorAPClient"
  )

  result <- collocationScoreQuery(
    kco,
    node = "node",
    collocate = c("first", "second"),
    vc = c("vc1", "vc2"),
    leftContextSize = 0,
    rightContextSize = 1
  )

  expect_equal(nrow(result), 4)
  expect_equal(result$collocate, c("first", "first", "second", "second"))
  expect_equal(result$vc, c("vc1", "vc2", "vc1", "vc2"))
  expect_true(all(mapply(
    grepl,
    pattern = result$collocate,
    x = result$query,
    MoreArgs = list(fixed = TRUE)
  )))
})

test_that("collocationScoreQuery aligns observed frequencies with collocates", {
  kco <- methods::new(
    "KorAPConnection",
    apiUrl = "https://example.test/",
    KorAPUrl = "https://example.test/"
  )

  fake_frequency_query <- function(kco, query, vc = "", ...) {
    expand <- length(query) != length(vc)
    combinations <- if (expand) {
      tidyr::expand_grid(query = query, vc = vc)
    } else {
      tibble::tibble(query = query, vc = vc)
    }

    combinations |>
      dplyr::mutate(
        totalResults = 10,
        webUIRequestUrl = "https://example.test/",
        total = 1000
      )
  }

  testthat::local_mocked_bindings(
    frequencyQuery = fake_frequency_query,
    .package = "RKorAPClient"
  )

  result <- collocationScoreQuery(
    kco,
    node = "node",
    collocate = c("first", "second"),
    vc = c("vc1", "vc2"),
    observed = c(3, 7),
    smoothingConstant = 0
  )

  expect_equal(result$O, c(3, 3, 7, 7))
})


test_that("collocationAnalysis works and warns about missing token", {
  skip_if_offline()
  kco <- KorAPConnection(
    accessToken = NULL,
    verbose = TRUE
  )
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
  kco <- KorAPConnection(accessToken = NULL, verbose = TRUE)
  expect_warning(
    df <- collocationAnalysis(kco, "XXXXXXXXAmeisenplage", vc = c("corpusSigle=/WDD17/", "corpusSigle=/WUD17/"), maxRecurse = 2),
    "access token"
  )
  testthat::expect_equal(nrow(df), 0)
})

# test_that("removeWithinSpanWorks", {
#  expect_equal(
#    removeWithinSpan("contains(<base/s=s>, (machen []{0,1} aufmerksam | aufmerksam []{0,1} machen))", "base/s=s"),
#    "(machen []{0,1} aufmerksam | aufmerksam []{0,1} machen)")
# })


test_that("mergeDuplicateCollocatesWorksAsExpected", {
  ldf <- tibble::tibble(
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
  rdf <- tibble::tibble(
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

test_that("add_multi_vc_comparisons adds favorite columns", {
  sample_result <- tibble::tibble(
    node = c("n", "n"),
    collocate = c("c", "c"),
    vc = c("vc1", "vc2"),
    label = c("A", "B"),
    N = c(100, 100),
    O = c(10, 20),
    O1 = c(50, 50),
    O2 = c(30, 30),
    E = c(5, 5),
    w = c(2, 2),
    leftContextSize = c(1, 1),
    rightContextSize = c(1, 1),
    frequency = c(10, 20),
    webUIRequestUrl = c("https://korap.example/A", "https://korap.example/B"),
    logDice = c(5, 7),
    pmi = c(2, 3)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)

  expect_true(all(c(
    "winner_logDice",
    "winner_logDice_value",
    "winner_logDice_webUIRequestUrl",
    "winner_webUIRequestUrl",
    "runner_up_logDice",
    "runner_up_logDice_value",
    "loser_logDice_webUIRequestUrl",
    "loser_webUIRequestUrl",
    "max_delta_logDice",
    "winner_rank_logDice",
    "winner_rank_logDice_value",
    "winner_rank_logDice_webUIRequestUrl",
    "runner_up_rank_logDice",
    "runner_up_rank_logDice_value",
    "loser_rank_logDice",
    "loser_rank_logDice_value",
    "loser_rank_logDice_webUIRequestUrl",
    "max_delta_rank_logDice",
    "winner_percentile_rank_logDice",
    "winner_percentile_rank_logDice_value",
    "winner_percentile_rank_logDice_webUIRequestUrl",
    "runner_up_percentile_rank_logDice",
    "runner_up_percentile_rank_logDice_value",
    "loser_percentile_rank_logDice",
    "loser_percentile_rank_logDice_value",
    "loser_percentile_rank_logDice_webUIRequestUrl",
    "max_delta_percentile_rank_logDice",
    "winner_rank_pmi",
    "winner_rank_pmi_value",
    "runner_up_rank_pmi",
    "runner_up_rank_pmi_value",
    "loser_rank_pmi",
    "loser_rank_pmi_value",
    "max_delta_rank_pmi",
    "winner_percentile_rank_pmi",
    "winner_percentile_rank_pmi_value",
    "runner_up_percentile_rank_pmi",
    "runner_up_percentile_rank_pmi_value",
    "loser_percentile_rank_pmi",
    "loser_percentile_rank_pmi_value",
    "max_delta_percentile_rank_pmi",
    "rank_A_logDice",
    "rank_B_logDice",
    "rank_A_pmi",
    "rank_B_pmi",
    "percentile_rank_A_logDice",
    "percentile_rank_B_logDice",
    "percentile_rank_A_pmi",
    "percentile_rank_B_pmi",
    "delta_rank_logDice",
    "delta_rank_pmi",
    "delta_percentile_rank_logDice",
    "delta_percentile_rank_pmi"
  ) %in% colnames(enriched)))

  expect_true(all(enriched$winner_logDice == "B"))
  expect_true(all(enriched$runner_up_logDice == "A"))
  expect_true(all(enriched$winner_logDice_value >= enriched$runner_up_logDice_value))
  expect_true(all(enriched$winner_logDice_webUIRequestUrl == "https://korap.example/B"))
  expect_true(all(enriched$loser_logDice_webUIRequestUrl == "https://korap.example/A"))
  expect_true(all(enriched$winner_webUIRequestUrl == "https://korap.example/B"))
  expect_true(all(enriched$loser_webUIRequestUrl == "https://korap.example/A"))
  expect_true(all(enriched$percentile_rank_A_logDice == 1))
  expect_true(all(enriched$percentile_rank_B_logDice == 1))
  expect_true(all(enriched$delta_percentile_rank_logDice == 0))
})

test_that("add_multi_vc_comparisons fills missing label URLs from vc", {
  sample_result <- tibble::tibble(
    node = c("n", "n", "n"),
    collocate = c("c1", "c2", "c2"),
    vc = c("corpusSigle=/A/", "corpusSigle=/A/", "corpusSigle=/B/"),
    label = c("A", "A", "B"),
    N = c(100, 100, 100),
    O = c(10, 10, 20),
    O1 = c(50, 50, 50),
    O2 = c(30, 30, 30),
    E = c(5, 5, 5),
    w = c(2, 2, 2),
    leftContextSize = c(1, 1, 1),
    rightContextSize = c(1, 1, 1),
    frequency = c(10, 10, 20),
    webUIRequestUrl = c(
      "https://korap.example/?q=q1&cq=corpusSigle%3D%2FA%2F&ql=poliqarp",
      "https://korap.example/?q=q2&cq=corpusSigle%3D%2FA%2F&ql=poliqarp",
      "https://korap.example/?q=q2&cq=corpusSigle%3D%2FB%2F&ql=poliqarp"
    ),
    logDice = c(6, 5, 7),
    pmi = c(3, 2, 4)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)
  c1 <- enriched[enriched$collocate == "c1", ]
  expected_b_url <- paste0(
    "https://korap.example/?q=q1&cq=",
    urltools::url_encode("corpusSigle=/B/"),
    "&ql=poliqarp"
  )

  expect_equal(
    unique(c1$loser_logDice_webUIRequestUrl),
    expected_b_url
  )
  expect_equal(
    unique(c1$loser_webUIRequestUrl),
    expected_b_url
  )
})

test_that("add_multi_vc_comparisons flags imputed cells", {
  sample_result <- tibble::tibble(
    node = c("n", "n", "n"),
    collocate = c("c1", "c2", "c2"),
    vc = c("corpusSigle=/A/", "corpusSigle=/A/", "corpusSigle=/B/"),
    label = c("A", "A", "B"),
    N = c(100, 100, 100),
    O = c(10, 10, 20),
    O1 = c(50, 50, 50),
    O2 = c(30, 30, 30),
    E = c(5, 5, 5),
    w = c(2, 2, 2),
    leftContextSize = c(1, 1, 1),
    rightContextSize = c(1, 1, 1),
    frequency = c(10, 10, 20),
    webUIRequestUrl = c(
      "https://korap.example/A1",
      "https://korap.example/A2",
      "https://korap.example/B2"
    ),
    logDice = c(6, 5, 7),
    pmi = c(3, 2, 4)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)

  expect_true(all(c("imputed_A", "imputed_B", "n_imputed", "imputed") %in% colnames(enriched)))

  c1 <- enriched[enriched$collocate == "c1", ]
  c2 <- enriched[enriched$collocate == "c2", ]

  # c1 only occurs in A, so B's scores had to be imputed
  expect_true(all(c1$imputed_B))
  expect_true(all(!c1$imputed_A))
  expect_true(all(c1$imputed))
  expect_true(all(c1$n_imputed == 1L))

  # c2 occurs in both, so nothing is imputed
  expect_true(all(!c2$imputed_A))
  expect_true(all(!c2$imputed_B))
  expect_true(all(!c2$imputed))
  expect_true(all(c2$n_imputed == 0L))

  # c1's delta is measured against the imputed floor rather than against observed data:
  # the absent label loses, at or below the weakest score actually attested anywhere.
  expect_true(all(is.finite(c1$max_delta_logDice)))
  expect_equal(unique(c1$loser_logDice), "B")
  expect_lte(unique(c1$loser_logDice_value), min(sample_result$logDice))
})

test_that("add_multi_vc_comparisons reports no imputation when all labels are complete", {
  sample_result <- tibble::tibble(
    node = rep("n", 4),
    collocate = c("c1", "c1", "c2", "c2"),
    vc = rep(c("vc1", "vc2"), 2),
    label = rep(c("A", "B"), 2),
    N = rep(100, 4),
    O = c(10, 20, 30, 40),
    O1 = rep(50, 4),
    O2 = rep(30, 4),
    E = rep(5, 4),
    w = rep(2, 4),
    leftContextSize = rep(1, 4),
    rightContextSize = rep(1, 4),
    frequency = c(10, 20, 30, 40),
    logDice = c(5, 7, 6, 4),
    pmi = c(2, 3, 4, 1)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)

  expect_true(all(!enriched$imputed))
  expect_true(all(enriched$n_imputed == 0L))
})

test_that("add_multi_vc_comparisons handles more than two labels", {
  sample_result <- tibble::tibble(
    node = rep("n", 3),
    collocate = rep("c", 3),
    vc = c("vc1", "vc2", "vc3"),
    label = c("A", "B", "C"),
    N = rep(100, 3),
    O = c(10, 30, 5),
    O1 = rep(50, 3),
    O2 = rep(30, 3),
    E = rep(5, 3),
    w = rep(2, 3),
    leftContextSize = rep(1, 3),
    rightContextSize = rep(1, 3),
    frequency = c(10, 30, 5),
    webUIRequestUrl = c("https://korap.example/A", "https://korap.example/B", "https://korap.example/C"),
    logDice = c(5, 8, 4),
    pmi = c(2, 3, 1)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)
  expect_equal(enriched$winner_logDice[1], "B")
  expect_equal(enriched$winner_logDice_value[1], 8)
  expect_equal(enriched$winner_logDice_webUIRequestUrl[1], "https://korap.example/B")
  expect_equal(enriched$runner_up_logDice[1], "A")
  expect_equal(enriched$runner_up_logDice_value[1], 5)
  expect_equal(enriched$loser_logDice[1], "C")
  expect_equal(enriched$loser_logDice_value[1], 4)
  expect_equal(enriched$loser_logDice_webUIRequestUrl[1], "https://korap.example/C")
  expect_equal(enriched$max_delta_logDice[1], 4)
})

test_that("add_multi_vc_comparisons leaves unsuffixed URLs ambiguous when scores disagree", {
  sample_result <- tibble::tibble(
    node = c("n", "n"),
    collocate = c("c", "c"),
    vc = c("vc1", "vc2"),
    label = c("A", "B"),
    N = c(100, 100),
    O = c(10, 20),
    O1 = c(50, 50),
    O2 = c(30, 30),
    E = c(5, 5),
    w = c(2, 2),
    leftContextSize = c(1, 1),
    rightContextSize = c(1, 1),
    frequency = c(10, 20),
    webUIRequestUrl = c("https://korap.example/A", "https://korap.example/B"),
    logDice = c(5, 7),
    pmi = c(4, 3)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)

  expect_equal(enriched$winner_logDice_webUIRequestUrl[1], "https://korap.example/B")
  expect_equal(enriched$winner_pmi_webUIRequestUrl[1], "https://korap.example/A")
  expect_true(is.na(enriched$winner_webUIRequestUrl[1]))
  expect_true(is.na(enriched$loser_webUIRequestUrl[1]))
})

test_that("add_multi_vc_comparisons computes rank deltas", {
  base_tbl <- tidyr::expand_grid(
    label = c("A", "B", "C"),
    collocate = c("c1", "c2", "c3")
  ) |>
    dplyr::mutate(
      node = "n",
      vc = paste0("vc", label),
      N = 100,
      O = 10,
      O1 = 50,
      O2 = 40,
      E = 5,
      w = 2,
      leftContextSize = 1,
      rightContextSize = 1,
      frequency = 10,
      logDice = dplyr::case_when(
        label == "A" & collocate == "c1" ~ 9,
        label == "A" & collocate == "c2" ~ 6,
        label == "A" & collocate == "c3" ~ 3,
        label == "B" & collocate == "c1" ~ 7,
        label == "B" & collocate == "c2" ~ 9,
        label == "B" & collocate == "c3" ~ 5,
        label == "C" & collocate == "c1" ~ 4,
        label == "C" & collocate == "c2" ~ 6,
        label == "C" & collocate == "c3" ~ 8,
        TRUE ~ 0
      )
    )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(base_tbl)
  target_row <- enriched |>
    dplyr::filter(collocate == "c1") |>
    dplyr::slice_head(n = 1)

  expect_equal(target_row$rank_A_logDice, 1)
  expect_equal(target_row$rank_B_logDice, 2)
  expect_equal(target_row$rank_C_logDice, 3)
  expect_equal(target_row$winner_rank_logDice, "A")
  expect_equal(target_row$winner_rank_logDice_value, 1)
  expect_equal(target_row$runner_up_rank_logDice, "B")
  expect_equal(target_row$runner_up_rank_logDice_value, 2)
  expect_equal(target_row$loser_rank_logDice, "C")
  expect_equal(target_row$loser_rank_logDice_value, 3)
  expect_equal(target_row$max_delta_rank_logDice, 2)
})

test_that("add_multi_vc_comparisons imputes missing ranks for max delta", {
  sample_result <- tibble::tibble(
    node = c("n", "n"),
    collocate = c("c", "c"),
    vc = c("vc1", "vc2"),
    label = c("A", "B"),
    N = c(100, 100),
    O = c(10, 10),
    O1 = c(50, 50),
    O2 = c(30, 30),
    E = c(5, 5),
    w = c(2, 2),
    leftContextSize = c(1, 1),
    rightContextSize = c(1, 1),
    frequency = c(10, 10),
    logDice = c(5, NA)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(sample_result)

  expect_equal(enriched$rank_A_logDice[1], 1)
  expect_true(is.na(enriched$rank_B_logDice[1]))
  expect_equal(enriched$winner_rank_logDice[1], "A")
  expect_equal(enriched$loser_rank_logDice[1], "B")
  expect_equal(enriched$loser_rank_logDice_value[1], 2)
  expect_equal(enriched$max_delta_rank_logDice[1], 1)
})

test_that("adaptive missing score imputation respects measure-specific scales", {
  sample_result <- tibble::tibble(
    node = c("n", "n", "n"),
    collocate = c("c", "c", "c"),
    vc = c("vc1", "vc2", "vc3"),
    label = c("A", "B", "C"),
    N = c(100, 100, 100),
    O = c(12, 9, 7),
    O1 = c(60, 40, 30),
    O2 = c(33, 22, 18),
    E = c(6, 6, 6),
    w = c(2, 2, 2),
    leftContextSize = c(1, 1, 1),
    rightContextSize = c(1, 1, 1),
    frequency = c(15, 11, 9),
    logDice = c(-0.31, NA, -0.12),
    pmi = c(-1.65, NA, -0.48),
    ll = c(12.4, NA, 7.9)
  )

  enriched <- RKorAPClient:::add_multi_vc_comparisons(
    sample_result,
    missingScoreQuantile = 0.05
  )

  row_a <- dplyr::filter(enriched, label == "A") |> dplyr::slice_head(n = 1)

  expect_false(is.na(row_a$logDice_B))
  expect_false(is.na(row_a$pmi_B))
  expect_false(is.na(row_a$ll_B))

  expect_lt(row_a$logDice_B, min(sample_result$logDice, na.rm = TRUE))
  expect_lt(row_a$pmi_B, min(sample_result$pmi, na.rm = TRUE))
  expect_lte(row_a$ll_B, min(sample_result$ll, na.rm = TRUE))

  expect_gt(row_a$max_delta_logDice, 0)
  expect_gt(row_a$winner_logDice_value - row_a$loser_logDice_value, 0)
})

# New tests for improved coverage of collocationAnalysis.R helper functions

test_that("synsemanticStopwords returns German stopwords", {
  stopwords <- synsemanticStopwords()
  expect_true(is.character(stopwords))
  expect_true(length(stopwords) > 50)
  expect_true("der" %in% stopwords)
  expect_true("die" %in% stopwords)
  expect_true("und" %in% stopwords)
  expect_true("mit" %in% stopwords)
})

test_that("removeWithinSpan removes span constraints correctly", {
  # Test basic span removal
  query1 <- "contains(<base/s=s>, (machen []{0,1} aufmerksam | aufmerksam []{0,1} machen))"
  result1 <- RKorAPClient:::removeWithinSpan(query1, "base/s=s")
  expect_equal(result1, "(machen []{0,1} aufmerksam | aufmerksam []{0,1} machen)")

  # Test with different span
  query2 <- "contains(<p/s=s>, (test query))"
  result2 <- RKorAPClient:::removeWithinSpan(query2, "p/s=s")
  expect_equal(result2, "(test query)")

  # Test with empty span - should return original query
  query3 <- "simple query"
  result3 <- RKorAPClient:::removeWithinSpan(query3, "")
  expect_equal(result3, query3)

  # Test with non-matching span
  query4 <- "contains(<base/s=s>, test)"
  result4 <- RKorAPClient:::removeWithinSpan(query4, "other/span")
  expect_equal(result4, query4)
})

test_that("matches2FreqTable handles empty matches", {
  empty_matches <- data.frame()
  result <- RKorAPClient:::matches2FreqTable(empty_matches, index = 0)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("matches2FreqTable processes single match correctly", {
  # Create mock matches data
  mock_matches <- data.frame(
    tokens = I(list(list(
      left = c("der", "große"),
      match = "Test",
      right = c("ist", "wichtig")
    ))),
    stringsAsFactors = FALSE
  )

  result <- RKorAPClient:::matches2FreqTable(
    mock_matches,
    index = 1,
    leftContextSize = 2,
    rightContextSize = 2,
    stopwords = c("der", "ist") # Provide stopwords to avoid empty join
  )

  expect_true(is.data.frame(result))
})

test_that("snippet2FreqTable handles empty snippet", {
  result <- RKorAPClient:::snippet2FreqTable(character(0))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("snippet2FreqTable processes single snippet correctly", {
  snippet <- '<span class="context-left">der große </span><span class="match"><mark>Test</mark></span><span class="context-right"> ist wichtig</span>'

  result <- RKorAPClient:::snippet2FreqTable(
    snippet,
    leftContextSize = 2,
    rightContextSize = 2,
    stopwords = c("der"), # Provide stopwords to avoid empty join
    verbose = FALSE
  )

  expect_true(is.data.frame(result))
})

test_that("inject_focus_into_query adds focus wrappers when span queries lack them", {
  unfocused <- "contains(<base/s=s>, (Anspruch [tt/l=nehmen] | [tt/l=nehmen] Anspruch))"
  focused <- RKorAPClient:::inject_focus_into_query(unfocused)

  expect_equal(
    focused,
    "contains(<base/s=s>, (focus({Anspruch [tt/l=nehmen]}) | focus({[tt/l=nehmen] Anspruch})))"
  )
})

test_that("inject_focus_into_query leaves existing focus segments unchanged", {
  already_focused <- "contains(<base/s=s>, (focus({Anspruch [tt/l=nehmen]})))"
  expect_identical(
    RKorAPClient:::inject_focus_into_query(already_focused),
    already_focused
  )
})

# Removed hanging findExample tests as they cause infinite wait
# These tests make API calls that don't complete properly

# Removed hanging collocatesQuery tests as they cause infinite wait
# These tests were causing the test suite to hang and not terminate

test_that("collocationAnalysis handles exactFrequencies parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "Test",
      exactFrequencies = TRUE,
      searchHitsSampleLimit = 5,
      topCollocatesLimit = 5
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})

test_that("collocationAnalysis handles withinSpan parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "Test",
      withinSpan = "base/s=s",
      exactFrequencies = TRUE,
      searchHitsSampleLimit = 5,
      topCollocatesLimit = 5
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})

test_that("collocationAnalysis handles expand parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      c("Test", "der"),
      expand = TRUE,
      searchHitsSampleLimit = 2,
      topCollocatesLimit = 2
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})

test_that("collocationAnalysis honors named vc labels", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  named_vc <- c(
    Western = "textType=/.*Western.*/ & pubDate in 2012",
    Erotic = "textType=/.*(Erotik|Gay).*/ & pubDate in 2012",
    Historic = "textType=/.*Historisch.*/ & pubDate in 2012"
  )

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "[tt/l=treffen]",
      vc = named_vc,
      searchHitsSampleLimit = 2,
      topCollocatesLimit = 2
    ),
    "access token"
  )

  if (nrow(result) > 0) {
    expect_true("label" %in% colnames(result))
    expect_setequal(unique(result$label), names(named_vc))
  }
})

test_that("collocationAnalysis handles stopwords parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "Test",
      stopwords = c("der", "die", "und"),
      searchHitsSampleLimit = 5,
      topCollocatesLimit = 5
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})

test_that("collocationAnalysis handles lemmatizeNodeQuery parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "laufen",
      lemmatizeNodeQuery = TRUE,
      searchHitsSampleLimit = 5,
      topCollocatesLimit = 5
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})

test_that("collocationAnalysis handles addExamples parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "Test",
      addExamples = TRUE,
      searchHitsSampleLimit = 3,
      topCollocatesLimit = 3
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true("example" %in% colnames(result))
  }
})

test_that("collocationAnalysis handles maxRecurse parameter", {
  skip_if_offline()
  kco <- KorAPConnection(accessToken = NULL, cache = TRUE, verbose = FALSE)

  expect_warning(
    result <- collocationAnalysis(
      kco,
      "Test",
      maxRecurse = 1,
      searchHitsSampleLimit = 2,
      topCollocatesLimit = 2
    ),
    "access token"
  )
  expect_true(is.data.frame(result))
})
