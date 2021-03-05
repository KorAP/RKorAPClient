test_that("Alternatives over time highcharter example works", {
  year <- c(2013:2018)
  alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")
  hc <- new("KorAPConnection", verbose = TRUE) %>%
    frequencyQuery(
      query = alternatives,
      vc = paste("textType = /Zeit.*/ & pubDate in", year),
      as.alternatives = TRUE
    ) %>%
    hc_freq_by_year_ci(as.alternatives = TRUE)
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Multiple queries over time highcharter example works", {
  year <- c(2013:2018)
  alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")

  hc <- new("KorAPConnection", verbose = TRUE) %>%
    frequencyQuery(
      query = alternatives,
      vc = paste("textType = /Zeit.*/ & pubDate in", year),
      as.alternatives = FALSE
    ) %>%
    hc_freq_by_year_ci(as.alternatives = FALSE)
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Single query in multiple over time highcharter example works", {
  year <- c(2013:2018)
  alternatives <- c("macht []{0,3} Sinn", "ergibt []{0,3} Sinn")

  hc <- new("KorAPConnection", verbose = TRUE) %>%
    frequencyQuery(
      query = alternatives,
      vc = paste("textType = /Zeit.*/ & pubDate in", year),
      as.alternatives = FALSE
    ) %>%
    hc_freq_by_year_ci(as.alternatives = FALSE)
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Single query over time highcharter example works", {
  year <- c(2013:2018)
  q <- c("macht []{0,3} Sinn")

  hc <- new("KorAPConnection", verbose = TRUE) %>%
    frequencyQuery(
      query = q,
      vc = paste("textType = /Zeit.*/ & pubDate in", year),
      as.alternatives = FALSE
    ) %>%
    hc_freq_by_year_ci(as.alternatives = FALSE)
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Auto conditions over time highcharter example works", {
  kco <- new("KorAPConnection", verbose=TRUE)
  hc <- expand_grid(
    myconditions = c("textDomain = /Wirtschaft.*/",
                  "textDomain != /Wirtschaft.*/"),
    year = (2011:2013)
  ) %>%
    cbind(frequencyQuery(
      kco,
      c("[tt/l=Heuschrecke]", "Ameise"),
      paste(.$myconditions, "& pubDate in", .$year)
    ))  %>%
    hc_freq_by_year_ci()
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Single condition over time highcharter example works", {
  kco <- new("KorAPConnection", verbose=TRUE)
  hc <- expand_grid(
    condition = c("textDomain = /Wirtschaft.*/"),
    year = (2011:2013)
  ) %>%
    cbind(frequencyQuery(
      kco,
      c("[tt/l=Heuschrecke]", "Ameise"),
      paste(.$condition, "& pubDate in", .$year),
    ))  %>%
    hc_freq_by_year_ci()
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Multiple conditions over time highcharter example works", {
  kco <- new("KorAPConnection", verbose=TRUE)
  hc <- expand_grid(
    condition = c("textDomain = /Wirtschaft.*/",
                  "textDomain != /Wirtschaft.*/"),
    year = (2011:2013)
  ) %>%
    cbind(frequencyQuery(
      kco,
      c("[tt/l=Heuschrecke]", "Ameise"),
      paste(.$condition, "& pubDate in", .$year),
    ))  %>%
    hc_freq_by_year_ci()
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("Multiple conditions and queries over time highcharter example works", {
  kco <- new("KorAPConnection", verbose=TRUE)
  hc <- expand_grid(
    qx = c("[tt/l=Heuschrecke]", "Ameise"),
    condition = c("textDomain = /Wirtschaft.*/",
                  "textDomain != /Wirtschaft.*/"),
    year = (2011:2013)
  ) %>%
    cbind(frequencyQuery(
      kco,
      .$qx,
      paste(.$condition, "& pubDate in", .$year),
    ))  %>%
    hc_freq_by_year_ci()
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})

test_that("collocationScoreQuery works iwth hchart and hc_add_onclick_korap_search", {
  kco <- new("KorAPConnection", cache = TRUE, verbose = TRUE)
  df <- collocationScoreQuery(kco,"Ameisenplage", "heimgesucht", leftContextSize=0, rightContextSize=1)
  hc <- hchart(df, type="spline", hcaes(label, logDice))
  hc <- hc_add_onclick_korap_search(hc)
  expect_true(all(class(hc) %in% c("highchart", "htmlwidget")))
})
