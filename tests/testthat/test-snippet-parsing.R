# Snippets come in several shapes, and matching them as a whole used to require
# one particular one, silently dropping the rest (issue #14).

snippetOf <- function(left, match, right) {
  paste0(
    '<span class="context-left">', left, '</span>',
    '<span class="match">', match, '</span>',
    '<span class="context-right">', right, '</span>'
  )
}

freqOf <- function(snippet, ...) {
  RKorAPClient:::snippet2FreqTable(
    snippet,
    oldTable = dplyr::tibble(word = character(0), frequency = numeric(0)),
    verbose = FALSE,
    ...
  )
}

test_that("the words of an ordinary snippet are counted", {
  result <- freqOf(snippetOf("der grosse alte Baum ", "<mark>steht</mark>", " im tiefen dunklen Wald"))
  expect_true(all(c("Baum", "Wald") %in% result$word))
})

test_that("a snippet cut at the sentence boundary is counted", {
  # <span class="cutted"> appears inside the match of contains(<base/s=s>, ...)
  # queries, which used to make the whole snippet be dropped
  cut <- snippetOf(
    'ein linker Kontext hier ',
    '<mark>Treffer</mark><span class="cutted"></span>',
    ' und rechts weiter'
  )
  result <- freqOf(cut)
  expect_true(all(c("Kontext", "rechts") %in% result$word))
})

test_that("a snippet with an empty context is counted on its other side", {
  # a match filling the whole sentence leaves one context span empty
  result <- freqOf(snippetOf("", "<mark>Treffer</mark>", " nur rechts etwas"))
  expect_true("rechts" %in% result$word)

  result <- freqOf(snippetOf("nur links etwas ", "<mark>Treffer</mark>", ""))
  expect_true("links" %in% result$word)
})

test_that("the more markers are not counted as words", {
  result <- freqOf(snippetOf(
    '<span class="more"></span>links davon ',
    "<mark>Treffer</mark>",
    ' rechts davon<span class="more"></span>'
  ))
  expect_true(all(c("links", "rechts") %in% result$word))
  expect_false(any(grepl("span|more|class", result$word)))
})

test_that("both contexts empty yields no words rather than an error", {
  expect_equal(nrow(freqOf(snippetOf("", "<mark>Treffer</mark>", ""))), 0)
})

test_that("a snippet without the expected spans is skipped", {
  expect_equal(nrow(freqOf("<span class=\"nothing\">kein KWIC</span>")), 0)
})

test_that("findExample survives a fetch that returned no snippet column", {
  # after a failed request collectedMatches has no snippet column at all, so
  # that the example was character(0) and the assignment aborted with
  # "replacement has length zero"
  kco <- methods::new(
    "KorAPConnection",
    apiUrl = "https://example.invalid/",
    KorAPUrl = "https://example.invalid/",
    authorizationSupported = FALSE,
    verbose = FALSE
  )

  testthat::local_mocked_bindings(
    corpusQuery = function(...) methods::new("KorAPQuery", korapConnection = kco, totalResults = 1),
    fetchNext = function(q, ...) q,
    .package = "RKorAPClient"
  )

  expect_equal(RKorAPClient:::findExample(kco, query = "irgendwas"), "")
})
