textFrequencyTestConnection <- function() {
  testthat::local_mocked_bindings(apiCall = function(...) NULL, .package = "RKorAPClient")
  suppressMessages(KorAPConnection(KorAPUrl = "https://example.invalid/", accessToken = NULL, verbose = FALSE, cache = FALSE))
}

# A server with the texts A/1/1 to A/1/3, the last one belonging to both corpora.
# Counts: 10 * text number for the query "x", otherwise 0
textFrequencyTestServer <- function(frame = parent.frame(), failing = "", texts = list(a = c("A/1/1", "A/1/3"), b = c("A/1/2", "A/1/3"))) {
  requests <- new.env()
  requests$urls <- character()
  testthat::local_mocked_bindings(
    apiCall = function(kco, url, ...) {
      requests$urls <- c(requests$urls, url)
      url <- urltools::url_decode(url)
      sigle <- stringr::str_match(url, 'textSigle = "([^"]+)"')[, 2]
      if (nzchar(failing) && grepl(failing, url, fixed = TRUE)) return(NULL)
      if (grepl("statistics?", url, fixed = TRUE)) {
        return(list(documents = 1, tokens = if (sigle == "A/1/2") 0 else 100, sentences = 1, paragraphs = 1))
      }
      if (is.na(sigle)) return(list(meta = list(totalResults = length(texts[[stringr::str_match(url, "cq=([ab])")[, 2]]]))))
      list(meta = list(totalResults = if (grepl("q=x", url, fixed = TRUE) && sigle != "A/1/2") 10 * as.numeric(substring(sigle, 5)) else 0))
    },
    fetchAll = function(kqo, ...) {
      sigles <- texts[[kqo@vc]]
      kqo@collectedMatches <- data.frame(textSigle = sigles, title = paste("Title", sigles), matchStart = 0)
      kqo
    }, .package = "RKorAPClient", .env = frame)
  requests
}

test_that("frequencies per text include texts without hits and texts of several corpora", {
  kco <- textFrequencyTestConnection()
  requests <- textFrequencyTestServer()
  result <- textFrequencyQuery(kco, c(X = "x", Y = "y"), vc = c(First = "a", Second = "b"), fields = "title")
  expect_equal(nrow(result), 8)
  expect_equal(names(result), c("query", "variant", "vc", "label", "textSigle", "title",
    "totalResults", "total", "f", "webUIRequestUrl"))
  expect_equal(result$textSigle, rep(c("A/1/1", "A/1/3", "A/1/2", "A/1/3"), 2))
  expect_equal(result$label, rep(c("First", "First", "Second", "Second"), 2))
  expect_equal(result$variant, rep(c("X", "Y"), each = 4))
  expect_equal(result$totalResults, c(10, 30, 0, 30, 0, 0, 0, 0))
  expect_equal(result$f, c(.1, .3, NA, .3, 0, 0, NA, 0))
  expect_match(urltools::url_decode(result$webUIRequestUrl[2]), 'q=x&cq=textSigle = "A/1/3"', fixed = TRUE)
  # two inventories, three sizes and two queries on three texts
  expect_length(requests$urls, 2 + 3 + 6)
  plain <- textFrequencyQuery(kco, "x", vc = "a", fields = "title")
  expect_false(any(c("variant", "label") %in% names(plain)))
  expect_equal(plain$totalResults, c(10, 30))
})

test_that("frequencies per text stop rather than returning zeros or sending too many requests", {
  kco <- textFrequencyTestConnection()
  requests <- textFrequencyTestServer()
  expect_error(textFrequencyQuery(kco, "x", vc = c("a", "b"), maxTexts = 3), "4 texts are more than maxTexts = 3")
  expect_length(requests$urls, 2)
  expect_error(textFrequencyQuery(kco, c("x", "x")), "distinct")
  expect_error(textFrequencyQuery(kco, ""), "non-empty")
  expect_error(textFrequencyQuery(kco, "x", vc = c("a", "a")), "distinct")
  expect_error(textFrequencyQuery(kco, "x", maxTexts = NA), "maxTexts")
  textFrequencyTestServer(failing = 'search?q=x&cq=textSigle = "A/1/3"')
  expect_error(textFrequencyQuery(kco, "x", vc = "a"), "requires complete requests")
  textFrequencyTestServer(failing = 'statistics?cq=textSigle = "A/1/3"')
  expect_error(textFrequencyQuery(kco, "x", vc = "a"), "token counts")
  textFrequencyTestServer(texts = list(a = c("A/1/1", "A/1/1")))
  expect_error(textFrequencyQuery(kco, "x", vc = "a"), "incomplete")
  textFrequencyTestServer(texts = list(a = character()))
  expect_error(textFrequencyQuery(kco, "x", vc = "a"), "No texts found")
})

test_that("frequencies per text can be kept in a file", {
  kco <- textFrequencyTestConnection()
  requests <- textFrequencyTestServer()
  file <- withr::local_tempfile(fileext = ".rds")
  first <- textFrequencyQuery(kco, "x", vc = "a", fields = "title", cacheAs = file)
  sent <- length(requests$urls)
  expect_equal(textFrequencyQuery(kco, "x", vc = "a", fields = "title", cacheAs = file), first, ignore_attr = TRUE)
  expect_length(requests$urls, sent)
})
