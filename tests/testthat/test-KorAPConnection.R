test_that("KorAPConnection fails gracefully on unresolvable host", {
  expect_message(KorAPConnection(accessToken = NULL, apiUrl="http://xxx.asdhsahdsadhvgas.org"), "No internet|Could not resolve")
})

test_that("KorAPConnection fails gracefully on timeout", {
  expect_message(KorAPConnection(apiUrl="http://httpbin.org/delay/3", accessToken = NULL, timeout = 0.2), "No internet|Timeout|json|progress|Unavailable")
})

test_that("KorAPConnection fails gracefully on Bad Gateway errors", {
  expect_message(KorAPConnection(apiUrl="http://httpbin.org/status/502", accessToken = NULL, timeout = 0.5), "No internet|Timeout|progress|json|502")
})

test_that("KorAPConnection is printable", {
  kco <- KorAPConnection(accessToken = NULL, timeout = 1)
  skip_if(is.null(kco@welcome))
  expect_error(print(kco), NA)
})

test_that("Opening KorAPConnection prints some message.", {
  expect_message(KorAPConnection(accessToken = NULL), "KorAP")
})

test_that("Opening KorAPConnection with invalid apiToken fails gracefully", {
  expect_message(KorAPConnection(accessToken="test token", timeout = 3),
               "401|Timeout|progress")
})

test_that("Persisting null apiToken fails", {
  kco <- KorAPConnection(accessToken = NULL, timeout = 3)
  skip_if_not(is.null(kco@accessToken))
  skip_if(is.null(kco@welcome))
  expect_error(persistAccessToken(kco),
               ".*not supplied any access token.*",
               perl = TRUE)
})

test_that("Opening KorAPConnection with KorAPUrl works", {
  kco <- KorAPConnection(accessToken = NULL, KorAPUrl="https://korap.ids-mannheim.de", timeout = 1)
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
  kco <- KorAPConnection(accessToken = NULL, KorAPUrl="https://korap.ids-mannheim.de/", timeout = 1)
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
})
