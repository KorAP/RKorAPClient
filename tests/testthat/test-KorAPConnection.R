test_that("KorAPConnection fails gracefully on unresolvable host", {
  skip_if_offline()
  expect_message(new("KorAPConnection", apiUrl="http://xxx.asdhsahdsadhvgas.org"), "No internet|Could not resolve")
})

test_that("KorAPConnection fails gracefully on timeout", {
  skip_if_offline()
  expect_message(new("KorAPConnection", apiUrl="http://httpbin.org/delay/3", timeout = 1), "No internet|Timeout")
})

test_that("KorAPConnection is printable", {
  skip_if_offline()
  kco <- new("KorAPConnection", timeout = 10)
  skip_if(is.null(kco@welcome))
  expect_error(print(kco), NA)
})

test_that("Opening KorAPConnection prints some message.", {
  skip_if_offline()
  expect_message(new("KorAPConnection"), "KorAP")
})

test_that("Opening KorAPConnection with invalid apiToken fails gracefully", {
  skip_if_offline()
  expect_message(new("KorAPConnection", accessToken="test token", timeout = 10),
               "401|Timeout")
})

test_that("Persisting null apiToken fails", {
  skip_if_offline()
  kco <- new("KorAPConnection", timeout = 10)
  skip_if_not(is.null(kco@accessToken))
  skip_if(is.null(kco@welcome))
  expect_error(persistAccessToken(kco),
               ".*not supplied any access token.*",
               perl = TRUE)
})

test_that("Opening KorAPConnection with KorAPUrl works", {
  skip_if_offline()
  kco <- new("KorAPConnection", KorAPUrl="https://korap.ids-mannheim.de", timeout = 1)
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
  kco <- new("KorAPConnection", KorAPUrl="https://korap.ids-mannheim.de/", timeout = 1)
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
})
