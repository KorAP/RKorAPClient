test_that("KorAPConnection is printable", {
  kco <- new("KorAPConnection")
  expect_error(print(kco), NA)
})

test_that("Opening KorAPConnection prints some message.", {
  expect_message(new("KorAPConnection"), "KorAP")
})

test_that("Opening KorAPConnection with invalid apiToken fails", {
  expect_error(new("KorAPConnection", accessToken="test token"),
               "401")
})

test_that("Persisting null apiToken fails", {
  kco <- new("KorAPConnection")
  skip_if_not(is.null(kco@accessToken))
  expect_error(persistAccessToken(kco),
               ".*not supplied any access token.*",
               perl = TRUE)
})

test_that("Opening KorAPConnection with KorAPUrl works", {
  kco <- new("KorAPConnection", KorAPUrl="https://korap.ids-mannheim.de")
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
  kco <- new("KorAPConnection", KorAPUrl="https://korap.ids-mannheim.de/")
  expect_equal(kco@apiUrl, paste0("https://korap.ids-mannheim.de/api/", kco@apiVersion, "/"))
})
