test_that("KorAPConnection is printable", {
  kco <- new("KorAPConnection")
  expect_error(print(kco), NA)
})

test_that("KorAPConnection without access token prints ToS message", {
  expect_message(new("KorAPConnection", accessToken = NULL),
                 ".*By using.*non-commercial.*purposes", perl = TRUE)
})

test_that("Opening KorAPConnection with apiToken works", {
  kco <- new("KorAPConnection", accessToken="test token")
  persistAccessToken(kco)
  kco <- new("KorAPConnection")
  expect_equal(kco@accessToken, "test token")
  clearAccessToken(kco)
  kco <- new("KorAPConnection")
  expect_null(kco@accessToken)
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
