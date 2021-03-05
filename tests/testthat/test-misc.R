test_that("queryStringToLabel works correctly", {
  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & creationDate in 2013",
      "referTo=x & textClass = /natur.*/ & creationDate in 2014"
    )
  ),
  c("2013", "2014"))

  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & pubDate in 2013",
      "referTo=x & textClass = /freizeit.*/ & pubDate in 2014"
    ), pubDateOnly = T
  ),
  c("2013", "2014"))

  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & creationDate in 2013",
      "referTo=x & textClass = /freizeit.*/ & creationDate in 2014"
    ), pubDateOnly = T
  ),
  c("2013", "2014"))

  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & creationDate in 2013",
      "referTo=x & textClass = /freizeit.*/ & creationDate in 2014"
    ), excludePubDate = T
  ),
  c("/natur.*/", "/freizeit.*/"))
})
