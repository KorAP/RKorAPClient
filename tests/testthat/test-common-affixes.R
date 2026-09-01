lcp <- RKorAPClient:::longestCommonPrefixLength
lcs <- RKorAPClient:::longestCommonSuffixLength

test_that("longestCommonPrefixLength counts the shared prefix", {
  expect_equal(lcp(c("abc", "abd")), 2)
  expect_equal(lcp(c("abc", "abc")), 3)
  expect_equal(lcp(c("xyz", "abc")), 0)
  expect_equal(lcp(c("aaa", "aa", "a")), 1)
  expect_equal(lcp(c("prefix_A_suffix", "prefix_B_suffix")), 7)
  expect_equal(lcp(c("üöä-test", "üöä-rest")), 4)
})

test_that("longestCommonSuffixLength counts the shared suffix", {
  expect_equal(lcs(c("abc", "dbc")), 2)
  expect_equal(lcs(c("abc", "abc")), 3)
  expect_equal(lcs(c("xyz", "abc")), 0)
  expect_equal(lcs(c("aaa", "aa", "a")), 1)
  expect_equal(lcs(c("prefix_A_suffix", "prefix_B_suffix")), 7)
  expect_equal(lcs(c("abc-üöä", "xyz-üöä")), 4)
})

test_that("empty vectors, empty strings and single strings behave as before", {
  # a single string is its own prefix and suffix, as in the PTXQC functions
  # these replace
  expect_equal(lcp(character(0)), 0)
  expect_equal(lcs(character(0)), 0)
  expect_equal(lcp("single"), 6)
  expect_equal(lcs("single"), 6)
  expect_equal(lcp(c("", "")), 0)
  expect_equal(lcs(c("", "")), 0)
  expect_equal(lcp(c("abc", "")), 0)
  expect_equal(lcs(c("", "abc")), 0)
})

test_that("queryStringToLabel clips common prefixes and suffixes", {
  expect_equal(
    queryStringToLabel(paste("textType = /Zeit.*/ & pubDate in", 2010:2019)),
    as.character(2010:2019)
  )
  expect_equal(
    queryStringToLabel(c("[marmot/m=mood:subj]", "[marmot/m=mood:ind]")),
    c("subj", "ind")
  )
  expect_equal(
    queryStringToLabel(c("wegen dem [tt/p=NN]", "wegen des [tt/p=NN]")),
    c("dem", "des")
  )
})

test_that("queryStringToLabel honours pubDateOnly and excludePubDate", {
  vc <- paste("textType = /Zeit.*/ & pubDate in", 2010:2012)
  expect_equal(queryStringToLabel(vc, pubDateOnly = TRUE), as.character(2010:2012))
  expect_equal(
    queryStringToLabel(c("textDomain = /Wirtschaft.*/ & pubDate in 2010",
                         "textDomain != /Wirtschaft.*/ & pubDate in 2010"),
                       excludePubDate = TRUE),
    c("=", "!=")
  )
})
