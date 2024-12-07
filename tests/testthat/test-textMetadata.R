test_that("textMetadata works", {
  skip_if_offline()
  m <- new("KorAPConnection") %>% textMetadata(c("WUD17/B96/57558", "WUD17/A97/08541"))
  expect("textType" %in% names(m), "textMetadata value should contain a textType column")
})


test_that("textMetadata works for unknown text sigles", {
  skip_if_offline()
  m <- new("KorAPConnection") %>% textMetadata(c("WUD17/B96/57558", "unknownsigle"))
  expect("errors" %in% names(m), "textMetadata should return an errors column if a text does not exist")
})

test_that("textMetadata works with list valued fields", {
  skip_if_offline()
  m <- new("KorAPConnection") %>% textMetadata("WUD17/B96/57558")
  expect("staat-gesellschaft\\tbiographien-interviews" == m$textClass[1], "multiple text classes / domnains should be tab separated")
})
