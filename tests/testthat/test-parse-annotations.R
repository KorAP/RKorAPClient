test_that("parse_xml_annotations returns empty vectors for empty input", {
  res1 <- RKorAPClient:::parse_xml_annotations(NULL)
  res2 <- RKorAPClient:::parse_xml_annotations(NA)
  res3 <- RKorAPClient:::parse_xml_annotations("")

  for (res in list(res1, res2, res3)) {
    expect_true(is.list(res))
    expect_named(res, c("token", "lemma", "pos", "morph"))
    expect_length(res$token, 0)
    expect_length(res$lemma, 0)
    expect_length(res$pos, 0)
    expect_length(res$morph, 0)
  }
})

test_that("parse_xml_annotations extracts tokens/pos/lemma across multiple <mark> blocks", {
  xml_snippet <- '<span class="context-left"></span>
  <span class="match">
    <span title="tt/l:Wir"><span title="tt/p:PPER">Wir</span></span>
    <mark>
      <span title="tt/l:können"><span title="tt/p:VVFIN">können</span></span>
    </mark>
    <span title="tt/l:alles"><span title="tt/p:PIS">alles</span></span>
    <mark>
      <span title="tt/l:außer"><span title="tt/p:APPR">außer</span></span>
      <span title="tt/l:Plan"><span title="tt/p:NN">Plan</span></span>
    </mark>
  </span>
  <span class="context-right"></span>'

  parsed <- RKorAPClient:::parse_xml_annotations(xml_snippet)

  expect_equal(parsed$token, c("Wir", "können", "alles", "außer", "Plan"))
  expect_equal(parsed$pos,   c("PPER", "VVFIN", "PIS", "APPR", "NN"))
  expect_equal(parsed$lemma, c("Wir", "können", "alles", "außer", "Plan"))

  # morph not present in snippet; should be NA-aligned to tokens
  expect_length(parsed$morph, length(parsed$token))
  expect_true(all(is.na(parsed$morph)))
})

test_that("parse_xml_annotations handles missing lemma/pos/morph gracefully", {
  # First token has POS only; second has lemma+POS+morph; third has lemma only
  xml_snippet <- '<span class="match">
    <span title="tt/p:NN">Haus</span>
    <mark><span title="tt/l:können tt/p:VVFIN marmot/m:verbform:fin">können</span></mark>
    <span title="tt/l:gehen">gehen</span>
  </span>'

  parsed <- RKorAPClient:::parse_xml_annotations(xml_snippet)

  expect_equal(parsed$token, c("Haus", "können", "gehen"))
  expect_equal(parsed$pos,   c("NN",   "VVFIN", NA))
  expect_equal(parsed$lemma, c(NA,      "können", "gehen"))
  expect_equal(parsed$morph, c(NA,      "verbform:fin", NA))

  # Vectors must be equal length
  n <- length(parsed$token)
  expect_length(parsed$lemma, n)
  expect_length(parsed$pos, n)
  expect_length(parsed$morph, n)
})

