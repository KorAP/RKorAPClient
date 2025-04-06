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
    ),
    pubDateOnly = T
  ),
  c("2013", "2014"))

  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & creationDate in 2013",
      "referTo=x & textClass = /freizeit.*/ & creationDate in 2014"
    ),
    pubDateOnly = T
  ),
  c("2013", "2014"))

  expect_equivalent(queryStringToLabel(
    c(
      "referTo=x & textClass = /natur.*/ & creationDate in 2013",
      "referTo=x & textClass = /freizeit.*/ & creationDate in 2014"
    ),
    excludePubDate = T
  ),
  c("/natur.*/", "/freizeit.*/"))
})

test_that("geom_freq_by_year_ci works correctly", {
  df <-
    structure(
      list(
        condition = c(
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain = /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/",
          "textDomain != /Wirtschaft.*/"
        ),
        year = c(
          2005L,
          2006L,
          2007L,
          2008L,
          2009L,
          2010L,
          2011L,
          2005L,
          2006L,
          2007L,
          2008L,
          2009L,
          2010L,
          2011L
        ),
        query = c(
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]",
          "[tt/l=Heuschrecke]"
        ),
        totalResults = c(
          531L,
          823L,
          1130L,
          496L,
          302L,
          159L,
          122L,
          2831L,
          2245L,
          2477L,
          2010L,
          1697L,
          1142L,
          1829L
        ),
        vc = c(
          "textDomain = /Wirtschaft.*/ & pubDate in 2005",
          "textDomain = /Wirtschaft.*/ & pubDate in 2006",
          "textDomain = /Wirtschaft.*/ & pubDate in 2007",
          "textDomain = /Wirtschaft.*/ & pubDate in 2008",
          "textDomain = /Wirtschaft.*/ & pubDate in 2009",
          "textDomain = /Wirtschaft.*/ & pubDate in 2010",
          "textDomain = /Wirtschaft.*/ & pubDate in 2011",
          "textDomain != /Wirtschaft.*/ & pubDate in 2005",
          "textDomain != /Wirtschaft.*/ & pubDate in 2006",
          "textDomain != /Wirtschaft.*/ & pubDate in 2007",
          "textDomain != /Wirtschaft.*/ & pubDate in 2008",
          "textDomain != /Wirtschaft.*/ & pubDate in 2009",
          "textDomain != /Wirtschaft.*/ & pubDate in 2010",
          "textDomain != /Wirtschaft.*/ & pubDate in 2011"
        ),
        webUIRequestUrl = c(
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202005&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202006&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202007&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202008&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202009&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202010&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202011&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202005&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202006&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202007&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202008&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202009&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202010&ql=poliqarp",
          "https://korapt.ids-mannheim.de/?q=%5btt%2fl%3dHeuschrecke%5d&cq=textDomain%20%21%3d%20%2fWirtschaft.%2a%2f%20%26%20pubDate%20in%202011&ql=poliqarp"
        ),
        total = c(
          35980430L,
          43834111L,
          45318302L,
          48021215L,
          43445640L,
          37850216L,
          43208130L,
          734690498L,
          754436010L,
          837243512L,
          870913993L,
          840473763L,
          758631859L,
          1650860702L
        ),
        f = c(
          1.4758022625077e-05,
          1.87753322977167e-05,
          2.49347382874142e-05,
          1.0328768232957e-05,
          6.95121535785869e-06,
          4.20076862969553e-06,
          2.82354269902447e-06,
          3.85332328062857e-06,
          2.97573282590262e-06,
          2.9585179992413e-06,
          2.30792020355103e-06,
          2.0190993160128e-06,
          1.50534147287901e-06,
          1.10790692260358e-06
        ),
        conf.low = c(
          13.541726123006,
          17.5246639403598,
          23.5119781061303,
          9.44895282189122,
          6.19934083242504,
          3.58441307384462,
          2.35445121482762,
          3.71330289674541,
          2.85451040064217,
          2.84369529096211,
          2.20864478463198,
          1.92471184006826,
          1.41988948639017,
          1.05798679123886
        ),
        conf.high = c(
          16.0822620637798,
          20.1144124716816,
          26.4429033931224,
          11.2894924346856,
          7.79280444795191,
          4.92066150039394,
          3.38358495629102,
          3.9985973655165,
          3.10207445836027,
          3.07795241551153,
          2.41163153037033,
          2.11808565716224,
          1.59589532013765,
          1.1601678685439
        ),
        ipm = c(
          14.758022625077,
          18.7753322977167,
          24.9347382874142,
          10.328768232957,
          6.95121535785869,
          4.20076862969553,
          2.82354269902447,
          3.85332328062857,
          2.97573282590262,
          2.9585179992413,
          2.30792020355103,
          2.0190993160128,
          1.50534147287901,
          1.10790692260358
        )
      ),
      class = "data.frame",
      row.names = c(NA,-14L)
    )
    gpt <- df %>% ggplot2::ggplot(ggplot2::aes(year, ipm, fill = condition, color = condition)) +
      geom_freq_by_year_ci()
    labels <- if ("get_labs" %in% getNamespaceExports("ggplot2")) {
      ggplot2::get_labs(gpt)
    } else {
      gpt[["labels"]]
    }
    expect_equal(labels[["url"]], "webUIRequestUrl")
    expect_equal(gpt[["data"]][["query"]][14], "[tt/l=Heuschrecke]")
})
