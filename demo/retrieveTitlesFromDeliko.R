library(RKorAPClient)
library(tidyverse)

VC = "textTypeRef = /Arzt.*/" # virtual corpus with just doctor novels etc.

query <- KorAPConnection("https://korap.dnb.de", verbose = TRUE) |>
	corpusQuery(
		"<base/s=t>",	# this finds each text once
		vc = VC,
		fields = c(
			"textSigle",
			"title",
			"subTitle"
			#		"author",
			#		"textType",
			#		"textTypeRef",
			#		"publisher",
			#		"pubDate",
			#		"pubPlace",
			#		"ISBN",
			#		"URN"
		)
	) |>
	fetchAll()

df <- query@collectedMatches |>
	select(-c("matchStart", "matchEnd"))

View(df)
