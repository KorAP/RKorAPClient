---
KorAP web service client package for R: tidyverse branch
---

## Description

Simple R package to access the [web service API](https://github.com/KorAP/Kustvakt/wiki) of the [KorAP Corpus Analysis Platform](https://korap.ids-mannheim.de/) devloped at the [IDS Mannheim](http://ids-mannheim.de/)

**This packgage is in its early stages and not stable yet! In particular, please expect that, at this early stage, objects, functions, parameters as well as their names or identifiers will still change continuously without any notification. Use it on your own risk!**

## Getting started

At this point there is no binary package on CRAN yet, so you have to install the development version from our [Gerrit server](https://korap.ids-mannheim.de/gerrit/) using the devtool package:

```R
# install.packages("devtools")
library(devtools)
install_git("https://korap.ids-mannheim.de/gerrit/KorAP/RKorAPClient", ref="tidy")
library(RKorAPClient)
?corpusQuery
?freqeuncyQuery
```

## Hello world

```R
library(RKorAPClient)
new("KorAPConnection", verbose=TRUE) %>% corpusQuery("Hello world") %>% fetchAll()
```

## Example
```r
library(RKorAPClient)
library(ggplot2)
kco <- new("KorAPConnection", verbose=TRUE)
expand_grid(condition = c("textDomain = /Wirtschaft.*/", "textDomain != /Wirtschaft.*/"), year = (2002:2018)) %>%
    cbind(frequencyQuery(kco, "[tt/l=Heuschrecke]", paste0(.$condition," & pubDate in ", .$year)))  %>%
    ipm() %>%
    ggplot(aes(x = year, y = ipm, fill = condition, color = condition, ymin = conf.low, ymax = conf.high)) +
    geom_freq_by_year_ci()
```
![](man/figures/Readme-Example-1.png)<!-- -->

## Demos

More elaborate R scripts demonstrating the use of the package can be found in the [demo](demo) folder.

## Development and License

**Authors**: [Marc Kupietz](http://www1.ids-mannheim.de/zfo/personal/kupietz/)

Copyright (c) 2019, [IDS Mannheim](http://ids-mannheim.de/), Germany

This package is developed as part of the [KorAP](http://korap.ids-mannheim.de/)
Corpus Analysis Platform at the Leibniz Institute for German Language
([IDS](http://ids-mannheim.de/)).

It is published under the
[BSD-2 License](LICENSE).

## Contributions

Contributions are very welcome!

Your contributions should ideally be committed via our [Gerrit server](https://korap.ids-mannheim.de/gerrit/)
to facilitate reviewing (see [Gerrit Code Review - A Quick Introduction](https://korap.ids-mannheim.de/gerrit/Documentation/intro-quick.html)
if you are not familiar with Gerrit). However, we are also happy to accept comments and pull requests
via GitHub.

Please note that unless you explicitly state otherwise any
contribution intentionally submitted for inclusion into this software shall –
as this software itself – be under the [BSD-2 License](https://raw.githubusercontent.com/KorAP/Krill/master/LICENSE).

## References

Kupietz, Marc / Margaretha, Eliza / Diewald, Nils / Lüngen, Harald / Fankhauser, Peter (2019): [What’s New in EuReCo? Interoperability, Comparable Corpora, Licensing](https://nbn-resolving.org/urn:nbn:de:bsz:mh39-90261). In: Bański, Piotr/Barbaresi, Adrien/Biber, Hanno/Breiteneder, Evelyn/Clematide, Simon/Kupietz, Marc/Lüngen, Harald/Iliadi, Caroline (Hrsg.): [*Proceedings of the Internation Corpus Linguistics Conference 2019 Workshop "Challenges in the Management of Large Corpora (CMLC-7)"*](https://ids-pub.bsz-bw.de/solrsearch/index/search/searchtype/collection/id/21038), 22nd of July Mannheim: Leibniz-Institut für Deutsche Sprache,33-39.
