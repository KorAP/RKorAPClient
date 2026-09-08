# unpublished dev version 1.3.0.9000

- **`cacheAs` is now offered by `frequencyQuery()`, `corpusStats()`, `collocationScoreQuery()` and `textMetadata()`** as well, not only by `collocationAnalysis()`. It is a different thing from the connection's `cache`, which is a transparent speed-up: a `cacheAs` file belongs to the caller and is what keeps an analysis reproducible, since KorAP corpora grow and the same query returns different numbers next year. That is worth having for the quick functions too, where caching for speed would be pointless

- **`cacheAs` files record the version that wrote them** and are refused, with a warning, when that is older than 1.4.0. Their contents are finished results including the association scores, which this version computes differently, so an old file would silently hand back numbers that would not be arrived at again - something no comparison of parameters can notice. The file is then recomputed and overwritten; pass a different name to keep it. How loud a query is no longer counts as a parameter either: `verbose` does not change what is returned

- **the names of a named `vc` vector are now used as labels** by `frequencyQuery()`, `corpusStats()` and `collocationScoreQuery()`, as `collocationAnalysis()` already did. `frequencyQuery()` ignored them, `corpusStats()` put them into row names, which the first `bind_rows()` drops, and `collocationScoreQuery()` derived a label from the corpus definitions instead, so that `c(before = ..., since = ...)` came out as `"1990 & pubDat…"`. Where a vector carries no names, nothing changes: no `label` column appears that was not there before

- **`collocationAnalysis()` now discards collocates that occur less often than expected** by chance. `logDice`, by which it ranks and thresholds, expresses how salient a pair is rather than how surprising, so a frequent word could appear among the top collocates although the node does not attract it at all: for *Grund* in a 5+5 window, *Berlin* reaches a logDice of 3.84, close to *triftiger* at 3.96, while co-occurring 1.74 bits *less* often than expected. The new `minObservedExpectedRatio` parameter defaults to 1 and keeps such pairs out. Raise it to demand a stronger contrast, e.g. 2 for collocates occurring at least twice as often as expected, or set it to 0 for the unfiltered result of earlier versions, e.g. to study repulsion. `collocationScoreQuery()` is unaffected, as there the pairs to score are given explicitly
- **changed `logDice` values**: `logDice()` is now computed as defined by Rychlý (2008), `14 + log2(2 * O / (O1 + O2))`, so that its values are comparable to those of Sketch Engine and other tools. It previously multiplied the node frequency by the window size, `14 + log2(2 * O / (w * O1 + O2))`, which added a count of window positions to a count of word tokens and made the coefficient asymmetric, so that swapping node and collocate changed the score. Because `w * O1` dominated the denominator for a frequent node, rare collocates were penalised: for *triftiger* as a collocate of *Grund* in a 5+5 window, logDice was 0.64, below *Berlin* at 2.03, although *Berlin* co-occurs with *Grund* less often than chance predicts. The values are now 3.96 and 3.84. Scores rise by up to `log2(w)`, that is by up to 3.32 for the default context of 5 left and 5 right, so `collocationAnalysis()` with the default `thresholdScore = "logDice"` and `threshold = 2` is now somewhat more permissive when recursing. Results computed with a total window size of 1, as in the light verb construction example of the Readme, are unaffected. The other association scores are unchanged: they take the window size into account through the expected frequency `E`, which is correct

- **changed `ll()` values**: the contingency table of `ll()` scaled only its row total by the window size, an inconsistency spotted by [Tim Feldmüller](https://github.com/feldmueller), leaving cells that do not add up to one sample. Following Evert (2004), the sample consists of co-occurrence tokens, so the sample size and both marginals scale with the window: an occurrence of either word takes part in `window_size` pairs. The expected co-occurrence frequency is unchanged at `window_size * O1 * O2 / N`, which is why `pmi`, `mi2` and `mi3` are unaffected, but log-likelihood values differ, by 0.1% to well over 100% depending on the frequencies and the window. This also removes the case where `N - window_size * O1` turned negative and the score became `NaN`, since `window_size * (N - O1)` cannot: the warning added earlier in this development version is therefore gone again, having treated a symptom of this

- `collocationAnalysis()` now stores the analysis parameters in its `cacheAs` file and compares them on the next call. If they differ, the cached result is not the one that was asked for, so it is recomputed and the file overwritten, with a warning naming the parameters that differ. This catches the case of a parameter being changed while an old cache file is still lying around. Cache files written by 1.3.0 do not contain the parameters yet and keep being used as they are

- dropped the `PTXQC` dependency, which was imported for two small string functions (`lcpCount()` and `lcsCount()`, used by `queryStringToLabel()`) but pulled in `rmzqc`, `jsonvalidate` and `V8`, and with them the only dependency requiring a `libv8` installation. The two functions are now implemented in the package itself, 10 to 65 times faster than the originals, and with unchanged results

# RKorAPClient 1.3.0

- added `cacheAs` parameter to `collocationAnalysis()` for transparent result caching: if the specified RDS file exists, the cached result is returned immediately; otherwise the analysis runs and the result is saved to the file (`.rds` extension is added automatically if omitted)
- fixed score threshold in recursive CA
- focus is now injected into webUIRequestUrls in collocationAnalysis results, when possible
- added *experimental* support for comparing collocation analyses across multiple vcs (`max_delta_<score>`, `winner<score>`, `loser_score<score>` columns etc.), including explicit winner/loser `webUIRequestUrl` columns for association scores, ranks, and percentile ranks. Missing per-label concordance URLs are now derived by replacing the `cq` parameter of an available row URL with the target label's vc, and unsuffixed consensus `winner_webUIRequestUrl` / `loser_webUIRequestUrl` columns are populated when score-based URL choices agree. The names and semantics of these columns may still change without a deprecation cycle
- collocates that are not attested in every compared vc get their missing scores imputed. Such rows are now marked by the new `imputed`, `n_imputed` and `imputed_<label>` columns, and reported in verbose mode, so that presence/absence artifacts can be told apart from measured contrasts (`filter(!imputed)`). See the new "Interpreting multi-VC comparisons" section in `?collocationAnalysis` for how to read the comparison columns
- `collocationAnalysis()` now warns when repeated node/collocate/label rows are reduced to their first occurrence for the comparison columns, instead of dropping them silently
- added support for passing condition labels when comparing multiple vcs by allowing for named vc lists
- `collocationScoreQuery()` now accepts a vector of collocates and queries every combination of collocate and virtual corpus
- `KorAPConnection()` is now a regular constructor function instead of the S4 class generator, so that its manual page documents how it is actually called. `new("KorAPConnection", ...)` and all existing calls keep working unchanged

# RKorAPClient 1.2.1

- fixed `fetchAnnotations()` morphology so MarMoT and other foundries keep all features from nested spans ([#30](https://github.com/KorAP/RKorAPClient/issues/30))
- reimplemented annotation parsing with `xml2`, retaining multiple lemma/POS values and improving robustness of token alignment
- changed format of annotation_snippet column for compatibility with Python client

# RKorAPClient 1.2.0

- `fetchAnnotations()` method added to `KorAPQuery` class, to fetch annotations for all collected matches
- regional demo updated and extended with interactive highcharter plot
- automatic documentation tests introduced (via copromted Readme.md and different LLMs)
- allow overriding `KorAPConnection(verbose=)` default via env var `KORAP_VERBOSE` or R option `rkorap.verbose` (explicit argument still wins)

# RKorAPClient 1.1.0

- improved documentation, which should be more use case oriented now
- fixed bug with fetching result pages with an offset >= 10,000 (=1e+05 ...) [issue #25](https://github.com/KorAP/RKorAPClient/issues)
- timed out corpus queries are no longer cached (see [issue #7](https://github.com/KorAP/RKorAPClient/issues/7))
- improved stability of `ci` function
- improved error handling
- improved logging
- added ETAs to logging in verboose mode

# RKorAPClient 1.0.0

- Simplified authorization process for accessing restricted data via the new `auth()` function (see `?auth`)
- Switched from `httr` to `httr2`for all API requests
- Minimally required R version is now 4.1.0 (released 2021-05-18)
- Fixed compatibility with upcoming  `ggplot2` 3.6.0 (thanks @teunbrand)
- Fixed issues with tokenized matches in `corpusQuery` results
- Fixed smoothing constant in `mergeDuplicateCollocates` function
- Fixed missing suggested packages in DESCRIPTION (for demos)

# RKorAPClient 0.9.0

- added citation hint, see `citation("RKorAPClient")`
- added `matchStart` and `matchEnd` columns to `corpusQuery` results, that contain the start and end position of the match in the text
- added function `mergeDuplicateCollocates` to merge collocation analysis results for different context positions
- added a column `query` to collocation analysis results
- fixed CRAN check notes on Rd `\link{}` targets missing package
- fixed possible problems in `corpusStats` method
- improved documentation for `span` parameter in collocation analysis functions
- uses new metadata fields API in `textMetadata` method, if available, to retrieve also custom metadata for a text based on its sigle
- uses new metadata fields API in `corpusQuery` method, if available


# RKorAPClient 0.8.1

- fixed R 4.4 compatibility of collocationAnalysis function
- replaced deprecated legend.position (ggplot2)
- fixed rare frequencyQuery incompatibility with outdated KorAP instances (see https://github.com/KorAP/Kustvakt/issues/668)


# RKorAPClient 0.8.0

- added `textMetadata` KorAPConnection method to retrieve all metadata for a text based on its sigle
- added `webUiRequestUrl` column also to corpusStats results, so that also virtual corpus definitions can be linked to / tested directly in the KorAP UI
- added on-click action to open KorAP query also to highcharter scatter plots
- if not `metadataOnly` is set, also tokenized snippets are now retrieved in corpus queries (stored in `collectedMatches$tokens`) 
- uses server side tokenized matches in collocation analysis, if supported by KorAP server
- fixed regional demo (will be updated again soon)

# RKorAPClient 0.7.7

- fixed R 4.3 compatibility (see <https://github.com/KorAP/RKorAPClient/issues/12>)
- added demo that plots plural gender variant frequencies over time
- added runtime OAuth2 browser flow demos with httr and httr2

# RKorAPClient 0.7.6

- fixed error in recursiveCA demo
- added documentation on authorization using the OAuth browser flow
- fixed graceful fail on invalid json responses from API requests (CRAN policy violation)
- updated Roxygen to 7.2.3
- added context parameter to corpusQuery
- updated GitHub workflows
- in collocationAnalysis: given withinSpan parameters are now correctly passed to queries for examples
- replaced our log.info function with log_info to avoid name clashes

# RKorAPClient 0.7.5 (CRAN release)

- resolved CRAN policy violation (writing to user's home filespace)

# RKorAPClient 0.7.4 (unreleased due to CRAN requests)

- resolved CRAN requests:
  - documentation completed and improved for hc_add_onclick_korap_search, hc_freq_by_year_ci, KorAPConnection class
  - proper cache directory used in regional demo
  - fixed and improved path handling in collocation analysis and light verb construction demos
  - fixed documentation for reexported magrittr::`%>%` (pipe function)
- replaced head with dplyr::slice_head when used on data frames or tibbles

# RKorAPClient 0.7.3 (unreleased due to CRAN requests)

- updated Roxygen to 7.2.1 (fixes bug that caused CRAN removal of RKorAPClient)
- updated shiny demo
- added some demos for comparisons by country of publication
- fixed collocation scores for lemmatized node or collocate queries

# RKorAPClient 0.7.2 (CRAN release)

- now fails gracefully with an informative message if the API server is not accessible
- fixed shiny demo
- `hc_add_onclick_korap_search()` now also works with bar, column and pie highcharts
- new parameters added to `collocationAnalysis` function:
  - `maxRecurse` - apply collocation analysis recursively `maxRecurse` times
  - `addExamples` -  If `TRUE`, examples for instances of collocations will be added in a column `example`. This makes a difference in particular if 'node' is given as a lemma query.
  - `thresholdScore` - association score function to use for computing the threshold that is applied for recursive collocation analysis calls
  - `threshold`- minimum value of `thresholdScore` function call to apply collocation analysis recursively

# RKorAPClient 0.7.1 (CRAN release)
- documentation migrated to roxygen2md
- Some examples in the documentation are now wrapped with:
  ```
     ## Not run
     ...
     ## End(Not run)
  ```
  This was necessary to meet the time limits of automatic CRAN checks
  under all circumstances and has otherwise no significance,

## Version change invalidates cache
The change of the minor version from 6 to 7 automatically invalidates your
cached query results. However, since the API has not changed, you can continue
using the old cached results by copying or renaming your cache folder.

On linux, for example, this can look like this:
```
mv ~/.cache/R/R.cache/RKorAPClient_0.6 ~/.cache/R/R.cache/RKorAPClient_0.7
```

or like this:
```
mv ~/.Rcache/RKorAPClient_0.6 ~/.Rcache/RKorAPClient_0.7
```
To find the RKorAPClient cache directory for your environment, you can use:
```
R.cache::getCacheRootPath()
```

# RKorAPClient 0.7.0

## New Features
- experimental new `collocationAnalysis` method (client-sided)
- new parameter `randomizePageOrder` to fetch result pages in randomized order 
- new parameter `ignoreCollocateCase` in `collocationScoreQuery`
- new parameter `withinSpan` (default: `base/s=s`) in `collocationScoreQuery`
- number of hits logged during queries, if `verbose=TRUE`

## Bug Fixes
- fixed umlaut queries on windows
- fixed retrieval of access token when multiple access tokens are stored

# RKorAPClient 0.6.1 (CRAN release)

## Changes
- fixes calculation of logDice coefficient
- updated Readme.md

# RKorAPClient 0.6.0

## Changes
- collocationScoreQuery method added
- hc_add_onclick_korap_search function added
- shiny web application demo added
- support for orphaned plotly package dropped
- improved documentation structure

# RKorAPClient 0.5.9

## Changes
- The local cache is now notified of new corpus index revisions when new connections are established. As a side effect, with v0.5.9 existing caches are invalidated. 

# RKorAPClient 0.5.8

## Changes
- New boolean parameter `smooth` added to `hc_freq_by_year_ci`

## Bug Fixes
- Fixed issues with [dplyr 1.0](https://github.com/tidyverse/dplyr/releases/tag/v1.0.0)

# RKorAPClient 0.5.7

## Changes
- Introduced continuous integration tests via gh-actions, pulled from [highcharter](https://github.com/jbkunst/highcharter) (thanks @pachamaltese)

## Bug Fixes
- Fixed some links in Readme.md
- Removed redundant curl import
- Fixed handling of single query term queries in highcharter-helper that had an empty plot and a warning 
  ```
  Unknown or uninitialised column: 'condition'.
  ```
  as consequences.

# RKorAPClient 0.5.6
- Fix some links in Readme.md
- First release on CRAN

# RKorAPClient 0.5.5
- Use TRUE and FALSE instead of T and F
- Be more specific about authors and rights holder
- Elaborate Description field in DESCRIPTION file
- Add forthcoming LREC paper to Readme.md

# RKorAPClient 0.5.4
- Fix DESCRIPTION file.

# RKorAPClient 0.5.3
- Further minimization of tests to make CRAN happy.
- Make authors and maintainers machine readable.

# RKorAPClient 0.5.2
- Further minimization of tests to make CRAN happy. Safety shouldn't suffer.
- Make license specification github and CRAN compliant.

# RKorAPClient 0.5.1
- Fixed many typos.
- Removed redundant and too long running tests.

# RKorAPClient 0.5.0

- Initial release on CRAN.
- Merge highcharter branch providing the new helper function `hc_freq_by_year_ci`.
- Fix reporting cached results in verbose mode.
- Add demo for comparing frequencies in spoken vs. written virtual corpora. See `demo("writtenVsSpoken")`
- Don't invalidate cache on patch level increments.
