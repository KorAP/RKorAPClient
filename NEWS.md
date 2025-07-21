# RKorAPClient 1.1.0.9000

- `fetchAnnotaions()` method added to `KorAPQuery` class, to fetch annotations for all collected matches


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

