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

