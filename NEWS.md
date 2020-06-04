# RKorAPClient 0.5.8.9001

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

