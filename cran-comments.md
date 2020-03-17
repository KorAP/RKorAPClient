## Test environments
* local Fedora 31 with R version 3.6.3
* CentOS 7.7
* win builder (release)
* R-hub builder

## local R CMD check results
```
── R CMD check results ───────────────────────────────── RKorAPClient 0.5.7 ────
Duration: 45.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## https://cran.rstudio.com//web/checks/check_results_RKorAPClient.html

```
checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘curl’
  All declared Imports should be used.
```

Fixed in 0.5.7
