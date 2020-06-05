## Relation between 0.5.9 and 0.5.8
If you haven't checked 0.5.8 yet, you can just skip it and use 0.5.9 (unless 0.5.8 is OK and 0.5.9 not)

## Test environments
* local Fedora 32 with R version 3.6.3
* CentOS 7.7
* win builder (devel)
* R-hub builder
* github workflow with checks on Ubuntu, Mac and Windows with R 4.0

## local R CMD check results
```
── R CMD check results ───────────────────────────────── RKorAPClient 0.5.9 ────
Duration: 3m 38.4s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## https://cran.rstudio.com//web/checks/check_results_RKorAPClient.html

Fixed already in v0.5.8: Donttest compatibility issues with dplyr 1.0.0:
https://www.stats.ox.ac.uk/pub/bdr/donttest/RKorAPClient.out

## R-hub on Windows Server 2008 R2 SP1, R-devel, 32/64 bit
… reports the following dependency issue which, however, does not appear on any other test build (win builder, mac, Ubuntu, Fedora) and seems unrelated to RKorAPClient:

```
 877#> trying URL 'https://cloud.r-project.org/bin/windows/contrib/4.1/PTXQC_1.0.4.zip'

 878#> Content type 'application/zip' length 4038469 bytes (3.9 MB)

 879#> ==================================================

 880#> downloaded 3.9 MB

 881#> setting _R_CHECK_FORCE_SUGGESTS_ to false

 882#> setting R_COMPILE_AND_INSTALL_PACKAGES to never

 883#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false

 884#> setting R_REMOTES_STANDALONE to true

 885#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true

 886#> setting _R_CHECK_FORCE_SUGGESTS_ to true

 887#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true

 888#> * installing to library 'C:/Users/USERSHFYCydkVg/R'

 889#> * installing *source* package 'RKorAPClient' ...

 890#> ** using staged installation

 891#> ** R

 892#> ** demo

 893#> ** byte-compile and prepare package for lazy loading

 894#> Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :

 895#> namespace 'vctrs' 0.2.4 is being loaded, but >= 0.3.0 is required

 896#> Calls: ... namespaceImport -> loadNamespace -> namespaceImport -> loadNamespace

 897#> Execution halted

 898#> ERROR: lazy loading failed for package 'RKorAPClient'

 899#> * removing 'C:/Users/USERSHFYCydkVg/R/RKorAPClient'
```

## testthat duration
I've tried to reduce the time required for the tests.
