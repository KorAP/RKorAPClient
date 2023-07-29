## Notes

* new release to fix R 4.3 compatibility issues with `collocationAnalysis` method 
  (see <https://github.com/KorAP/RKorAPClient/issues/12>)

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Test environments

* local Fedora 38 with R version 4.3.1
* CentOS-release-7-9.2009 with R version 3.6.0
* github workflow on Windows with R 4.3.1
* github workflow on macOS with R 4.3.1
* github workflow on Ubuntu with R 4.3.1
* github workflow on Ubuntu with R 4.2.3
* github workflow on Ubuntu with R-devel (4.4.0)
* win-builder workflow on Windows with R-devel (2023-07-28 r84779 ucrt)


### Old Notes

#### Notes on 0.7.6

* Fixed CRAN policy violation (Packages which use Internet resources should fail gracefully...)
  pointed out in email from Prof Brian Ripley on Sat, 15 Apr 2023 16:20:19 +0100
  by gracefully failing on invalid json api responses, via:
  <https://github.com/KorAP/RKorAPClient/commit/04814f2be215f08a3777310af2202d14457c2e7c>,
  <https://github.com/KorAP/RKorAPClient/commit/f650629fa69ab10979f2ffe2652da77599caaf70>
* Replaced our log.info function with log_info to avoid name clashes
* Reduced timeouts in tests to make sure they don't sum up to over 10s.
* Fixed dontrun position in misc.R example making sure that examples run in <5s.
* Wrapped the last remaining web api query example in \dontrun.


#### Notes on 0.7.5

* Fixed CRAN policy violation (writing to the user's home filespace) pointed out
  in CRAN email from 2022-09-07 10:19 via
  <https://github.com/KorAP/RKorAPClient/commit/35eecca9d1fd43b441692a56bbd2aea94a7c3ed9>

#### Notes on 0.7.4

* All CRAN requests (email from 2022-09-06 08:36) are now resolved:
  * documentation completed and improved for `hc_add_onclick_korap_search`, `hc_freq_by_year_ci`, `KorAPConnection` class
  * proper cache directory used in `regional.R` demo
  * fixed and improved tempdir and path handling in `light-verb-construction-ca.R` and `recursiveCA.R` demos
  * fixed documentation for re-exported magrittr::`%>%` (pipe function)

#### Notes on 0.7.3

* Concerning potential reasons why RKorAPClient was archived (already resolved in previous version):
  * CRAN Package Check (NOTE) from 2022-08-12         <https://cran-archive.r-project.org/web/checks/2022/2022-08-12_check_results_RKorAPClient.html>
  ```
Check Details
Version: 0.7.2
Check: HTML version of manual
Result: NOTE
    Found the following HTML validation problems:
    RKorAPClient-package.html:23:4: Warning: <img> attribute "align" not allowed for HTML5
    RKorAPClient-package.html:23:4: Warning: <img> attribute "align" not allowed for HTML5
```
    has been fixed by upgrading Roxygen to 7.2.1.
  * I did not receive any email notification about the issue or the removal from CRAN.
    * With respect to previous *problems with emails from CRAN* (`cransubmit@xmbombadil.wu.ac.at`)
      in February/March 2022 our mail admin said that the CRAN emails were classified
      as spam by SpamAssasin because of:
      ```
      FROM_NOT_REPLYTO=2,
      FROM_NOT_REPLYTO_SAME_DOMAIN=5
      ```
      thus receiving a spam score > 6.31. As a hot fix we whitelisted `cransubmit@xmbombadil.wu.ac.at`
      and reported the problem to the CRAN admins there. Maybe this is the reason why I didn't receive
      and email notification about the NOTE.
    * Could you send me the bounce message which might contain more information why I did not receive the notification mail?
