## Test environments

* local Fedora 37 with R version 4.2.3
* CentOS-release-7-9.2009 with R version 3.6.0
* github workflow on Windows with R 4.2.3
* github workflow on macOS with R 4.2.3
* github workflow on Ubuntu with R 4.2.3
* github workflow on Ubuntu with R 4.1.3
* github workflow on Ubuntu with R-devel (4.4.0)

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Checks with check_win()

* R 4.2.3
* R 4.1.3

## Notes

* Fixed CRAN policy violation (Packages which use Internet resources should fail gracefully with
  an informative message if the resource is not available or has changed (and not give a check
  warning nor error) by gracefully failing on invalid json api responses (via 04814f2, f650629).

### Old Notes

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
