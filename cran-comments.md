## Test environments

* local Fedora 35 with R version 4.1.3
* CentOS-release-7-9.2009.1.el7.centos.x86_64 with R version 3.6.0
* github workflow on Windows with R 4.2.1
* github workflow on macOS with R 4.2.1
* github workflow on Ubuntu with R 4.2.1
* github workflow on Ubuntu with R-devel

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Checks with check_win()

* R-devel (1 NOTE because package was archived on CRAN)
* R 4.2.1 (1 NOTE because package was archived on CRAN)
* R 4.1.3 OK

## Notes

* CRAN Package Check (NOTE) from 2022-08-12 has been fixed by upgrading Roxygen to 7.2.1.
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
