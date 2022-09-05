## Test environments

* local Fedora 35 with R version 4.1.3
* CentOS-release-7-9.2009.1.el7.centos.x86_64 with R version 3.6.0
* R-hub: Windows Server 2008 R2 SP1, R-release, 32/64 bit
* R-hub: Windows Server 2022, R-devel, 64 bit
* R-hub: Fedora Linux, R-devel, clang, gfortran
* github workflow on Windows with R 4.1.1 and 4.0.5
* github workflow on macOS with R 4.1.2
* github workflow on Ubuntu with R 4.0.5
* github workflow on Ubuntu with R 4.1.2


## Check results on all platforms

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Notes

* CRAN Package Check (NOTE) from 2022-08-12 has been fixed by upgrading Roxygen to 7.2.1.
* I did not receive any email notification about the issue or the removal from CRAN. 
* With respect to previous *problems with mails from CRAN* (`cransubmit@xmbombadil.wu.ac.at`) 
  in February/March 2022 our mail admin said that the CRAN mails were classified 
  as spam by SpamAssasin because of:
  ```
  FROM_NOT_REPLYTO=2,
  FROM_NOT_REPLYTO_SAME_DOMAIN=5
  ```
  thus receiving a spam score > 6.31. As a hotfix we whitelisted `cransubmit@xmbombadil.wu.ac.at`
  and reported the problem to the CRAN admins there.
