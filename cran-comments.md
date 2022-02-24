## Test environments

* local Fedora 35 with R version 4.1.2
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

* CRAN Package Check problems starting from Februar 20 have been fixed.
* Now fully compliant with CRAN policy: *Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).*
  * The fixes requested in the mail from Brian Ripley of 23 Feb have been implemented.
* Tests that require API server connection are now skipped if no connection is available.
