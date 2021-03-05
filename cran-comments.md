## Test environments
* local Fedora 33 with R version 4.0.4
* centos-release-7-9.2009.1.el7.centos.x86_64 with R version 3.6.0
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran
* github workflow with checks on Ubuntu, Mac and Windows with R 3.6 and 4.0

## Check results on all platforms
```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Notes
* version bump partially due to dropped support for orphaned plotly package
* test might be slightly slow sometimes because it's a web service client library 
and avoiding a mock web server still seems preferable
