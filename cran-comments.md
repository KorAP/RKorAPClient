## Test environments
* local Fedora 34 with R version 4.0.5
* CentOS-release-7-9.2009.1.el7.centos.x86_64 with R version 3.6.0
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * executed with: `rhub::check(platform="windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran
* github workflow on Windows with R 4.1.1 and 4.0.5
* github workflow on macOS with R 4.0.5
* github workflow on Ubuntu with R 4.0.5


## Check results on all platforms
```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Notes
* Tests might be slightly slow sometimes because it's a web service client library 
and avoiding a mock web server still seems preferable.
