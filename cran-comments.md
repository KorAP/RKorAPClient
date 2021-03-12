## Test environments
* local Fedora 33 with R version 4.0.4
* centos-release-7-9.2009.1.el7.centos.x86_64 with R version 3.6.0
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * executed with: `rhub::check(platform="windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran
* github workflow with checks on Ubuntu, Mac and Windows with R 3.6 and 4.0

## Check results on all platforms
```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Notes
* In case v0.6.0 arrived at CRAN (for the first time, I got no confirmation mail), please ignore it 
* The minor version bump was partially due to the dropped support for the orphaned plotly package.
  (Initially deprecating the abandoned functions seemed not necessary here, as they were marked as risky and unmaintained from the outset.)
* Tests might be slightly slow sometimes because it's a web service client library 
and avoiding a mock web server still seems preferable.
