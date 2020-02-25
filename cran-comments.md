## Test environments
* local Fedora 31 with R version 3.6.3
* CentOS 7.7
* win builder (release)
* R-hub builder

## local R CMD check results
```
── R CMD check results ───────────────────────────────── RKorAPClient 0.5.3 ────
Duration: 49.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## R-hub builder
### NOTE
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marc Kupietz <kupietz@ids-mannheim.de>'

New submission
```
According to [this stackoverflow](https://stackoverflow.com/questions/23829978/checking-cran-incoming-feasibility-note-maintainer) this note can be ignored if the maintainer is specified correctly.

```
Possibly mis-spelled words in DESCRIPTION:
  KorAP (3:8)
  KorAP's (7:42)
```
KorAP is the acronym of the project. It's spelled correctly.
