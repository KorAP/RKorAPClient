## Test environments
* local Fedora 31 with R version 3.6.3
* CentOS 7.7
* win builder (release)
* R-hub builder

## local R CMD check results
```
── R CMD check results ───────────────────────────────── RKorAPClient 0.5.1 ────
Duration: 1m 16.2s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## R-hub builder
### NOTE 1
```
Possibly mis-spelled words in DESCRIPTION:
  KorAP (3:8)
  KorAP's (7:42)
  1. Redistributions of source code must retain the above copyright notice,
File 'LICENSE':
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
```
KorAP is the acronym of the project. It's spelled correctly. The rest, too.

### NOTE 2
```
* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
                                   user system elapsed
hc_freq_by_year_ci                 0.21   0.33    7.04
KorAPQuery-class                   0.31   0.04    5.36
corpusQuery-KorAPConnection-method 0.14   0.02    5.39
ggplotly                           0.64   0.17   15.35
** found \donttest examples: check also with --run-donttest
```
I have reduced the tests as much as possible with combinations of  `\dontshow` and `\donttest`. 
The rest is absolutely necessary and useful. The times vary a bit depending on the load of the server.
