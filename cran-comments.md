## Test environments
* local Fedora 31 with R version 3.6.3
* CentOS 7.7
* win builder (release)
* R-hub builder

## local R CMD check results
```
── R CMD check results ───────────────────────────────── RKorAPClient 0.5.5 ────
Duration: 45.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## R-hub builder

```
── RKorAPClient 0.5.5: NOTE

  Build ID:   RKorAPClient_0.5.5.tar.gz-ed1ae575c5624fe183feac9af9530208
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  12m 33.8s ago
  Build time: 12m 13.4s

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marc Kupietz <kupietz@ids-mannheim.de>'
  
  New submission

0 errors ✓ | 0 warnings ✓ | 1 note x
```

## CRAN comments on issues in previous version

> The Description field is intended to be a (one paragraph) description
of what the package does and why it may be useful. Please elaborate.

The Description field is now elaborated accordingly.

> If there is a web reference please add it to the description of the 
DESCRIPTION file in the form <https:.....>
with no space after 'https:' and angle brackets for auto-linking.

The web reference is also added.

> Please always add all authors and copyright holders in the Authors@R 
field with the appropriate roles.
 From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all 
components of the package must be clear and unambiguous (including from 
the authors specification in the DESCRIPTION file). Where code is copied 
(or derived) from the work of others (including from R itself), care 
must be taken that any copyright/license statements are preserved and 
authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the 
authors of such code. Alternatively, the ‘Author’ field should list 
these authors as contributors.
Where copyrights are held by an entity other than the package authors, 
this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’ 
field, or using a ‘Copyright’ field (if necessary referring to an 
inst/COPYRIGHTS file)."
e.g.: IDS Mannheim, also KorAP-Team seems not specific enough.
Please explain in the submission comments what you did about this issue.

I have added Nils Diewald as the only contributor as of now (role: ctb) instead of using the generic KorAP-Team <korap@ids-mannheim.de> and expanded the informal copyright holder (role: cph) name 'IDS Mannheim' to the official name 'Leibniz Instute for the German Language'. There is no code copied or derived from other authors or rights holders.

> Please write TRUE and FALSE instead of T and F.
(Please don't use "T" or "F" as vector names.)

Every occurence of T and F is now replaced with TRUE and FALSE, respectively and  T or F are not used as vector names.
