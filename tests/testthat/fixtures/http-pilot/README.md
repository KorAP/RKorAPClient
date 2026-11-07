# Offline HTTP pilot fixtures

These are synthetic responses in the KorAP API format, replayed by `vcr`.
They contain no copied corpus texts, access tokens or other credentials.
Each JSON file stores one scenario's request URLs, response headers and bodies
in vcr's cassette format. Matching uses the HTTP method and complete URI. Tests use
`record = "none"`; missing interactions never trigger recording or real requests.

The welcome response includes an index-revision header. The authorization probe
returns HTTP 200. A count query reports 51 hits, and two result pages contain 50
and 1 matches using the fields-array metadata representation. A separate query
has zero hits. All texts and match IDs are artificial.
Two statistics responses provide known document, token, sentence and paragraph
counts for the whole corpus and a date-restricted subcorpus.
The fetch cassette contains the continuation followed by `fetchAll()`; the
workflow cassette uses the same positive assertions as the live test, including
a final single-hit page. Separate small cassettes cover connection errors,
cache reuse, warnings and statistics. vcr consumes each matching interaction
once within a cassette context. Per-scenario cassettes avoid scanning unrelated
requests during matching.

To regenerate the same files without accessing a server, run from the package root:

```sh
Rscript tests/testthat/fixtures/http-pilot/generate.R
```

Fixture lookup is strict: missing files fail rather than recording a real request.
The tests keep `apiCall()`, request construction, JSON parsing and fetching active.
