# Offline HTTP pilot fixtures

These are synthetic responses in the KorAP API format, replayed by `vcr`.
They contain no copied corpus texts, access tokens or other credentials.
`http-pilot.json` stores request URLs, response headers and bodies in vcr's
cassette format. Matching uses the HTTP method and complete URI. Tests use
`record = "none"`; missing interactions never trigger recording or real requests.

The welcome response includes an index-revision header. The authorization probe
returns HTTP 200. A count query reports 51 hits, and two result pages contain 50
and 1 matches using the fields-array metadata representation. A separate query
has zero hits. All texts and match IDs are artificial.
Two statistics responses provide known document, token, sentence and paragraph
counts for the whole corpus and a date-restricted subcorpus.
Each result page occurs twice for the continuation followed by `fetchAll()`;
vcr consumes each matching interaction once within a cassette context.

To regenerate the same files without accessing a server, run from the package root:

```sh
Rscript tests/testthat/fixtures/http-pilot/generate.R
```

Fixture lookup is strict: missing files fail rather than recording a real request.
The tests keep `apiCall()`, request construction, JSON parsing and fetching active.
