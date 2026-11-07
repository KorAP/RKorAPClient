# Synthetic HTTP fixtures

These vcr JSON cassettes contain artificial KorAP responses, with no copied
corpus texts or credentials. Tests match HTTP method and full URI using
`record = "none"`; missing interactions fail without recording or live fallback.
Per-scenario cassettes limit matching overhead. Repeated requests have separate
interactions because vcr consumes each interaction once per cassette context.

`generate.R` writes connection, query, statistics, metadata and annotation
scenarios directly. Welcome responses include index revisions; results include
fields-array metadata, pagination, token contexts and extended KED fields.
Separate cases cover failures, warnings, cache reuse and fixed-seed page order.

`generate-workflows.R` records the remaining workflows against a temporary local
`webfakes` server containing synthetic search results, statistics and annotation
XML. Only that process's loopback URL is allowed during recording. Request URLs
are normalized to `https://korap.example.invalid/` and timestamps are fixed.
The server is stopped afterwards; no public KorAP server is contacted.

Regenerate from the package root with the suggested development dependencies
installed:

```sh
Rscript tests/testthat/fixtures/http-pilot/generate.R
Rscript tests/testthat/fixtures/http-pilot/generate-workflows.R
```

Inspect cassette diffs before committing. Regeneration is explicit and never
runs as part of the standard tests or package checks. Tests retain the real
`apiCall()`, request construction, JSON parsing and fetching code paths.
