# Explicit developer command, from the package root:
# Rscript tests/testthat/fixtures/http-pilot/generate-workflows.R
# Records only a local webfakes app, never a KorAP server or user credentials.
generateWorkflows <- function() {
  pkgload::load_all(quiet = TRUE)
  directory <- normalizePath("tests/testthat/fixtures/http-pilot")
  temporary <- tempfile("workflow-recording-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  app <- webfakes::new_app()
  app$use(function(req, res) {
    sendJson <- function(body) {
      res$set_header("Content-Type", "application/json")
      res$send(jsonlite::toJSON(body, auto_unbox = TRUE))
    }
    path <- req$path
    if (grepl("/authorize$", path)) return(res$send("Authorization supported"))
    if (grepl("/api/v1.0/$", path)) {
      res$set_header("x-index-revision", "workflow-index-1")
      return(res$send("Synthetic KorAP workflow server"))
    }
    if (grepl("/statistics$", path)) {
      return(sendJson(list(tokens = 1000000L, documents = 1000L, sentences = 10000L, paragraphs = 5000L)))
    }
    if (grepl("/corpus/", path)) {
      if (!is.null(req$query$foundry)) {
        return(sendJson(list(snippet = paste0(
          '<span class="context-left"><span title="tt/l:ein tt/p:ART">Ein</span></span>',
          '<span class="match"><mark><span title="tt/l:Test tt/p:NN marmot/m:case:gen|gender:fem|number:sg">Test</span></mark></span>',
          '<span class="context-right"><span title="tt/l:folgen tt/p:VVFIN">folgt</span></span>'))))
      }
      sigle <- utils::URLdecode(sub(".*/corpus/", "", path))
      fields <- list(list(key = "textSigle", value = list(sigle)),
        list(key = "textType", value = list("Synthetic text")))
      return(sendJson(list(document = list(fields = fields))))
    }
    if (!grepl("/search$", path)) return(res$set_status(404L)$send("Unknown synthetic endpoint"))
    query <- req$query
    q <- if (is.null(query$q)) "" else query$q
    count <- if (is.null(query$count)) 0L else as.integer(query$count)
    offset <- if (is.null(query$offset)) 0L else as.integer(query$offset)
    total <- if (grepl("XXXXXXXX", q, fixed = TRUE)) 0L else if (q == "geht") 1200L else
      if (grepl("Newstickeritis", q, fixed = TRUE) || grepl("contains(", q, fixed = TRUE)) 6L else
      if (q %in% c("Test", "der", "Ameisenplage", "Grund", "laufen", "[tt/l=laufen]")) 200L else 1000L
    n <- min(count, max(0L, total - offset))
    matches <- lapply(seq_len(n), function(i) {
      sigle <- sprintf("WORK/2023/%05d", offset + i)
      word <- if (grepl("Newstickeritis", q, fixed = TRUE)) "Newstickeritis" else "Test"
      field <- function(key, value) list(key = key, type = "type:string", value = list(value))
      list(matchID = sprintf("match-%s-p%d-%d", sigle, 1000L + i, 1001L + i),
        fields = list(field("textSigle", sigle), field("pubDate", "2023-01-01"),
          field("corpusTitle", "Synthetic workflow corpus")),
        tokens = list(left = as.list(rep("reine", 5)), match = list(word), right = as.list(rep("Beispiel", 5))),
        snippet = paste0('reine <mark>', word, '</mark> Beispiel'))
    })
    sendJson(list(meta = list(totalResults = total, startIndex = offset,
      itemsPerPage = n, benchmark = "0.0s"), matches = matches))
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  base <- server$url()
  stopifnot(grepl("^http://127[.]0[.]0[.]1:[0-9]+/$", base))
  nativeFetch <- curl::curl_fetch_memory
  testthat::local_mocked_bindings(has_internet = function() TRUE,
    curl_fetch_memory = function(url, ...) {
      if (!startsWith(url, base)) stop("Fixture recording allows only the local workflow server.")
      nativeFetch(url, ...)
    }, .package = "curl")
  testthat::local_mocked_bindings(default_backend = function() stop("OS keyring disabled during recording"), .package = "keyring")
  env <- testthat::test_env("RKorAPClient")
  sys.source("tests/testthat/helper-http-fixtures.R", env)
  replay <- env$withKorapFixtureApi
  factory <- env$fixtureKorapConnection
  recording <- FALSE
  env$fixtureKorapConnection <- function(cache = FALSE, verbose = FALSE) {
    if (!recording) return(factory(cache = cache, verbose = verbose))
    KorAPConnection(KorAPUrl = base, accessToken = NULL, cache = cache, verbose = verbose)
  }
  env$withKorapFixtureApi <- function(code, cassette = "connection") {
    if (!startsWith(cassette, "workflow-")) return(replay(code, cassette))
    recording <<- TRUE
    on.exit(recording <<- FALSE, add = TRUE)
    state <- new.env(parent = emptyenv())
    state$requests <- character()
    vcr::local_vcr_configure(ignore_hosts = NULL, ignore_localhost = FALSE, turned_off = FALSE,
      re_record_interval = NULL, match_requests_on = c("method", "uri"), serialize_with = "json")
    vcr::use_cassette(cassette, result <- code(state), dir = temporary, record = "all")
    file <- file.path(temporary, paste0(cassette, ".json"))
    saved <- jsonlite::read_json(file)
    for (i in seq_along(saved$http_interactions)) {
      entry <- saved$http_interactions[[i]]
      entry$request$uri <- sub(base, "https://korap.example.invalid/", entry$request$uri, fixed = TRUE)
      entry$recorded_at <- "2020-01-01 00:00:00"
      entry$response$headers$date <- "Wed, 01 Jan 2020 00:00:00 GMT"
      saved$http_interactions[[i]] <- entry
    }
    jsonlite::write_json(saved, file.path(directory, basename(file)), auto_unbox = TRUE, pretty = TRUE)
    result
  }
  filter <- commandArgs(trailingOnly = TRUE)
  if (!length(filter)) filter <- "^(collocations|corpusStats-eta|corpusquery-eta|page-numbering|vc-labels|cache-as|demos|http-content)$"
  testthat::set_max_fails(Inf)
  testthat::test_dir("tests/testthat", filter = filter[[1]], env = env, load_helpers = FALSE,
    reporter = "summary", package = "RKorAPClient", stop_on_failure = TRUE)
}

generateWorkflows()
