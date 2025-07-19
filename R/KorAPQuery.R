#' KorAPQuery class (internal)
#'
#' Internal class for query state management. Users work with `corpusQuery()`, `fetchAll()`, and `fetchNext()` instead.
#'
#' @keywords internal
#' @include KorAPConnection.R
#' @include logging.R
#' @import httr2
#'
#' @include RKorAPClient-package.R

#' @export
KorAPQuery <- setClass("KorAPQuery", slots = c(
  "korapConnection",
  "request",
  "vc",
  "totalResults",
  "nextStartIndex",
  "fields",
  "requestUrl",
  "webUIRequestUrl",
  "apiResponse",
  "collectedMatches",
  "hasMoreMatches"
))

#' Initialize KorAPQuery object
#' @keywords internal
#' @param .Object …
#' @param korapConnection KorAPConnection object
#' @param request query part of the request URL
#' @param vc definition of a virtual corpus
#' @param totalResults number of hits the query has yielded
#' @param nextStartIndex at what index to start the next fetch of query results
#' @param fields what data / metadata fields should be collected
#' @param requestUrl complete URL of the API request
#' @param webUIRequestUrl URL of a web frontend request corresponding to the API request
#' @param apiResponse data-frame representation of the JSON response of the API request
#' @param hasMoreMatches logical that signals if more query results can be fetched
#' @param collectedMatches matches already fetched from the KorAP-API-server
#'
#' @importFrom tibble tibble
#' @export
setMethod(
  "initialize", "KorAPQuery",
  function(.Object, korapConnection = NULL, request = NULL, vc = "", totalResults = 0, nextStartIndex = 0, fields = c(
             "corpusSigle", "textSigle", "pubDate", "pubPlace",
             "availability", "textClass", "snippet", "tokens"
           ),
           requestUrl = "", webUIRequestUrl = "", apiResponse = NULL, hasMoreMatches = FALSE, collectedMatches = NULL) {
    .Object <- callNextMethod()
    .Object@korapConnection <- korapConnection
    .Object@request <- request
    .Object@vc <- vc
    .Object@totalResults <- totalResults
    .Object@nextStartIndex <- nextStartIndex
    .Object@fields <- fields
    .Object@requestUrl <- requestUrl
    .Object@webUIRequestUrl <- webUIRequestUrl
    .Object@apiResponse <- apiResponse
    .Object@hasMoreMatches <- hasMoreMatches
    .Object@collectedMatches <- collectedMatches
    .Object
  }
)

setGeneric("corpusQuery", function(kco, ...) standardGeneric("corpusQuery"))
setGeneric("fetchAll", function(kqo, ...) standardGeneric("fetchAll"))
setGeneric("fetchNext", function(kqo, ...) standardGeneric("fetchNext"))
setGeneric("fetchRest", function(kqo, ...) standardGeneric("fetchRest"))
setGeneric("fetchAnnotations", function(kqo, ...) standardGeneric("fetchAnnotations"))
setGeneric("frequencyQuery", function(kco, ...) standardGeneric("frequencyQuery"))

maxResultsPerPage <- 50

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c("."))

#' Search corpus for query terms
#'
#' **`corpusQuery`** performs a corpus query via a connection to a KorAP-API-server
#'
#' @family corpus search functions
#' @aliases corpusQuery
#'
#' @importFrom urltools url_encode
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows group_by
#'
#' @param kco [KorAPConnection()] object (obtained e.g. from `KorAPConnection()`
#' @param query string that contains the corpus query. The query language depends on the `ql` parameter. Either `query` must be provided or `KorAPUrl`.
#' @param vc string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.
#' @param KorAPUrl instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in `KorAPConnection`) to provide all necessary information for the query.
#' @param metadataOnly logical that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE.
#'    If you want your corpus queries to return not only metadata, but also KWICS, you need to authorize
#'    your RKorAPClient application as explained in the
#'   [authorization section](https://github.com/KorAP/RKorAPClient#authorization)
#'   of the RKorAPClient Readme on GitHub and set the `metadataOnly` parameter to
#'   `FALSE`.
#' @param ql string to choose the query language (see [section on Query Parameters](https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters) in the Kustvakt-Wiki for possible values.
#' @param fields character vector specifying which metadata fields to retrieve for each match.
#' Available fields depend on the corpus. For DeReKo (German Reference Corpus), possible fields include:
#' \describe{
#'   \item{**Text identification**:}{`textSigle`, `docSigle`, `corpusSigle` - hierarchical text identifiers}
#'   \item{**Publication info**:}{`author`, `editor`, `title`, `docTitle`, `corpusTitle` - authorship and titles}
#'   \item{**Temporal data**:}{`pubDate`, `creationDate` - when text was published/created}
#'   \item{**Publication details**:}{`pubPlace`, `publisher`, `reference` - where/how published}
#'   \item{**Text classification**:}{`textClass`, `textType`, `textTypeArt`, `textDomain`, `textColumn` - topic domain, genre, text type and column}
#'   \item{**Adminstrative and technical info**:}{`corpusEditor`, `availability`, `language`, `foundries` - access rights and annotations}
#'   \item{**Content data**:}{`snippet`, `tokens`, `tokenSource`, `externalLink` - actual text content, tokenization, and link to source text}
#'   \item{**System data**:}{`indexCreationDate`, `indexLastModified` - corpus indexing info}
#' }
#' Use `c("textSigle", "pubDate", "author")` to retrieve multiple fields.
#' Default fields provide basic text identification and publication metadata. The actual text content (`snippet` and `tokens`) are activated by default  if `metadataOnly` is set to `FALSE`.
#' @param accessRewriteFatal abort if query or given vc had to be rewritten due to insufficient rights (not yet implemented).
#' @param verbose print some info
#' @param as.df return result as data frame instead of as S4 object?
#' @param expand logical that decides if `query` and `vc` parameters are expanded to all of their combinations. Defaults to `TRUE`, iff `query` and `vc` have different lengths
#' @param context string that specifies the size of the left and the right context returned in `snippet`
#'        (provided that `metadataOnly` is set to `false` and that the necessary access right are  met).
#'        The format of the context size specifcation (e.g. `3-token,3-token`) is described in the [Service: Search GET documentation of the Kustvakt Wiki](https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET).
#'        If the parameter is not set, the default context size secification of the KorAP server instance will be used.
#'        Note that you cannot overrule the maximum context size set in the KorAP server instance,
#'        as this is typically legally motivated.
#' @return Depending on the `as.df` parameter, a tibble or a [KorAPQuery()] object that, among other information, contains the total number of results in `@totalResults`. The resulting object can be used to fetch all query results (with [fetchAll()]) or the next page of results (with [fetchNext()]).
#' A corresponding URL to be used within a web browser is contained in `@webUIRequestUrl`
#' Please make sure to check `$collection$rewrites` to see if any unforeseen access rewrites of the query's virtual corpus had to be performed.
#'
#' @examples
#' \dontrun{
#'
#' # Fetch basic metadata for "Ameisenplage"
#' KorAPConnection() |>
#'   corpusQuery("Ameisenplage") |>
#'   fetchAll()
#'
#' # Fetch specific metadata fields for bibliographic analysis
#' query <- KorAPConnection() |>
#'   corpusQuery("Ameisenplage",
#'               fields = c("textSigle", "author", "title", "pubDate", "pubPlace", "textType"))
#' results <- fetchAll(query)
#' results@collectedMatches
#' }
#'
#' \dontrun{
#'
#' # Use the copy of a KorAP-web-frontend URL for an API query of "Ameise" in a virtual corpus
#' # and show the number of query hits (but don't fetch them).
#'
#' KorAPConnection(verbose = TRUE) |>
#'   corpusQuery(
#'     KorAPUrl =
#'       "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp"
#'   )
#' }
#'
#' \dontrun{
#'
#' # Plot the time/frequency curve of "Ameisenplage"
#' KorAPConnection(verbose = TRUE) |>
#'   {
#'     . ->> kco
#'   } |>
#'   corpusQuery("Ameisenplage") |>
#'   fetchAll() |>
#'   slot("collectedMatches") |>
#'   mutate(year = lubridate::year(pubDate)) |>
#'   dplyr::select(year) |>
#'   group_by(year) |>
#'   summarise(Count = dplyr::n()) |>
#'   mutate(Freq = mapply(function(f, y) {
#'     f / corpusStats(kco, paste("pubDate in", y))@tokens
#'   }, Count, year)) |>
#'   dplyr::select(-Count) |>
#'   complete(year = min(year):max(year), fill = list(Freq = 0)) |>
#'   plot(type = "l")
#' }
#' @seealso [KorAPConnection()], [fetchNext()], [fetchRest()], [fetchAll()], [corpusStats()]
#'
#' @references
#' <https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026>
#'
#' @export
setMethod(
  "corpusQuery", "KorAPConnection",
  function(kco,
           query = if (missing(KorAPUrl)) {
             stop("At least one of the parameters query and KorAPUrl must be specified.", call. = FALSE)
           } else {
             httr2::url_parse(KorAPUrl)$query$q
           },
           vc = if (missing(KorAPUrl)) "" else httr2::url_parse(KorAPUrl)$query$cq,
           KorAPUrl,
           metadataOnly = TRUE,
           ql = if (missing(KorAPUrl)) "poliqarp" else httr2::url_parse(KorAPUrl)$query$ql,
           fields = c(
             "corpusSigle",
             "textSigle",
             "pubDate",
             "pubPlace",
             "availability",
             "textClass",
             "snippet",
             "tokens"
           ),
           accessRewriteFatal = TRUE,
           verbose = kco@verbose,
           expand = length(vc) != length(query),
           as.df = FALSE,
           context = NULL) {
    if (length(query) > 1 || length(vc) > 1) {
      grid <- if (expand) expand_grid(query = query, vc = vc) else tibble(query = query, vc = vc)

      # Initialize timing variables for ETA calculation
      total_queries <- nrow(grid)
      current_query <- 0
      start_time <- Sys.time()

      results <- purrr::pmap(grid, function(query, vc, ...) {
        current_query <<- current_query + 1

        # Execute the single query directly (avoiding recursive call)
        contentFields <- c("snippet", "tokens")
        query_fields <- fields
        if (metadataOnly) {
          query_fields <- query_fields[!query_fields %in% contentFields]
        }
        if (!"textSigle" %in% query_fields) {
          query_fields <- c(query_fields, "textSigle")
        }
        request <-
          paste0(
            "?q=",
            url_encode(enc2utf8(query)),
            ifelse(!metadataOnly && !is.null(context) && context != "", paste0("&context=", url_encode(enc2utf8(context))), ""),
            ifelse(vc != "", paste0("&cq=", url_encode(enc2utf8(vc))), ""),
            ifelse(!metadataOnly, "&show-tokens=true", ""),
            "&ql=", ql
          )
        webUIRequestUrl <- paste0(kco@KorAPUrl, request)
        requestUrl <- paste0(
          kco@apiUrl,
          "search",
          request,
          "&fields=",
          paste(query_fields, collapse = ","),
          if (metadataOnly) "&access-rewrite-disabled=true" else ""
        )

        # Show individual query progress
        log_info(verbose, "\rSearching \"", query, "\" in \"", vc, "\"", sep = "")
        res <- apiCall(kco, paste0(requestUrl, "&count=0"))
        if (is.null(res)) {
          log_info(verbose, ": API call failed\n")
          totalResults <- 0
        } else {
          totalResults <- as.integer(res$meta$totalResults)
          log_info(verbose, ": ", totalResults, " hits")
          if (!is.null(res$meta$cached)) {
            log_info(verbose, " [cached]")
          } else if (!is.null(res$meta$benchmark)) {
            if (is.character(res$meta$benchmark) && grepl("s$", res$meta$benchmark)) {
              time_value <- as.numeric(sub("s$", "", res$meta$benchmark))
              formatted_time <- paste0(round(time_value, 2), "s")
              log_info(verbose, ", took ", formatted_time)
            } else {
              log_info(verbose, ", took ", res$meta$benchmark)
            }
          }

          # Calculate and display ETA information on the same line if verbose and we have more than one query
          if (verbose && total_queries > 1) {
            eta_info <- calculate_eta(current_query, total_queries, start_time)
            if (eta_info != "") {
              elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
              avg_time_per_query <- elapsed_time / current_query

              # Add ETA info to the same line - remove the leading ". " for cleaner formatting
              clean_eta_info <- sub("^\\. ", ". ", eta_info)
              log_info(verbose, clean_eta_info)
            }
          }

          log_info(verbose, "\n")
        }

        result <- data.frame(
          query = query,
          totalResults = totalResults,
          vc = vc,
          webUIRequestUrl = webUIRequestUrl,
          stringsAsFactors = FALSE
        )

        return(result)
      })

      results %>% bind_rows()
    } else {
      contentFields <- c("snippet", "tokens")
      if (metadataOnly) {
        fields <- fields[!fields %in% contentFields]
      }
      if (!"textSigle" %in% fields) {
        fields <- c(fields, "textSigle")
      }
      request <-
        paste0(
          "?q=",
          url_encode(enc2utf8(query)),
          ifelse(!metadataOnly && !is.null(context) && context != "", paste0("&context=", url_encode(enc2utf8(context))), ""),
          ifelse(vc != "", paste0("&cq=", url_encode(enc2utf8(vc))), ""),
          ifelse(!metadataOnly, "&show-tokens=true", ""),
          "&ql=", ql
        )
      webUIRequestUrl <- paste0(kco@KorAPUrl, request)
      requestUrl <- paste0(
        kco@apiUrl,
        "search",
        request,
        "&fields=",
        paste(fields, collapse = ","),
        if (metadataOnly) "&access-rewrite-disabled=true" else ""
      )
      log_info(verbose, "\rSearching \"", query, "\" in \"", vc, "\"",
        sep =
          ""
      )
      res <- apiCall(kco, paste0(requestUrl, "&count=0"))
      if (is.null(res)) {
        message("API call failed.")
        totalResults <- 0
      } else {
        totalResults <- as.integer(res$meta$totalResults)
        log_info(verbose, ": ", totalResults, " hits")
        if (!is.null(res$meta$cached)) {
          log_info(verbose, " [cached]\n")
        } else if (!is.null(res$meta$benchmark)) {
          # Round the benchmark time to 2 decimal places for better readability
          # If it's a string ending with 's', extract the number, round it, and re-add 's'
          if (is.character(res$meta$benchmark) && grepl("s$", res$meta$benchmark)) {
            time_value <- as.numeric(sub("s$", "", res$meta$benchmark))
            formatted_time <- paste0(round(time_value, 2), "s")
            log_info(verbose, ", took ", formatted_time, "\n", sep = "")
          } else {
            # Fallback if the format is different than expected
            log_info(verbose, ", took ", res$meta$benchmark, "\n", sep = "")
          }
        } else {
          log_info(verbose, "\n")
        }
      }
      if (as.df) {
        data.frame(
          query = query,
          totalResults = totalResults,
          vc = vc,
          webUIRequestUrl = webUIRequestUrl,
          stringsAsFactors = FALSE
        )
      } else {
        KorAPQuery(
          korapConnection = kco,
          nextStartIndex = 0,
          fields = fields,
          requestUrl = requestUrl,
          request = request,
          totalResults = totalResults,
          vc = vc,
          apiResponse = res,
          webUIRequestUrl = webUIRequestUrl,
          hasMoreMatches = (totalResults > 0),
        )
      }
    }
  }
)

#' @importFrom purrr map
repair_data_strcuture <- function(x) {
  if (is.list(x)) {
    as.character(purrr::map(x, ~ if (length(.x) > 1) {
      paste(.x, collapse = " ")
    } else {
      .x
    }))
  } else {
    ifelse(is.na(x), "", x)
  }
}

#' Fetch the next bunch of results of a KorAP query.
#'
#' **`fetchNext`** fetches the next bunch of results of a KorAP query.
#'
#' @family corpus search functions
#'
#' @param kqo object obtained from [corpusQuery()]
#' @param offset start offset for query results to fetch
#' @param maxFetch maximum number of query results to fetch
#' @param verbose print progress information if true
#' @param randomizePageOrder fetch result pages in pseudo random order if true. Use [set.seed()] to set seed for reproducible results.
#' @return The `kqo` input object with updated slots `collectedMatches`, `apiResponse`, `nextStartIndex`, `hasMoreMatches`
#'
#' @examples
#' \dontrun{
#'
#' q <- KorAPConnection() |>
#'   corpusQuery("Ameisenplage") |>
#'   fetchNext()
#' q@collectedMatches
#' }
#'
#' @references
#' <https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026>
#'
#' @aliases fetchNext
#' @importFrom dplyr rowwise mutate bind_rows select summarise n select
#' @importFrom tibble enframe add_column
#' @importFrom stringr word
#' @importFrom tidyr unnest unchop pivot_wider
#' @importFrom purrr map
#' @export
setMethod("fetchNext", "KorAPQuery", function(kqo,
                                              offset = kqo@nextStartIndex,
                                              maxFetch = maxResultsPerPage,
                                              verbose = kqo@korapConnection@verbose,
                                              randomizePageOrder = FALSE) {
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  results <- key <- name <- tmp_positions <- 0

  if (kqo@totalResults == 0 || offset >= kqo@totalResults) {
    return(kqo)
  }
  use_korap_api <- Sys.getenv("USE_KORAP_API", unset = NA)
  # Calculate the initial page number (not used directly - keeping for reference)
  collectedMatches <- kqo@collectedMatches

  # Track start time for ETA calculation
  start_time <- Sys.time()

  # For randomized page order, generate a list of randomized page indices
  if (randomizePageOrder) {
    # Calculate how many pages we need to fetch based on maxFetch
    total_pages_to_fetch <- if (!is.na(maxFetch)) {
      # Either limited by maxFetch or total results, whichever is smaller
      min(ceiling(maxFetch / maxResultsPerPage), ceiling(kqo@totalResults / maxResultsPerPage))
    } else {
      # All pages
      ceiling(kqo@totalResults / maxResultsPerPage)
    }

    # Generate randomized page indices (0-based for API)
    pages <- sample.int(ceiling(kqo@totalResults / maxResultsPerPage), total_pages_to_fetch) - 1
    page_index <- 1 # Index to track which page in the randomized list we're on
  }

  if (is.null(collectedMatches)) {
    collectedMatches <- data.frame()
  }

  # Initialize the page counter properly based on nextStartIndex and any previously fetched results
  # We add 1 to make it 1-based for display purposes since users expect page numbers to start from 1
  # For first call, this will be 1, for subsequent calls, it will reflect our actual position
  current_page_number <- ceiling(offset / maxResultsPerPage) + 1

  # For sequential fetches, keep track of which global page we're on
  # This is important for correctly showing page numbers in subsequent fetchNext calls
  page_count_start <- current_page_number

  repeat {
    # Determine which page to fetch next
    if (randomizePageOrder) {
      # In randomized mode, get the page from our randomized list using the page_index
      # Make sure we don't exceed the array bounds
      if (page_index > length(pages)) {
        break # No more pages to fetch in randomized mode
      }
      current_offset_page <- pages[page_index]
      # For display purposes in randomized mode, show which page out of the total we're fetching
      display_page_number <- page_index
    } else {
      # In sequential mode, use the current_page_number to calculate the offset
      current_offset_page <- (current_page_number - 1)
      display_page_number <- current_page_number
    }

    # Calculate the actual offset in tokens
    currentOffset <- current_offset_page * maxResultsPerPage

    # Build the query with the appropriate count and offset using httr2
    count_param <- min(if (!is.na(maxFetch)) maxFetch - results else maxResultsPerPage, maxResultsPerPage)

    # Parse existing URL to preserve all query parameters
    parsed_url <- httr2::url_parse(kqo@requestUrl)
    existing_query <- parsed_url$query

    # Add/update count and offset parameters
    existing_query$count <- count_param
    existing_query$offset <- currentOffset

    # Rebuild the URL with all parameters
    query <- httr2::url_modify(kqo@requestUrl, query = existing_query)
    res <- apiCall(kqo@korapConnection, query)
    if (length(res$matches) == 0) {
      break
    }

    if ("fields" %in% colnames(res$matches) && (is.na(use_korap_api) || as.numeric(use_korap_api) >= 1.0)) {
      log_info(verbose, "Using fields API: ")
      currentMatches <- res$matches$fields %>%
        purrr::map(~ mutate(.x, value = repair_data_strcuture(value))) %>%
        tibble::enframe() %>%
        tidyr::unnest(cols = value) %>%
        tidyr::pivot_wider(names_from = key, id_cols = name, names_repair = "unique") %>%
        dplyr::select(-name)
      if ("snippet" %in% colnames(res$matches)) {
        currentMatches$snippet <- res$matches$snippet
      }
      if ("tokens" %in% colnames(res$matches)) {
        currentMatches$tokens <- res$matches$tokens
      }
    } else {
      currentMatches <- res$matches
    }

    for (field in kqo@fields) {
      if (!field %in% colnames(currentMatches)) {
        currentMatches[, field] <- NA
      }
    }
    currentMatches <- currentMatches %>%
      select(kqo@fields) %>%
      mutate(
        matchID = res$matches$matchID,
        tmp_positions = gsub(".*-p(\\d+)-(\\d+).*", "\\1 \\2", res$matches$matchID),
        matchStart = as.integer(stringr::word(tmp_positions, 1)),
        matchEnd = as.integer(stringr::word(tmp_positions, 2)) - 1
      ) %>%
      select(-tmp_positions)

    if (!is.list(collectedMatches)) {
      collectedMatches <- currentMatches
    } else {
      collectedMatches <- bind_rows(collectedMatches, currentMatches)
    }

    # Get the actual items per page from the API response
    # We now consistently use maxResultsPerPage instead

    # Calculate total pages consistently using fixed maxResultsPerPage
    # This ensures consistent page counting across the function
    total_pages <- ceiling(kqo@totalResults / maxResultsPerPage)

    # Calculate ETA using the centralized function from logging.R
    current_page <- if (randomizePageOrder) page_index else display_page_number
    total_pages_to_fetch <- if (!is.na(maxFetch)) {
      # Account for offset - we can only fetch from the remaining results after offset
      remaining_results_after_offset <- max(0, kqo@totalResults - offset)
      min(ceiling(maxFetch / maxResultsPerPage), ceiling(remaining_results_after_offset / maxResultsPerPage))
    } else {
      total_pages
    }

    eta_info <- calculate_eta(current_page, total_pages_to_fetch, start_time)

    # Extract timing information for display
    time_per_page <- NA
    if (!is.null(res$meta$benchmark) && is.character(res$meta$benchmark)) {
      time_per_page <- suppressWarnings(as.numeric(sub("s", "", res$meta$benchmark)))
    }

    # Create the page display string with proper formatting

    # For global page tracking, calculate the absolute page number
    actual_display_number <- if (randomizePageOrder) {
      current_offset_page + 1 # In randomized mode, this is the actual page (0-based + 1)
    } else {
      # In sequential mode, the absolute page number is the actual offset page + 1 (to make it 1-based)
      current_offset_page + 1
    }

    # For subsequent calls to fetchNext, we need to calculate the correct page numbers
    # based on the current batch being fetched

    # For each call to fetchNext, we want to show 1/2, 2/2 (not 3/4, 4/4)
    # Simply count from 1 within the current batch

    # The relative page number is simply the current position in this batch
    if (randomizePageOrder) {
      relative_page_number <- page_index # In randomized mode, we start from 1 in each batch
    } else {
      relative_page_number <- display_page_number - (page_count_start - 1)
    }

    # How many pages will we fetch in this batch?
    # If maxFetch is specified, calculate the total pages for this fetch operation
    pages_in_this_batch <- if (!is.na(maxFetch)) {
      # Account for offset - we can only fetch from the remaining results after offset
      remaining_results_after_offset <- max(0, kqo@totalResults - offset)
      min(ceiling(maxFetch / maxResultsPerPage), ceiling(remaining_results_after_offset / maxResultsPerPage))
    } else {
      # Otherwise fetch all remaining pages
      total_pages - page_count_start + 1
    }

    # The total pages to be shown in this batch
    batch_total_pages <- pages_in_this_batch

    page_display <- paste0(
      "Retrieved page ",
      sprintf(paste0("%", nchar(batch_total_pages), "d"), relative_page_number),
      "/",
      sprintf("%d", batch_total_pages)
    )

    # If randomized, also show which actual page we fetched
    if (randomizePageOrder) {
      # Determine the maximum width needed for page numbers (based on total pages)
      # This ensures consistent alignment
      max_page_width <- nchar(as.character(total_pages))
      # Add the actual page number that was fetched (0-based + 1 for display) with proper padding
      page_display <- paste0(
        page_display,
        sprintf(" (actual page %*d)", max_page_width, current_offset_page + 1)
      )
    }
    # Always show the absolute page number and total pages (for clarity)
    else {
      # Show the absolute page number (out of total possible pages)
      page_display <- paste0(page_display, sprintf(
        " (page %d of %d total)",
        actual_display_number, total_pages
      ))
    }

    # Add caching or timing information
    if (!is.null(res$meta$cached)) {
      page_display <- paste0(page_display, " [cached]")
    } else {
      page_display <- paste0(
        page_display,
        " in ",
        if (!is.na(time_per_page)) sprintf("%4.1f", time_per_page) else "?",
        "s",
        eta_info
      )
    }

    log_info(verbose, paste0(page_display, "\n"))

    # Increment the appropriate counter based on mode
    if (randomizePageOrder) {
      page_index <- page_index + 1
    } else {
      current_page_number <- current_page_number + 1
    }
    results <- results + res$meta$itemsPerPage
    if (nrow(collectedMatches) >= kqo@totalResults || (!is.na(maxFetch) && results >= maxFetch)) {
      break
    }
  }
  nextStartIndex <- min(res$meta$startIndex + res$meta$itemsPerPage, kqo@totalResults)
  KorAPQuery(
    nextStartIndex = nextStartIndex,
    korapConnection = kqo@korapConnection,
    fields = kqo@fields,
    requestUrl = kqo@requestUrl,
    request = kqo@request,
    totalResults = kqo@totalResults,
    vc = kqo@vc,
    webUIRequestUrl = kqo@webUIRequestUrl,
    hasMoreMatches = (kqo@totalResults > nextStartIndex),
    apiResponse = res,
    collectedMatches = collectedMatches
  )
})

#' Fetch all results of a KorAP query.
#'
#' **`fetchAll`** fetches all results of a KorAP query.
#'
#' @family corpus search functions
#' @param kqo object obtained from [corpusQuery()]
#' @param verbose print progress information if true
#' @param ... further arguments passed to [fetchNext()]
#' @return The updated `kqo` object with all results in `@collectedMatches`
#'
#' @examples
#' \dontrun{
#' # Fetch all metadata of every query hit for "Ameisenplage" and show a summary
#' q <- KorAPConnection() |>
#'   corpusQuery("Ameisenplage") |>
#'   fetchAll()
#' q@collectedMatches
#'
#' # Fetch also all KWICs
#' q <- KorAPConnection() |> auth() |>
#'  corpusQuery("Ameisenplage", metadataOnly = FALSE) |>
#'  fetchAll()
#' q@collectedMatches
#'
#' # Retrieve title and text sigle metadata of all texts published on 1958-03-12
#' q <- KorAPConnection() |>
#'  corpusQuery("<base/s=t>", # this matches each text once
#'     vc = "pubDate in 1958-03-12",
#'     fields = c("textSigle", "title"),
#' ) |>
#'  fetchAll()
#' q@collectedMatches
#' }
#'
#' @aliases fetchAll
#' @export
setMethod("fetchAll", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose, ...) {
  return(fetchNext(kqo, offset = 0, maxFetch = NA, verbose = verbose, ...))
})

#' Fetches the remaining results of a KorAP query.
#'
#' @param kqo object obtained from [corpusQuery()]
#' @param verbose print progress information if true
#' @param ... further arguments passed to [fetchNext()]
#' @return The updated `kqo` object with remaining results in `@collectedMatches`
#'
#' @examples
#' \dontrun{
#'
#' q <- KorAPConnection() |>
#'   corpusQuery("Ameisenplage") |>
#'   fetchRest()
#' q@collectedMatches
#' }
#'
#' @aliases fetchRest
#' @export
setMethod("fetchRest", "KorAPQuery", function(kqo, verbose = kqo@korapConnection@verbose, ...) {
  return(fetchNext(kqo, maxFetch = NA, verbose = verbose, ...))
})

#'
#' Parse XML annotations into linguistic layers
#'
#' Internal helper function to extract linguistic annotations (lemma, POS, morphology)
#' from XML annotation snippets returned by the KorAP API.
#'
#' @param xml_snippet XML string containing annotation data
#' @return Named list with vectors for 'token', 'lemma', 'pos', and 'morph'
#' @keywords internal
parse_xml_annotations <- function(xml_snippet) {
  if (is.null(xml_snippet) || is.na(xml_snippet) || xml_snippet == "") {
    return(list(token = character(0), lemma = character(0), pos = character(0), morph = character(0)))
  }

  # Extract content within <span class="match">...</span> using a more robust approach
  if (grepl('<span class="match">', xml_snippet)) {
    # Find the start of match span
    start_pos <- regexpr('<span class="match">', xml_snippet)
    if (start_pos > 0) {
      # Find the end by counting nested spans
      content_start <- start_pos + attr(start_pos, "match.length")
      remaining <- substr(xml_snippet, content_start, nchar(xml_snippet))

      # Simple approach: extract everything until we hit context-right or end
      if (grepl('<span class="context-right">', remaining)) {
        content_to_parse <- gsub('(.*?)<span class="context-right">.*', '\\1', remaining)
      } else {
        # Find the closing </span> that matches our opening span
        # For now, use a simpler approach - take everything until the last </span> sequence
        content_to_parse <- gsub('(.*)</span>\\s*$', '\\1', remaining)
      }
    } else {
      content_to_parse <- xml_snippet
    }
  } else {
    content_to_parse <- xml_snippet
  }

  # Initialize result vectors
  tokens <- character(0)
  lemmas <- character(0)
  pos_tags <- character(0)
  morph_tags <- character(0)

  # Split the content by </span> and process each meaningful part
  parts <- unlist(strsplit(content_to_parse, '</span>'))

  for (part in parts) {
    part <- trimws(part)
    if (nchar(part) == 0) next

    # Look for parts that have title attributes and end with text
    if (grepl('<span[^>]*title=', part)) {
      # Extract the text content (everything after the last >)
      text_content <- gsub('.*>([^<]*)$', '\\1', part)
      text_content <- trimws(text_content)

      if (nchar(text_content) > 0 && !grepl('^<', text_content)) {
        tokens <- c(tokens, text_content)

        # Extract all title attributes from this part
        title_pattern <- 'title="([^"]*)"'
        title_matches <- gregexpr(title_pattern, part)

        lemma <- NA
        pos_tag <- NA
        morph_tag <- NA

        if (title_matches[[1]][1] != -1) {
          all_titles <- regmatches(part, title_matches)[[1]]
          for (title_match in all_titles) {
            title_content <- gsub(title_pattern, '\\1', title_match)

            # Split by spaces and process each annotation
            annotations <- unlist(strsplit(title_content, "\\s+"))
            for (annotation in annotations) {
              if (grepl('^[^/]+/l:', annotation)) {
                lemma <- gsub('^[^/]+/l:(.*)$', '\\1', annotation)
              } else if (grepl('^[^/]+/p:', annotation)) {
                pos_tag <- gsub('^[^/]+/p:(.*)$', '\\1', annotation)
              } else if (grepl('^[^/]+/m:', annotation)) {
                morph_tag <- gsub('^[^/]+/m:(.*)$', '\\1', annotation)
              }
            }
          }
        }

        lemmas <- c(lemmas, lemma)
        pos_tags <- c(pos_tags, pos_tag)
        morph_tags <- c(morph_tags, morph_tag)
      }
    }
  }

  # If no tokens found with the splitting approach, try a different method
  if (length(tokens) == 0) {
    # Look for the innermost spans that contain actual text
    innermost_pattern <- '<span[^>]*title="([^"]*)"[^>]*>([^<]+)</span>'
    innermost_matches <- gregexpr(innermost_pattern, content_to_parse, perl = TRUE)

    if (innermost_matches[[1]][1] != -1) {
      matches <- regmatches(content_to_parse, innermost_matches)[[1]]

      for (match in matches) {
        title <- gsub(innermost_pattern, '\\1', match, perl = TRUE)
        text <- gsub(innermost_pattern, '\\2', match, perl = TRUE)
        text <- trimws(text)

        if (nchar(text) > 0) {
          tokens <- c(tokens, text)

          # Parse space-separated annotations in title
          lemma <- NA
          pos_tag <- NA
          morph_tag <- NA

          annotations <- unlist(strsplit(title, "\\s+"))
          for (annotation in annotations) {
            if (grepl('^[^/]+/l:', annotation)) {
              lemma <- gsub('^[^/]+/l:(.*)$', '\\1', annotation)
            } else if (grepl('^[^/]+/p:', annotation)) {
              pos_tag <- gsub('^[^/]+/p:(.*)$', '\\1', annotation)
            } else if (grepl('^[^/]+/m:', annotation)) {
              morph_tag <- gsub('^[^/]+/m:(.*)$', '\\1', annotation)
            }
          }

          lemmas <- c(lemmas, lemma)
          pos_tags <- c(pos_tags, pos_tag)
          morph_tags <- c(morph_tags, morph_tag)
        }
      }
    }
  }

  # Ensure all vectors have the same length
  max_length <- max(length(tokens), length(lemmas), length(pos_tags), length(morph_tags))
  if (max_length > 0) {
    tokens <- c(tokens, rep(NA, max_length - length(tokens)))
    lemmas <- c(lemmas, rep(NA, max_length - length(lemmas)))
    pos_tags <- c(pos_tags, rep(NA, max_length - length(pos_tags)))
    morph_tags <- c(morph_tags, rep(NA, max_length - length(morph_tags)))
  }

  return(list(
    token = tokens,
    lemma = lemmas,
    pos = pos_tags,
    morph = morph_tags
  ))
}

#'
#' Parse XML annotations into linguistic layers with left/match/right structure
#'
#' Internal helper function to extract linguistic annotations (lemma, POS, morphology)
#' from XML annotation snippets returned by the KorAP API, split into left context,
#' match, and right context sections like the tokens field.
#'
#' @param xml_snippet XML string containing annotation data
#' @return Named list with nested structure containing left/match/right for 'atokens', 'lemma', 'pos', and 'morph'
#' @keywords internal
parse_xml_annotations_structured <- function(xml_snippet) {
  if (is.null(xml_snippet) || is.na(xml_snippet) || xml_snippet == "") {
    empty_result <- list(left = character(0), match = character(0), right = character(0))
    return(list(
      atokens = empty_result,
      lemma = empty_result,
      pos = empty_result,
      morph = empty_result
    ))
  }

  # Helper function to extract annotations from a span section
  extract_annotations_from_section <- function(section_content) {
    # Handle both spaced tokens and nested single tokens
    tokens <- character(0)
    lemmas <- character(0)
    pos_tags <- character(0)
    morph_tags <- character(0)

    # First try to split by spaces between span groups (for multiple tokens)
    # Look for spaces that separate token groups
    if (grepl('</span>\\s+<span', section_content)) {
      # Multiple tokens separated by spaces
      token_groups <- unlist(strsplit(section_content, '(?<=</span>)\\s+(?=<span)', perl = TRUE))
    } else {
      # Single token (or no spaces between tokens)
      token_groups <- c(section_content)
    }

    for (group in token_groups) {
      group <- trimws(group)
      if (nchar(group) == 0) next

      # Extract the actual text content (the innermost text)
      text_match <- regexpr('>([^<>]+)</span>', group, perl = TRUE)
      if (text_match > 0) {
        # Find all possible text contents and take the last one (innermost)
        all_texts <- regmatches(group, gregexpr('>([^<>]+)</span>', group, perl = TRUE))[[1]]
        if (length(all_texts) > 0) {
          # Take the last match (innermost text)
          text_content <- sub('.*>([^<>]+)</span>.*', '\\1', all_texts[length(all_texts)], perl = TRUE)
          text_content <- trimws(text_content)

          if (nchar(text_content) > 0 && !grepl('^<', text_content)) {
            tokens <- c(tokens, text_content)

            # Extract all title attributes from this group
            titles <- regmatches(group, gregexpr('title="([^"]*)"', group, perl = TRUE))[[1]]

            morph_features <- character(0)
            lemma <- NA
            pos_tag <- NA

            for (title in titles) {
              content <- sub('title="([^"]*)"', '\\1', title, perl = TRUE)

              if (grepl('^[^/]+/l:', content)) {
                lemma <- sub('^[^/]+/l:(.*)$', '\\1', content)
              } else if (grepl('^[^/]+/p:', content)) {
                pos_tag <- sub('^[^/]+/p:(.*)$', '\\1', content)
              } else if (grepl('^[^/]+/m:', content)) {
                morph_feature <- sub('^[^/]+/m:(.*)$', '\\1', content)
                morph_features <- c(morph_features, morph_feature)
              }
            }

            lemmas <- c(lemmas, lemma)
            pos_tags <- c(pos_tags, pos_tag)
            morph_tag <- if (length(morph_features) > 0) paste(morph_features, collapse = "|") else NA
            morph_tags <- c(morph_tags, morph_tag)
          }
        }
      }
    }

    # Ensure all vectors have the same length
    max_length <- max(length(tokens), length(lemmas), length(pos_tags), length(morph_tags))
    if (max_length > 0) {
      tokens <- c(tokens, rep(NA, max_length - length(tokens)))
      lemmas <- c(lemmas, rep(NA, max_length - length(lemmas)))
      pos_tags <- c(pos_tags, rep(NA, max_length - length(pos_tags)))
      morph_tags <- c(morph_tags, rep(NA, max_length - length(morph_tags)))
    }

    return(list(
      tokens = tokens,
      lemmas = lemmas,
      pos_tags = pos_tags,
      morph_tags = morph_tags
    ))
  }

  # Split the XML into three parts: left context, match content, and right context
  # The structure is: <span class="match">...left...<mark>...match...</mark>...right...</span>

  # First extract the content within the match span using DOTALL modifier
  match_span_pattern <- '(?s)<span class="match">(.*?)</span>\\s*<span class="context-right">'
  match_span_match <- regexpr(match_span_pattern, xml_snippet, perl = TRUE)

  if (match_span_match == -1) {
    # Try alternative pattern if no context-right
    match_span_pattern <- '(?s)<span class="match">(.*?)</span>\\s*$'
    match_span_match <- regexpr(match_span_pattern, xml_snippet, perl = TRUE)
  }

  if (match_span_match > 0) {
    match_span_content <- gsub(match_span_pattern, '\\1', xml_snippet, perl = TRUE)

    # Now find the <mark> and </mark> positions within this content
    mark_start <- regexpr('<mark[^>]*>', match_span_content, perl = TRUE)
    mark_end <- regexpr('</mark>', match_span_content, perl = TRUE)

    if (mark_start > 0 && mark_end > 0) {
      # Left context: everything before <mark>
      left_content <- substr(match_span_content, 1, mark_start - 1)

      # Match content: everything between <mark> and </mark> (including the mark tags for now)
      match_content <- substr(match_span_content, mark_start, mark_end + attr(mark_end, "match.length") - 1)

      # Right context: everything after </mark>
      right_content_start <- mark_end + attr(mark_end, "match.length")
      right_content <- substr(match_span_content, right_content_start, nchar(match_span_content))
    } else {
      # No mark tags found, treat entire match span as match content
      left_content <- ""
      match_content <- match_span_content
      right_content <- ""
    }
  } else {
    # No match span found, treat entire content as match
    left_content <- ""
    match_content <- xml_snippet
    right_content <- ""
  }

  # Process each section
  left_annotations <- extract_annotations_from_section(left_content)
  match_annotations <- extract_annotations_from_section(match_content)
  right_annotations <- extract_annotations_from_section(right_content)

  return(list(
    atokens = list(
      left = left_annotations$tokens,
      match = match_annotations$tokens,
      right = right_annotations$tokens
    ),
    lemma = list(
      left = left_annotations$lemmas,
      match = match_annotations$lemmas,
      right = right_annotations$lemmas
    ),
    pos = list(
      left = left_annotations$pos_tags,
      match = match_annotations$pos_tags,
      right = right_annotations$pos_tags
    ),
    morph = list(
      left = left_annotations$morph_tags,
      match = match_annotations$morph_tags,
      right = right_annotations$morph_tags
    )
  ))
}

#' Fetch annotations for all collected matches
#'
#' `r lifecycle::badge("experimental")`
#'
#' **`fetchAnnotations`** fetches annotations (only token annotations, for now)
#' for all matches in the `@collectedMatches` slot
#' of a KorAPQuery object and adds annotation columns directly to the `@collectedMatches`
#' data frame. The method uses the `matchID` from collected matches.
#'
#' **Important**: For copyright-restricted corpora, users must be authorized via [auth()]
#' and the initial corpus query must have `metadataOnly = FALSE` to ensure snippets are
#' available for annotation parsing.
#'
#' The method parses XML snippet annotations and adds linguistic columns to the data frame:
#' - `pos`: data frame with `left`, `match`, `right` columns, each containing list vectors of part-of-speech tags
#' - `lemma`: data frame with `left`, `match`, `right` columns, each containing list vectors of lemmas
#' - `morph`: data frame with `left`, `match`, `right` columns, each containing list vectors of morphological tags
#' - `atokens`: data frame with `left`, `match`, `right` columns, each containing list vectors of token text (from annotations)
#' - `annotation_snippet`: original XML snippet from the annotation API
#'
#' @family corpus search functions
#' @concept Annotations
#' @aliases fetchAnnotations
#'
#' @param kqo object obtained from [corpusQuery()] with collected matches. Note: the original corpus query should have `metadataOnly = FALSE` for annotation parsing to work.
#' @param foundry string specifying the foundry to use for annotations (default: "tt" for Tree-Tagger)
#' @param verbose print progress information if true
#' @return The updated `kqo` object with annotation columns 
#' like `pos`, `lemma`, `morph` (and `atokens` and `annotation_snippet`)
#' in the `@collectedMatches` slot. Each column is a data frame
#' with `left`, `match`, and `right` columns containing list vectors of annotations
#' for the left context, matched tokens, and right context, respectively.
#' The original XML snippet for each match is also stored in `annotation_snippet`.
#'
#' @examples
#' \dontrun{
#'
#' # Fetch annotations for matches using Tree-Tagger foundry
#' # Note: Authorization required for copyright-restricted corpora
#' q <- KorAPConnection() |>
#'   auth() |>
#'   corpusQuery("Ameisenplage", metadataOnly = FALSE) |>
#'   fetchNext(maxFetch = 10) |>
#'   fetchAnnotations()
#'
#' # Access linguistic annotations for match i:
#' pos_tags <- q@collectedMatches$pos         # Data frame with left/match/right columns for POS tags
#' lemmas <- q@collectedMatches$lemma         # Data frame with left/match/right columns for lemmas
#' morphology <- q@collectedMatches$morph     # Data frame with left/match/right columns for morphological tags
#' atokens <- q@collectedMatches$atokens      # Data frame with left/match/right columns for annotation token text
#' raw_snippet <- q@collectedMatches$annotation_snippet[[i]] # Original XML snippet for match i
#'
#' # Access specific components:
#' match_pos <- q@collectedMatches$pos$match[[i]]     # POS tags for the matched tokens in match i
#' left_lemmas <- q@collectedMatches$lemma$left[[i]]  # Lemmas for the left context in match i
#' right_tokens <- q@collectedMatches$atokens$right[[i]] # Token text for the right context in match i
#'
#' # Use a different foundry (e.g., MarMoT)
#' q <- KorAPConnection() |>
#'   auth() |>
#'   corpusQuery("Ameisenplage", metadataOnly = FALSE) |>
#'   fetchNext(maxFetch = 10) |>
#'   fetchAnnotations(foundry = "marmot")
#' q@collectedMatches$pos$left[1] # POS tags for the left context of the first match
#' }
#' @export
setMethod("fetchAnnotations", "KorAPQuery", function(kqo, foundry = "tt", verbose = kqo@korapConnection@verbose) {
  if (is.null(kqo@collectedMatches) || nrow(kqo@collectedMatches) == 0) {
    warning("No collected matches found. Please run fetchNext() or fetchAll() first.")
    return(kqo)
  }

  df <- kqo@collectedMatches
  kco <- kqo@korapConnection

  # Initialize annotation columns as data frames (like tokens field)
  # Create the structure more explicitly to avoid assignment issues
  nrows <- nrow(df)

  # Pre-compute the empty character vector list to avoid repeated computation
  empty_char_list <- I(replicate(nrows, character(0), simplify = FALSE))
  
  # Helper function to create annotation data frame structure
  create_annotation_df <- function(empty_list) {
    data.frame(
      left = empty_list,
      match = empty_list,
      right = empty_list,
      stringsAsFactors = FALSE
    )
  }

  # Initialize all annotation columns using the helper function
  annotation_types <- c("pos", "lemma", "morph", "atokens")
  for (type in annotation_types) {
    df[[type]] <- create_annotation_df(empty_char_list)
  }

  df$annotation_snippet <- replicate(nrows, NA, simplify = FALSE)

  # Initialize timing for ETA calculation
  start_time <- Sys.time()
  if (verbose) {
    log_info(verbose, paste("Starting to fetch annotations for", nrows, "matches\n"))
  }

  for (i in seq_len(nrow(df))) {
    # ETA logging
    if (verbose && i > 1) {
      eta_info <- calculate_eta(i, nrows, start_time)
      log_info(verbose, paste("Fetching annotations for match", i, "of", nrows, eta_info, "\n"))
    }
    # Use matchID if available, otherwise fall back to constructing from matchStart/matchEnd
    if ("matchID" %in% colnames(df) && !is.na(df$matchID[i])) {
      # matchID format: "match-match-A00/JUN/39609-p202-203" or encrypted format like
      # "match-DNB10/CSL/80400-p2343-2344x_MinDOhu_P6dd2MMZJyyus_7MairdKnr1LxY07Cya-Ow"
      # Extract document path and position, handling both regular and encrypted formats

      # More flexible regex to extract the document path with position and encryption
      # Look for pattern: match-(...)-p(\d+)-(\d+)(.*) where (.*) is the encrypted part
      # We need to capture the entire path including the encrypted suffix
      match_result <- regexpr("match-(.+?-p\\d+-\\d+.*)", df$matchID[i], perl = TRUE)

      if (match_result > 0) {
        # Extract the complete path including encryption (everything after "match-")
        doc_path_with_pos_and_encryption <- gsub("^match-(.+)$", "\\1", df$matchID[i], perl = TRUE)
        # Convert the dash before position to slash, but keep everything after the position
        match_path <- gsub("-p(\\d+-\\d+.*)", "/p\\1", doc_path_with_pos_and_encryption)
        # Use httr2 to construct URL safely
        base_url <- paste0(kco@apiUrl, "corpus/", match_path)
        req <- httr2::url_modify(base_url, query = list(foundry = foundry))
      } else {
        # If regex fails, fall back to the old method with httr2
        # Format numbers to avoid scientific notation
        match_start <- format(df$matchStart[i], scientific = FALSE)
        match_end <- format(df$matchEnd[i], scientific = FALSE)
        base_url <- paste0(kco@apiUrl, "corpus/", df$textSigle[i], "/", "p", match_start, "-", match_end)
        req <- httr2::url_modify(base_url, query = list(foundry = foundry))
      }
    } else {
      # Fallback to the old method with httr2
      # Format numbers to avoid scientific notation
      match_start <- format(df$matchStart[i], scientific = FALSE)
      match_end <- format(df$matchEnd[i], scientific = FALSE)
      base_url <- paste0(kco@apiUrl, "corpus/", df$textSigle[i], "/", "p", match_start, "-", match_end)
      req <- httr2::url_modify(base_url, query = list(foundry = foundry))
    }

    tryCatch({
      res <- apiCall(kco, req)

      if (!is.null(res)) {
        # Store the raw annotation snippet
        df$annotation_snippet[[i]] <- if (is.list(res) && "snippet" %in% names(res)) res$snippet else NA

        # Parse XML annotations if snippet is available
        if (is.list(res) && "snippet" %in% names(res)) {
          parsed_annotations <- parse_xml_annotations_structured(res$snippet)

          # Store the parsed linguistic data in data frame format (like tokens)
          # Use individual assignment to avoid data frame mismatch errors
          tryCatch({
            # Assign POS annotations
            df$pos$left[i] <- list(parsed_annotations$pos$left)
            df$pos$match[i] <- list(parsed_annotations$pos$match)
            df$pos$right[i] <- list(parsed_annotations$pos$right)

            # Assign lemma annotations
            df$lemma$left[i] <- list(parsed_annotations$lemma$left)
            df$lemma$match[i] <- list(parsed_annotations$lemma$match)
            df$lemma$right[i] <- list(parsed_annotations$lemma$right)

            # Assign morphology annotations
            df$morph$left[i] <- list(parsed_annotations$morph$left)
            df$morph$match[i] <- list(parsed_annotations$morph$match)
            df$morph$right[i] <- list(parsed_annotations$morph$right)

            # Assign token annotations
            df$atokens$left[i] <- list(parsed_annotations$atokens$left)
            df$atokens$match[i] <- list(parsed_annotations$atokens$match)
            df$atokens$right[i] <- list(parsed_annotations$atokens$right)
          }, error = function(assign_error) {
            # Set empty character vectors on assignment error using list assignment
            df$pos$left[i] <<- list(character(0))
            df$pos$match[i] <<- list(character(0))
            df$pos$right[i] <<- list(character(0))

            df$lemma$left[i] <<- list(character(0))
            df$lemma$match[i] <<- list(character(0))
            df$lemma$right[i] <<- list(character(0))

            df$morph$left[i] <<- list(character(0))
            df$morph$match[i] <<- list(character(0))
            df$morph$right[i] <<- list(character(0))

            df$atokens$left[i] <<- list(character(0))
            df$atokens$match[i] <<- list(character(0))
            df$atokens$right[i] <<- list(character(0))
          })
        } else {
          # No snippet available, store empty vectors
          df$pos$left[i] <- list(character(0))
          df$pos$match[i] <- list(character(0))
          df$pos$right[i] <- list(character(0))

          df$lemma$left[i] <- list(character(0))
          df$lemma$match[i] <- list(character(0))
          df$lemma$right[i] <- list(character(0))

          df$morph$left[i] <- list(character(0))
          df$morph$match[i] <- list(character(0))
          df$morph$right[i] <- list(character(0))

          df$atokens$left[i] <- list(character(0))
          df$atokens$match[i] <- list(character(0))
          df$atokens$right[i] <- list(character(0))
        }
      } else {
        # Store NAs for failed requests
        df$pos$left[i] <- list(NA)
        df$pos$match[i] <- list(NA)
        df$pos$right[i] <- list(NA)

        df$lemma$left[i] <- list(NA)
        df$lemma$match[i] <- list(NA)
        df$lemma$right[i] <- list(NA)

        df$morph$left[i] <- list(NA)
        df$morph$match[i] <- list(NA)
        df$morph$right[i] <- list(NA)

        df$atokens$left[i] <- list(NA)
        df$atokens$match[i] <- list(NA)
        df$atokens$right[i] <- list(NA)
        df$annotation_snippet[[i]] <- NA
      }
    }, error = function(e) {
      # Store NAs for failed requests
      df$pos$left[i] <- list(NA)
      df$pos$match[i] <- list(NA)
      df$pos$right[i] <- list(NA)

      df$lemma$left[i] <- list(NA)
      df$lemma$match[i] <- list(NA)
      df$lemma$right[i] <- list(NA)

      df$morph$left[i] <- list(NA)
      df$morph$match[i] <- list(NA)
      df$morph$right[i] <- list(NA)

      df$atokens$left[i] <- list(NA)
      df$atokens$match[i] <- list(NA)
      df$atokens$right[i] <- list(NA)
      df$annotation_snippet[[i]] <- NA
    })
  }

  # Validate data frame structure before assignment
  if (nrow(df) != nrow(kqo@collectedMatches)) {
  }

  # Update the collectedMatches with annotation data
  tryCatch({
    kqo@collectedMatches <- df
  }, error = function(assign_error) {
    # Try a safer approach: add columns individually
    tryCatch({
      kqo@collectedMatches$pos <- df$pos
      kqo@collectedMatches$lemma <- df$lemma
      kqo@collectedMatches$morph <- df$morph
      kqo@collectedMatches$atokens <- df$atokens
      kqo@collectedMatches$annotation_snippet <- df$annotation_snippet
    }, error = function(col_error) {
      warning("Failed to add annotation data to collectedMatches")
    })
  })

  if (verbose) {
    elapsed_time <- Sys.time() - start_time
    log_info(verbose, paste("Finished fetching annotations for", nrows, "matches in", format_duration(as.numeric(elapsed_time, units = "secs")), "\n"))
  }

  return(kqo)
})

#' Query frequencies of search expressions in virtual corpora
#'
#' **`frequencyQuery`** combines [corpusQuery()], [corpusStats()] and
#' [ci()] to compute a tibble with the absolute and relative frequencies and
#' confidence intervals of one ore multiple search terms across one or multiple
#' virtual corpora.
#'
#' @family frequency analysis
#' @aliases frequencyQuery
#' @examples
#' \dontrun{
#'
#' KorAPConnection(verbose = TRUE) |>
#'   frequencyQuery(c("Mücke", "Schnake"), paste0("pubDate in ", 2000:2003))
#' }
#'
# @inheritParams corpusQuery
#' @param kco [KorAPConnection()] object (obtained e.g. from `KorAPConnection()`
#' @param query corpus query string(s.) (can be a vector). The query language depends on the `ql` parameter. Either `query` must be provided or `KorAPUrl`.
#' @param vc virtual corpus definition(s) (can be a vector)
#' @param conf.level confidence level of the returned confidence interval (passed through [ci()]  to [prop.test()]).
#' @param as.alternatives LOGICAL that specifies if the query terms should be treated as alternatives. If `as.alternatives` is TRUE, the sum over all query hits, instead of the respective vc token sizes is used as total for the calculation of relative frequencies.
#' @param ... further arguments passed to or from other methods (see [corpusQuery()]), most notably `expand`, a logical that decides if `query` and `vc` parameters are expanded to all of their combinations. It defaults to `TRUE`, if `query` and `vc` have different lengths, and to `FALSE` otherwise.
#' @export
#'
#' @return A tibble, with each row containing the following result columns for query and vc combinations:
#'   - **query**: the query string used for the frequency analysis.
#'   - **totalResults**: absolute frequency of query matches in the vc.
#'   - **vc**:  virtual corpus used for the query.
#'   - **webUIRequestUrl**: URL of the corresponding web UI request with respect to query and vc.
#'   - **total**: total number of words in vc.
#'   - **f**:  relative frequency of query matches in the vc.
#'   - **conf.low**:  lower bound of the confidence interval for the relative frequency, given `conf.level`.
#'   - **conf.high**:  upper bound of the confidence interval for the relative frequency, given `conf.level`.

setMethod(
  "frequencyQuery", "KorAPConnection",
  function(kco, query, vc = "", conf.level = 0.95, as.alternatives = FALSE, ...) {
    (if (as.alternatives) {
      corpusQuery(kco, query, vc, metadataOnly = TRUE, as.df = TRUE, ...) |>
        group_by(vc) |>
        mutate(total = sum(totalResults))
    } else {
      corpusQuery(kco, query, vc, metadataOnly = TRUE, as.df = TRUE, ...) |>
        mutate(total = corpusStats(kco, vc = vc, as.df = TRUE)$tokens)
    }) |>
      ci(conf.level = conf.level)
  }
)

#' buildWebUIRequestUrlFromString
#'
#' @rdname KorAPQuery-class
#' @importFrom urltools url_encode
#' @export
buildWebUIRequestUrlFromString <- function(KorAPUrl,
                                           query,
                                           vc = "",
                                           ql = "poliqarp") {
  if ("KorAPConnection" %in% class(KorAPUrl)) {
    KorAPUrl <- KorAPUrl@KorAPUrl
  }

  request <-
    paste0(
      "?q=",
      urltools::url_encode(enc2utf8(as.character(query))),
      ifelse(vc != "",
        paste0("&cq=", urltools::url_encode(enc2utf8(vc))),
        ""
      ),
      "&ql=",
      ql
    )
  paste0(KorAPUrl, request)
}

#' buildWebUIRequestUrl
#'
#' @rdname KorAPQuery-class
#' @importFrom httr2 url_parse
#' @export
buildWebUIRequestUrl <- function(kco,
                                 query = if (missing(KorAPUrl)) {
                                   stop("At least one of the parameters query and KorAPUrl must be specified.", call. = FALSE)
                                 } else {
                                   httr2::url_parse(KorAPUrl)$query$q
                                 },
                                 vc = if (missing(KorAPUrl)) "" else httr2::url_parse(KorAPUrl)$query$cq,
                                 KorAPUrl,
                                 ql = if (missing(KorAPUrl)) "poliqarp" else httr2::url_parse(KorAPUrl)$query$ql) {
  buildWebUIRequestUrlFromString(kco@KorAPUrl, query, vc, ql)
}

#' format()
#' @rdname KorAPQuery-class
#' @param x KorAPQuery object
#' @param ... further arguments passed to or from other methods
#' @importFrom urltools param_get url_decode
#' @export
format.KorAPQuery <- function(x, ...) {
  cat("<KorAPQuery>\n")
  q <- x
  param <- urltools::param_get(q@request) |> lapply(urltools::url_decode)
  cat("           Query: ", param$q, "\n")
  if (!is.null(param$cq) && param$cq != "") {
    cat("  Virtual corpus: ", param$cq, "\n")
  }
  if (!is.null(q@collectedMatches)) {
    cat("==============================================================================================================", "\n")
    print(summary(q@collectedMatches))
    cat("==============================================================================================================", "\n")
  }
  cat("   Total results: ", q@totalResults, "\n")
  cat(" Fetched results: ", q@nextStartIndex, "\n")
  if (!is.null(q@collectedMatches) && "pos" %in% colnames(q@collectedMatches)) {
    successful_annotations <- sum(!is.na(q@collectedMatches$annotation_snippet))
    parsed_annotations <- sum(!is.na(q@collectedMatches$pos))
    cat("     Annotations: ", successful_annotations, " of ", nrow(q@collectedMatches), " matches")
    if (parsed_annotations > 0) {
      cat(" (", parsed_annotations, " with parsed linguistic data)")
    }
    cat("\n")
  }
}

#' show()
#'
#' @rdname KorAPQuery-class
#' @param object KorAPQuery object
#' @export
setMethod("show", "KorAPQuery", function(object) {
  format(object)
  invisible(object)
})
