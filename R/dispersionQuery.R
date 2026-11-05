#' Define corpus parts for a dispersion analysis
#'
#' Discover metadata groups and their token counts, or define a custom
#' partition. Discovery checks the base corpus independently of query hits,
#' so that parts without hits are retained without downloading the corpus.
#'
#' @param kco A [KorAPConnection()] object.
#' @param by Metadata field. The aliases `source`, `year` and `region` stand for
#'   `corpusTitle`, `pubDate` (publication year) and `pubPlaceKey` respectively.
#' @param values Vector of metadata values; optional names label the parts.
#'   If neither `values` nor `parts` is supplied, discover the groups automatically.
#'   Four-digit values for `pubDate` or `creationDate` define years. Other
#'   fields use literal equality. Use the stored region keys, not place names.
#'   Corpus titles may include publication years. To group annual corpora by
#'   source, use `by = "source"`, as in `demo("dispersion")`.
#'   Custom `parts` and a matching `classify` function support other groupings.
#' @param vc Base virtual corpus. All parts are intersected with this corpus.
#' @param parts Alternatively, a uniquely named vector of virtual-corpus
#'   definitions. For sampling arbitrary definitions, also provide `classify`.
#' @param classify Optional function of a matches data frame returning one part
#'   label per row, or `NA` for excluded/missing metadata. Its membership rules
#'   must agree with `parts`. Multivalued fields require an explicit classifier.
#' @param fields Metadata fields needed by `classify`; defaults to `by`.
#' @param verbose Whether to print progress.
#' @param cacheAs Optional result file; see [frequencyQuery()].
#' @param regex Optional trailing-suffix grouping rule for automatic discovery;
#'   see [dispersionAnalysis,KorAPQuery-method]. Defaults to removing a year suffix
#'   for `by = "source"`.
#' @return A tibble of class `KorAPCorpusPartition` with `part`, `value`, `vc`,
#'   and `tokens` columns. Attributes record the metadata mapping, base corpus,
#'   server, index revision, covered tokens and excluded tokens. Overlapping
#'   parts are rejected by comparing their summed sizes with their union.
#'   An incomplete inventory explicitly restricts the population to its union,
#'   with a warning; it does not silently stand for the whole base corpus.
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' years <- corpusPartition(kco, "year", 2020:2022, vc = "referTo ratskorpus")
#' countries <- corpusPartition(kco, "region", c("DE", "AT", "CH"))
#' dispersionQuery(kco, "NutzerInnen", partitions = list(year = years))
#' }
#' @export
corpusPartition <- function(kco, by, values = NULL, vc = "", parts = NULL,
                            classify = NULL, fields = NULL,
                            verbose = kco@verbose, cacheAs = NULL, regex = NULL) {
  dispersionScalarString(by, "by")
  dispersionScalarString(vc, "vc", empty = TRUE)
  if (is.null(values) && is.null(parts)) {
    record <- NULL
    if (!is.null(cacheAs)) {
      cacheAs <- cacheAsFileName(cacheAs)
      record <- cacheAsRecord(environment(), NULL, kco)
      cached <- readCacheAs(cacheAs, kco, record, "corpus partition")
      if (!is.null(cached)) return(cached)
    }
    result <- discoverDispersionPartition(kco, by, vc, regex, verbose)
    if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "corpus partition", result)
    return(result)
  }
  if (!is.null(regex)) stop("Use regex with automatically discovered groups, without values or parts.", call. = FALSE)
  aliases <- c(source = "corpusTitle", year = "pubDate", region = "pubPlaceKey")
  if (by %in% names(aliases)) by <- unname(aliases[by])
  if (!grepl("^[[:alpha:]][[:alnum:]_.:-]*$", by)) {
    stop("by must be a metadata field name.", call. = FALSE)
  }
  if (is.null(fields)) fields <- by
  if (!is.character(fields) || !length(fields) || anyNA(fields) || any(!nzchar(fields))) {
    stop("fields must contain metadata field names.", call. = FALSE)
  }
  if (!is.null(classify) && !is.function(classify)) stop("classify must be a function.", call. = FALSE)
  year <- by %in% c("pubDate", "creationDate") && !is.null(values) &&
    all(grepl("^[0-9]{4}$", as.character(values)))

  if (is.null(parts)) {
    if (is.null(values) || !is.atomic(values) || !length(values) ||
        anyNA(values) || any(!nzchar(as.character(values))) || anyDuplicated(values)) {
      stop("Supply distinct values from an independent metadata inventory, or named parts.", call. = FALSE)
    }
    labels <- names(values)
    if (is.null(labels)) labels <- as.character(values)
    parts <- if (year) paste(by, "in", values) else
      paste(by, "=", vapply(as.character(values), quoteDispersionValue, character(1)))
    names(parts) <- labels
  }
  if (!is.character(parts) || !length(parts) || anyNA(parts) || any(!nzchar(parts)) ||
      is.null(names(parts)) || anyNA(names(parts)) || any(!nzchar(names(parts))) ||
      anyDuplicated(names(parts)) || anyDuplicated(unname(parts))) {
    stop("parts must be a uniquely named vector of distinct virtual corpora.", call. = FALSE)
  }
  if (!is.null(values) && length(values) != length(parts)) {
    stop("values and parts must have the same length.", call. = FALSE)
  }
  if (!is.null(values) && (!is.atomic(values) || anyNA(values) ||
      any(!nzchar(as.character(values))) || anyDuplicated(values))) {
    stop("values must contain distinct, non-missing metadata values.", call. = FALSE)
  }

  record <- NULL
  if (!is.null(cacheAs)) {
    cacheAs <- cacheAsFileName(cacheAs)
    record <- cacheAsRecord(environment(), NULL, kco)
    cached <- readCacheAs(cacheAs, kco, record, "corpus partition")
    if (!is.null(cached)) return(cached)
  }
  result <- assembleDispersionPartition(kco, by, vc, parts, values, fields,
    classify, year, verbose)
  if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "corpus partition", result)
  result
}

assembleDispersionPartition <- function(kco, by, vc, parts, values = NULL,
                                        fields = by, classify = NULL, year = FALSE,
                                        verbose = kco@verbose, unionCondition = NULL) {
  partVcs <- vapply(parts, function(part) intersectDispersionVc(vc, part), character(1))
  if (is.null(unionCondition)) unionCondition <-
    paste0("(", paste(paste0("(", unname(parts), ")"), collapse = " | "), ")")
  unionVc <- intersectDispersionVc(vc, unionCondition)
  base <- dispersionCorpusSizes(kco, vc, "base virtual corpus", verbose)
  if (base == 0) {
    stop("The base virtual corpus is empty (0 tokens). Check vc and its metadata filters; corpusTitle values may include publication years.", call. = FALSE)
  }
  sizes <- dispersionCorpusSizes(kco, partVcs, "corpus parts", verbose)
  covered <- dispersionCorpusSizes(kco, unionVc, "corpus-part union", verbose)
  if (covered == 0) {
    stop("None of the listed corpus parts contain tokens in the base corpus. Check the metadata values; corpusTitle values may include publication years.", call. = FALSE)
  }
  if (base < covered || sum(sizes) < covered) {
    stop("Inconsistent corpus-size responses: the union is larger than the base corpus or the sum of its parts. Recheck the virtual-corpus filters and server responses.", call. = FALSE)
  }
  if (sum(sizes) != covered) {
    stop("Corpus parts overlap: their summed token counts differ from their union.", call. = FALSE)
  }
  result <- tibble::tibble(part = names(parts),
                           value = if (is.null(values)) rep(NA_character_, length(parts)) else as.character(values),
                           vc = unname(partVcs), tokens = as.numeric(sizes))
  class(result) <- c("KorAPCorpusPartition", class(result))
  attr(result, "partition") <- list(by = by, fields = unique(fields), year = year,
    classify = classify, canClassify = !is.null(classify) || !is.null(values),
    baseVc = vc, unionVc = unionVc, baseTokens = as.numeric(base),
    coveredTokens = as.numeric(covered), excludedTokens = as.numeric(base - covered),
    apiUrl = kco@apiUrl, indexRevision = kco@indexRevision)
  warnDispersionCoverage(base - covered)
  result
}

warnDispersionCoverage <- function(excludedTokens) {
  if (excludedTokens > 0) warning("The partition excludes ",
    format(excludedTokens, scientific = FALSE),
    " base-corpus tokens; dispersion will describe only the listed parts.", call. = FALSE)
}

setGeneric("dispersionQuery", function(kco, ...) standardGeneric("dispersionQuery"))

#' Query dispersion across corpus metadata
#'
#' Compute selectable dispersion measures for one or more queries, using exact
#' counts per part or a reproducible metadata sample shared by all partitions.
#'
#' @aliases dispersionQuery
#' @param kco A [KorAPConnection()] object.
#' @param query Query string or vector of queries.
#' @param partitions Optional [corpusPartition()] table, or named list of them.
#'   If `NULL`, discover groups using `by`, as in [dispersionAnalysis,KorAPQuery-method].
#'   Explicit partitions must have the same base virtual corpus and KorAP instance.
#' @param by Metadata field(s) for automatic grouping, default `"source"`.
#' @param regex Optional trailing-suffix grouping rule for automatic grouping.
#' @param vc Base virtual corpus for automatic grouping. With explicit
#'   partitions, their base virtual corpus is used instead.
#' @param scoreFunctions Named list of replaceable measure functions; see
#'   [defaultDispersionFunctions()].
#' @param exactFrequencies If `TRUE` (default), request exact counts for each
#'   part, without downloading matches. If `FALSE`, collect match metadata,
#'   using at most `searchHitsSampleLimit` hits. No snippets are requested.
#' @param searchHitsSampleLimit Maximum metadata sample size, default 20000.
#'   `Inf` requests all match metadata. Samples draw complete pages without
#'   replacement, so finite limits are rounded down to multiples of 50 and
#'   must be at least 50 unless all hits fit below the limit.
#' @param seed Seed for page selection. The caller's random state is restored.
#' @param ql Query language, passed to [corpusQuery()].
#' @param verbose Whether to print progress.
#' @param cacheAs Optional result file; see [frequencyQuery()].
#'
#' @details
#' A sample is a simple random sample of result pages, not independent hits.
#' Every hit has the same page-inclusion probability. The returned distribution
#' retains unobserved parts and reports observed and extrapolated hit counts.
#' Ratios of these estimates provide plug-in estimates of dispersion; nonlinear
#' measures can be strongly biased by within-page clustering, even with many
#' sampled hits. Range from a sample is only a lower
#' bound, not an estimate of the number of occupied parts. No confidence bounds
#' are claimed. Missing or unlisted metadata are counted as excluded hits.
#'
#' Partition sizes come from independent corpus statistics, never from the
#' sample. Failed, incomplete or timed-out requests abort the analysis rather
#' than becoming zeros. `exactFrequencies = TRUE` never falls back to sampling.
#' Arbitrary `parts` require a membership classifier for metadata sampling.
#' Reuse a partition only with its original base corpus, server and index revision.
#'
#' @return A tibble with one row per query and partition, the requested scores,
#'   `totalResults` (base-corpus hits), `sampledHits` (metadata rows analyzed),
#'   `usedHits`, `excludedHits`, `nParts`, `estimated`, `rangeIsLowerBound`,
#'   and list columns `distribution` and `sampling`. Distributions include
#'   corpus-part sizes, observed counts, estimated frequencies, relative
#'   frequencies and concordance URLs. `sampling` records the method, seed,
#'   selected pages, inclusion probability, server and index revision.
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' years <- corpusPartition(kco, "year", 2020:2022)
#' dispersionQuery(kco, "NutzerInnen", years, scoreFunctions = list(
#'   evenness = function(frequencies, sizes) 1 - dispersionDP(frequencies, sizes)
#' ))
#' dispersionQuery(kco, "NutzerInnen", years, exactFrequencies = FALSE,
#'                 searchHitsSampleLimit = 10000, seed = 7)
#' }
#' @export
setMethod("dispersionQuery", "KorAPConnection",
  function(kco, query, partitions = NULL, scoreFunctions = defaultDispersionFunctions(),
           exactFrequencies = TRUE, searchHitsSampleLimit = 20000, seed = 7,
           ql = "poliqarp", verbose = kco@verbose, cacheAs = NULL,
           by = "source", regex = NULL, vc = "") {
    validateDispersionControls(scoreFunctions, exactFrequencies, searchHitsSampleLimit, seed)
    if (!is.character(query) || !length(query) || anyNA(query) || any(!nzchar(query))) {
      stop("query must contain non-empty query strings.", call. = FALSE)
    }
    if (is.null(partitions)) {
      dispersionScalarString(vc, "vc", empty = TRUE)
      record <- NULL
      if (!is.null(cacheAs)) {
        cacheAs <- cacheAsFileName(cacheAs)
        record <- cacheAsRecord(environment(), NULL, kco)
        cached <- readCacheAs(cacheAs, kco, record, "dispersion query")
        if (!is.null(cached)) return(cached)
      }
      shared <- NULL
      results <- lapply(query, function(expression) {
        q <- strictDispersionRequest(corpusQuery(kco, expression, vc = vc,
          ql = ql, metadataOnly = TRUE, verbose = verbose))
        result <- analyseDispersionQuery(q, by, regex, scoreFunctions,
          exactFrequencies, searchHitsSampleLimit, seed, verbose, partitions = shared)
        if (is.null(shared)) shared <<- attr(result, "partitions")
        result
      })
      result <- asDispersionResult(dplyr::bind_rows(results), names(scoreFunctions))
      if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "dispersion query", result)
      return(result)
    }
    if (inherits(partitions, "KorAPCorpusPartition")) partitions <- list(partition = partitions)
    if (!is.list(partitions) || !length(partitions) || is.null(names(partitions)) ||
        anyNA(names(partitions)) || any(!nzchar(names(partitions))) || anyDuplicated(names(partitions))) {
      stop("partitions must be a partition table or a uniquely named list of tables.", call. = FALSE)
    }
    specs <- lapply(partitions, function(part) {
      spec <- attr(part, "partition")
      if (!inherits(part, "KorAPCorpusPartition") || is.null(spec) ||
          !identical(spec$apiUrl, kco@apiUrl) || !identical(spec$indexRevision, kco@indexRevision)) {
        stop("Use corpusPartition() tables from this KorAP instance and index revision.", call. = FALSE)
      }
      if (!exactFrequencies && !spec$canClassify) {
        stop("Sampling custom corpus parts requires a classify function.", call. = FALSE)
      }
      spec
    })
    baseVc <- specs[[1]]$baseVc
    if (!all(vapply(specs, function(s) identical(s$baseVc, baseVc), logical(1)))) {
      stop("All partitions must use the same base virtual corpus.", call. = FALSE)
    }
    record <- NULL
    if (!is.null(cacheAs)) {
      cacheAs <- cacheAsFileName(cacheAs)
      record <- cacheAsRecord(environment(), NULL, kco)
      cached <- readCacheAs(cacheAs, kco, record, "dispersion query")
      if (!is.null(cached)) return(cached)
    }
    results <- list()
    fields <- unique(unlist(lapply(specs, `[[`, "fields")))
    for (expression in query) {
      population <- strictDispersionRequest(corpusQuery(kco, query = expression,
        vc = baseVc, ql = ql, metadataOnly = TRUE, fields = fields, verbose = verbose))
      checkDispersionQuery(population)
      results[[length(results) + 1L]] <- calculateQueryDispersion(
        population, expression, partitions, specs, scoreFunctions,
        exactFrequencies, searchHitsSampleLimit, seed, ql, verbose)
    }
    result <- asDispersionResult(dplyr::bind_rows(results), names(scoreFunctions))
    if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "dispersion query", result)
    result
  })

dispersionScalarString <- function(x, name, empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || (!empty && !nzchar(x))) {
    stop(name, " must be a single ", if (!empty) "non-empty ", "string.", call. = FALSE)
  }
}

quoteDispersionValue <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
}

intersectDispersionVc <- function(base, part) {
  if (!nzchar(base)) part else paste0("(", base, ") & (", part, ")")
}

strictDispersionRequest <- function(expr) {
  withCallingHandlers(expr, korapIncompleteResults = function(w) {
    stop("Dispersion requires complete requests: ", conditionMessage(w), call. = FALSE)
  })
}

dispersionCorpusSizes <- function(kco, vcs, context, verbose) {
  sizes <- strictDispersionRequest(corpusStats(kco, vc = vcs,
    as.df = TRUE, verbose = verbose))$tokens
  if (!is.numeric(sizes) || length(sizes) != length(vcs) ||
      any(!is.finite(sizes)) || any(sizes < 0)) {
    labels <- names(vcs)
    detail <- if (!is.null(labels) && length(labels) == length(sizes) && is.numeric(sizes)) {
      bad <- !is.finite(sizes) | sizes < 0
      if (any(bad)) paste0(" (", paste(labels[bad], collapse = ", "), ")") else ""
    } else ""
    stop("Could not retrieve token counts for the ", context, detail,
      ". Check the KorAP connection and virtual-corpus filters.", call. = FALSE)
  }
  sizes
}

checkDispersionQuery <- function(query) {
  response <- query@apiResponse
  if (is.null(response) || !is.null(response$errors) ||
      !is.null(response$meta$timeExceeded) || length(query@totalResults) != 1 ||
      !is.finite(query@totalResults) || query@totalResults < 0) {
    stop("The query did not return a complete hit count.", call. = FALSE)
  }
}

sampleDispersionMetadata <- function(query, limit, seed, verbose) {
  provenance <- attr(query, "korapSampling")
  if (!is.null(provenance)) {
    if (provenance$populationSize != query@totalResults ||
        is.null(query@collectedMatches) || nrow(query@collectedMatches) != provenance$sampleSize) {
      stop("The stored page-sample provenance no longer matches the query results.", call. = FALSE)
    }
    if (query@totalResults > 0 && !all(query@fields %in% names(query@collectedMatches))) {
      # Enrich exactly the recorded pages; never draw another sample silently.
      refreshed <- sampleQueryPages(query, limit, seed, verbose,
        selectedPages = provenance$pages, useExisting = FALSE)
      query <- refreshed$query
    }
    return(c(list(matches = query@collectedMatches), provenance))
  }
  sampled <- sampleQueryPages(query, limit, seed, verbose)
  if (sampled$inclusionProbability < 1) pageSampleWarning(query@totalResults, limit, TRUE)
  sampled
}

classifyDispersionMatches <- function(matches, part, spec) {
  if (!nrow(matches)) return(character())
  if (!all(spec$fields %in% names(matches))) stop("Required metadata fields are absent.", call. = FALSE)
  if (!is.null(spec$classify)) {
    labels <- spec$classify(matches)
  } else {
    values <- matches[[spec$by]]
    if (is.list(values)) {
      if (any(lengths(values) > 1)) stop("Multivalued metadata require an explicit classify function.", call. = FALSE)
      values <- vapply(values, function(x) if (!length(x)) NA_character_ else as.character(x), character(1))
    }
    values <- as.character(values)
    if (any(grepl("\t", values, fixed = TRUE), na.rm = TRUE)) {
      stop("Multivalued metadata require an explicit classify function.", call. = FALSE)
    }
    if (spec$year) values <- substr(values, 1, 4)
    labels <- part$part[match(values, part$value)]
  }
  if (!is.character(labels) || length(labels) != nrow(matches) ||
      any(!is.na(labels) & !labels %in% part$part)) {
    stop("classify must return one known part label (or NA) per match.", call. = FALSE)
  }
  labels
}


# Shared calculation for a query object and the convenience query wrapper.
calculateQueryDispersion <- function(population, expression, partitions, specs,
                                    scoreFunctions, exactFrequencies,
                                    searchHitsSampleLimit, seed, ql, verbose,
                                    sampled = NULL) {
  kco <- population@korapConnection
  results <- list()
  n <- population@totalResults
  sampled <- if (!is.null(sampled)) sampled else if (!exactFrequencies) sampleDispersionMetadata(population, searchHitsSampleLimit, seed, verbose) else NULL
  for (name in names(partitions)) {
    part <- partitions[[name]]
    spec <- specs[[name]]
    if (exactFrequencies) {
      counts <- strictDispersionRequest(corpusQuery(kco, query = expression,
        vc = part$vc, ql = ql, metadataOnly = TRUE, as.df = TRUE,
        expand = TRUE, verbose = verbose))$totalResults
      if (length(counts) != nrow(part) || any(!is.finite(counts)) || any(counts < 0) || sum(counts) > n) {
        stop("Exact part counts are unavailable or inconsistent with the base corpus.", call. = FALSE)
      }
      observed <- counts
      used <- sum(counts)
      excluded <- n - used
      sampling <- list(method = "exact_counts", seed = NULL, pages = integer(), inclusionProbability = 1)
      downloaded <- 0
      estimated <- FALSE
    } else {
      labels <- classifyDispersionMatches(sampled$matches, part, spec)
      observed <- tabulate(match(labels, part$part), nbins = nrow(part))
      counts <- observed / sampled$inclusionProbability
      used <- sum(observed)
      excluded <- nrow(sampled$matches) - used
      downloaded <- nrow(sampled$matches)
      estimated <- sampled$inclusionProbability < 1
      keys <- c("method", "seed", "pages", "inclusionProbability", "populationSize",
        "sampleSize", "requestedLimit", "pageSize", "hitOrder")
      sampling <- sampled[intersect(keys, names(sampled))]
    }
    if (any(counts[part$tokens == 0] > 0)) stop("Hits occur in a zero-sized corpus part.", call. = FALSE)
    scores <- dispersionAnalysis(counts, part$tokens, scoreFunctions)
    distribution <- tibble::as_tibble(part)
    distribution$observed <- as.numeric(observed)
    distribution$frequency <- as.numeric(counts)
    distribution$f <- ifelse(part$tokens > 0, counts / part$tokens, NA_real_)
    distribution$webUIRequestUrl <- vapply(part$vc, function(vc)
      buildWebUIRequestUrlFromString(kco@KorAPUrl, expression, vc, ql), character(1))
    sampling$apiUrl <- kco@apiUrl
    sampling$indexRevision <- kco@indexRevision
    sampling$baseTokens <- spec$baseTokens
    sampling$coveredTokens <- spec$coveredTokens
    sampling$excludedTokens <- spec$excludedTokens
    results[[length(results) + 1L]] <- dplyr::bind_cols(tibble::tibble(
      query = expression, partition = name, totalResults = as.numeric(n),
      sampledHits = downloaded, usedHits = used, excludedHits = excluded,
      nParts = sum(part$tokens > 0), estimated = estimated,
      rangeIsLowerBound = estimated, distribution = list(distribution),
      sampling = list(sampling)), scores)
  }
  asDispersionResult(dplyr::bind_rows(results), names(scoreFunctions))
}

asDispersionResult <- function(result, scores) {
  class(result) <- unique(c("KorAPDispersion", class(result)))
  attr(result, "dispersionScores") <- scores
  result
}

#' Print dispersion measures without hiding them behind metadata columns
#' @param x A dispersion result.
#' @param ... Passed to the tibble print method.
#' @param width Output width. Defaults to `Inf` so score columns stay visible.
#' @param n Number of rows to print. Defaults to `Inf` to show all measures.
#' @return The input, invisibly.
#' @export
print.KorAPDispersion <- function(x, ..., width = Inf, n = Inf) {
  scores <- intersect(attr(x, "dispersionScores"), names(x))
  required <- c("partition", "totalResults", "sampledHits", "nParts", "estimated", "sampling")
  if (isTRUE(attr(x, "comparison"))) required <- c(required, "vc", "variant")
  if (!all(required %in% names(x))) return(NextMethod("print", width = width, n = n, ...))
  if (isTRUE(attr(x, "comparison")) && nrow(x) && length(scores)) {
    groups <- unique(tibble::as_tibble(x)[, c("vc", "partition"), drop = FALSE])
    for (i in seq_len(nrow(groups))) {
      rows <- which(x$vc == groups$vc[i] & x$partition == groups$partition[i])
      labels <- make.unique(c("measure", x$variant[rows]))[-1]
      cat("Dispersion by ", groups$partition[i], ": ", x$nParts[rows[1]], " corpus parts",
          if (any(x$estimated[rows])) " (page-sample estimates)" else "", "\n", sep = "")
      if (nrow(groups) > 1) cat("Virtual corpus: ", groups$vc[i], "\n", sep = "")
      cat("Hits: ", paste(paste0(labels, " = ", format_count(x$totalResults[rows])), collapse = "; "), "\n", sep = "")
      if (any(x$estimated[rows])) printDispersionSampling(x[rows, ], labels)
      table <- tibble::tibble(measure = scores)
      for (j in seq_along(rows)) {
        table[[labels[j]]] <- vapply(scores, function(name) as.numeric(x[[name]][rows[j]]), numeric(1))
      }
      print(table, width = width, n = n, ...)
    }
    return(invisible(x))
  }
  if (nrow(x) == 1L && length(scores)) {
    cat("Dispersion by ", x$partition[1], ": ", format_count(x$totalResults[1]),
        " hits, ", x$nParts[1], " corpus parts",
        if (x$estimated[1]) " (page-sample estimates)" else "", "\n", sep = "")
    if (x$estimated[1]) printDispersionSampling(x, "query")
    table <- tibble::tibble(measure = scores,
      value = vapply(scores, function(name) as.numeric(x[[name]][1]), numeric(1)))
    print(table, width = width, n = n, ...)
    return(invisible(x))
  }
  columns <- c("query", "partition", "totalResults", "nParts", "estimated",
               scores)
  columns <- intersect(columns, names(x))
  print(tibble::as_tibble(x)[, columns, drop = FALSE], width = width, n = n, ...)
  invisible(x)
}

printDispersionSampling <- function(x, labels) {
  for (i in seq_len(nrow(x))) {
    s <- x$sampling[[i]]
    totalPages <- ceiling(x$totalResults[i] / maxResultsPerPage)
    usedPages <- if (identical(s$method, "existing_metadata")) totalPages else length(s$pages)
    cat("  ", labels[i], ": ", if (identical(s$method, "exact_counts")) "exact part counts" else
      paste0(format_count(x$sampledHits[i]), "/", format_count(x$totalResults[i]),
        " hits; ", usedPages, "/", totalPages,
        " pages; inclusion probability ", format(s$inclusionProbability, digits = 4)), "\n", sep = "")
  }
}

warnDispersionComparison <- function(x) {
  groups <- split(seq_len(nrow(x)), interaction(x$vc, x$partition, drop = TRUE))
  unequal <- any(vapply(groups, function(rows) {
    probabilities <- vapply(x$sampling[rows], `[[`, numeric(1), "inclusionProbability")
    length(unique(probabilities)) > 1L
  }, logical(1)))
  if (unequal) warning("Compared variants have different page-inclusion probabilities. Clustering can bias sampled dispersion scores; use exactFrequencies = TRUE for an exact comparison.", call. = FALSE)
  invisible(x)
}

#' Calculate dispersion directly from a corpus query
#'
#' `r lifecycle::badge("experimental")`
#'
#' Ask a corpus question first, then choose the metadata dimension of its
#' dispersion. The library discovers the groups and their sizes, including
#' groups without query hits. No source lists or classification code are needed.
#' The analysis interface and result structure may change in future releases.
#'
#' @param frequencies A [corpusQuery()] result for one or several queries.
#' @param by Metadata field or vector of fields. `"source"` groups DeReKo's
#'   annual `corpusTitle` values into sources automatically; `"year"` groups
#'   `pubDate` by publication year; `"region"` uses `pubPlaceKey`. Automatic
#'   grouping of `pubDate` and `creationDate` uses calendar-year groups.
#' @param regex Optional regular expression removing a trailing suffix from
#'   metadata values, ending in `$`, e.g. `" [0-9]{4}$"`. For `by = "source"`
#'   this is the default. For custom grouping, use a single field such as
#'   `by = "corpusTitle"`. With multiple dimensions, use their default grouping.
#' @param scoreFunctions Named list of replaceable measure functions; see
#'   [defaultDispersionFunctions()].
#' @param exactFrequencies By default (`NULL`), reuse an explicit page sample
#'   or complete, unjoined metadata containing the requested fields; otherwise
#'   request exact counts per group.
#'   `TRUE` requests exact counts even after a fetch. `FALSE` permits metadata
#'   sampling, controlled by `searchHitsSampleLimit`.
#' @param searchHitsSampleLimit Maximum metadata sample size; see [dispersionQuery()].
#' @param seed Seed for random page selection.
#' @param verbose Whether to print progress.
#' @param cacheAs Optional result file; see [frequencyQuery()].
#' @details
#' An explicit [fetchSamplePages()] step makes the retrieval scope visible.
#' Recorded pages are reused without drawing again; any missing metadata are
#' retrieved from those same pages. Multiple queries share independently
#' discovered groups and token sizes, seeded by the groups already observed
#' across their fetched metadata. Printing their result shows one column
#' per query variant, labelled by the names of the original query vector.
#'
#' See the Interpretation section in [dispersion-score-functions] for what the
#' measures describe, their directions and why they can disagree. Use
#' `distribution` to examine source counts and token sizes alongside the scores.
#'
#' `by = "year"` treats publication years as discrete corpus parts, using each
#' year's token count as its size and retaining positive-sized years without
#' query hits. The built-in measures ignore temporal order and distances;
#' they describe dispersion across year groups, not a trend over time.
#'
#' Use a metadata-only query (the default of [corpusQuery()]). A full-text
#' query rewritten to a smaller licensed population is rejected, so that hit
#' counts and corpus-part sizes cannot silently refer to different populations.
#'
#' All metadata categories are discovered independently of the query hits.
#' The current API does not offer a public distinct-values endpoint, so the
#' library retrieves one representative text's metadata from each remaining
#' group, excluding already known groups until none remain. It does not
#' download the corpus. The resulting partition is cached per server, index
#' revision, virtual corpus, metadata field and grouping rule when connection
#' caching is enabled. Discovery and size queries can take time on the first
#' call, especially for fields with many categories.
#'
#' `regex` removes a suffix, rather than selecting which categories are included.
#' It must have a corresponding corpus-query regex: use ordinary groups and
#' character classes, without lookarounds. This makes the metadata grouping and
#' the token denominators agree. Advanced arbitrary groupings remain available
#' through [corpusPartition()] and [dispersionQuery()].
#' @return A `KorAPDispersion` tibble. Printing it shows all requested scores.
#'   Detailed counts remain in `distribution`; `sampling` records whether the
#'   scores are estimated. The complete partitions are also retained as the
#'   `partitions` attribute. See [dispersionQuery()] for the other columns.
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' q <- corpusQuery(kco, "NutzerInnen", vc = "pubPlaceKey=AT & pubDate in 2022")
#' dispersionAnalysis(q, by = "source")
#' # The explicit metadata-field equivalent:
#' dispersionAnalysis(q, by = "corpusTitle", regex = " [0-9]{4}$")
#' }
#' @export
setMethod("dispersionAnalysis", "KorAPQuery",
  function(frequencies, by = "source", regex = NULL,
           scoreFunctions = defaultDispersionFunctions(), exactFrequencies = NULL,
           searchHitsSampleLimit = 20000, seed = 7,
           verbose = frequencies@korapConnection@verbose, cacheAs = NULL) {
    analyseDispersionQuery(frequencies, by, regex, scoreFunctions,
      exactFrequencies, searchHitsSampleLimit, seed, verbose, cacheAs)
  })

analyseDispersionQuery <- function(frequencies, by, regex, scoreFunctions,
                                   exactFrequencies, searchHitsSampleLimit,
                                   seed, verbose, cacheAs = NULL, partitions = NULL,
                                   initialMetadata = NULL) {
  query <- frequencies
  kco <- query@korapConnection
  checkDispersionQuery(query)
  if (!is.character(by) || !length(by) || anyNA(by) || any(!nzchar(by)) || anyDuplicated(by)) {
    stop("by must contain distinct metadata field names.", call. = FALSE)
  }
  if (length(by) > 1 && !is.null(regex)) {
    stop("Use a single metadata field when supplying regex.", call. = FALSE)
  }
  dimensions <- lapply(by, dispersionDimension, regex = regex)
  fields <- unique(vapply(dimensions, `[[`, character(1), "field"))
  query <- prepareDispersionMetadata(query, fields)
  if (is.null(exactFrequencies)) exactFrequencies <- is.null(attr(query, "korapSampling")) &&
    !completeDispersionMetadata(query)
  validateDispersionControls(scoreFunctions, exactFrequencies, searchHitsSampleLimit, seed)
  parameters <- httr2::url_parse(query@requestUrl)$query
  collection <- query@apiResponse$collection
  if (is.null(collection)) collection <- query@apiResponse$corpus
  scopes <- collection$rewrites$scope
  if (!identical(parameters[["access-rewrite-disabled"]], "true") &&
      any(grepl("^availability\\(", scopes) & scopes != "availability(ALL)")) {
    stop("This full-text query was restricted by access rights. Use corpusQuery(..., metadataOnly = TRUE) so hits and corpus sizes describe the same population.", call. = FALSE)
  }
  expression <- parameters$q
  if (is.null(expression) || !nzchar(expression)) {
    stop("Use a query object returned by corpusQuery(), with its original request URL.", call. = FALSE)
  }
  ql <- if (is.null(parameters$ql)) "poliqarp" else parameters$ql
  record <- NULL
  if (!is.null(cacheAs)) {
    cacheAs <- cacheAsFileName(cacheAs)
    record <- cacheAsRecord(environment(), NULL, kco)
    # Do not serialize the query's connection or credentials as parameters.
    record$parameters$frequencies <- list(query = expression, vc = query@vc, ql = ql)
    input <- dispersionFetchedInput(query)
    if (!is.null(input)) record$parameters$fetchedInput <- input
    cached <- readCacheAs(cacheAs, kco, record, "dispersion query")
    if (!is.null(cached)) return(cached)
  }
  sampled <- if (!exactFrequencies) sampleDispersionMetadata(query, searchHitsSampleLimit, seed, verbose) else NULL
  if (is.null(partitions)) {
    partitions <- stats::setNames(lapply(seq_along(by), function(i) {
      initial <- if (is.null(sampled) || !nrow(sampled$matches)) NULL else
        sampled$matches[[dimensions[[i]]$field]]
      if (length(initialMetadata)) {
        field <- dimensions[[i]]$field
        known <- lapply(initialMetadata, function(metadata) {
          if (!is.null(metadata) && field %in% names(metadata))
            dispersionMetadataValues(metadata[[field]]) else character()
        })
        initial <- c(dispersionMetadataValues(initial), unlist(known, use.names = FALSE))
      }
      discoverDispersionPartition(kco, by[i], query@vc, regex = regex,
        verbose = verbose, initialValues = initial)
    }), by)
  } else {
    if (!identical(names(partitions), by) || !all(vapply(partitions, function(p) {
      spec <- attr(p, "partition")
      inherits(p, "KorAPCorpusPartition") && identical(spec$baseVc, query@vc) &&
        identical(spec$apiUrl, kco@apiUrl) && identical(spec$indexRevision, kco@indexRevision)
    }, logical(1)))) stop("Shared partitions must describe the same corpus and metadata dimensions.", call. = FALSE)
  }
  specs <- lapply(partitions, attr, which = "partition")
  result <- calculateQueryDispersion(query, expression, partitions, specs,
    scoreFunctions, exactFrequencies, searchHitsSampleLimit, seed, ql, verbose, sampled)
  attr(result, "partitions") <- partitions
  if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "dispersion query", result)
  result
}

#' @rdname dispersionAnalysis-KorAPQuery-method
#' @export
setMethod("dispersionAnalysis", "data.frame",
  function(frequencies, by = "source", regex = NULL,
           scoreFunctions = defaultDispersionFunctions(), exactFrequencies = NULL,
           searchHitsSampleLimit = 20000, seed = 7, verbose = NULL, cacheAs = NULL) {
    queries <- korapQueryObjects(frequencies)
    kco <- queries[[1]]@korapConnection
    validateDispersionControls(scoreFunctions, if (is.null(exactFrequencies)) TRUE else exactFrequencies,
      searchHitsSampleLimit, seed)
    labels <- attr(frequencies, "korapQueryLabels")
    if (is.null(labels)) labels <- as.character(frequencies$query)
    record <- NULL
    if (!is.null(cacheAs)) {
      cacheAs <- cacheAsFileName(cacheAs)
      record <- cacheAsRecord(environment(), NULL, kco)
      record$parameters$frequencies <- list(query = as.character(frequencies$query),
        vc = as.character(frequencies$vc), labels = labels)
      record$parameters$queryInputs <- lapply(queries, function(q) list(
        ql = httr2::url_parse(q@requestUrl)$query$ql,
        fetchedInput = dispersionFetchedInput(q)))
      cached <- readCacheAs(cacheAs, kco, record, "dispersion comparison")
      if (!is.null(cached)) {
        warnDispersionComparison(cached)
        return(cached)
      }
    }
    shared <- list()
    results <- lapply(seq_along(queries), function(i) {
      q <- queries[[i]]
      matching <- which(vapply(shared, function(entry)
        identical(entry$vc, q@vc) && identical(entry$apiUrl, q@korapConnection@apiUrl) &&
        identical(entry$indexRevision, q@korapConnection@indexRevision), logical(1)))
      parts <- if (length(matching)) shared[[matching[1]]]$partitions else NULL
      related <- vapply(queries, function(other)
        identical(other@vc, q@vc) && identical(other@korapConnection@apiUrl, q@korapConnection@apiUrl) &&
        identical(other@korapConnection@indexRevision, q@korapConnection@indexRevision), logical(1))
      result <- analyseDispersionQuery(q, by, regex, scoreFunctions, exactFrequencies,
        searchHitsSampleLimit, seed,
        if (is.null(verbose)) q@korapConnection@verbose else verbose,
        partitions = parts,
        initialMetadata = lapply(queries[related], function(other) {
          if (!identical(attr(other, "korapMetadataValues"), "joined")) other@collectedMatches
        }))
      if (is.null(parts)) shared[[length(shared) + 1L]] <<- list(vc = q@vc,
        apiUrl = q@korapConnection@apiUrl, indexRevision = q@korapConnection@indexRevision,
        partitions = attr(result, "partitions"))
      result$variant <- labels[i]
      result$vc <- q@vc
      result
    })
    result <- asDispersionResult(dplyr::bind_rows(results), names(scoreFunctions))
    attr(result, "comparison") <- TRUE
    attr(result, "partitions") <- lapply(shared, `[[`, "partitions")
    warnDispersionComparison(result)
    if (!is.null(cacheAs)) writeCacheAs(cacheAs, kco, record, "dispersion comparison", result)
    result
  })

dispersionFetchedInput <- function(query) {
  if (is.null(query@collectedMatches) && is.null(attr(query, "korapSampling"))) return(NULL)
  list(sampling = attr(query, "korapSampling"),
    metadataValues = attr(query, "korapMetadataValues"),
    metadataChecksum = R.cache::getChecksum(query@collectedMatches))
}

validateDispersionControls <- function(scoreFunctions, exactFrequencies, limit, seed) {
  validateDispersionFunctions(scoreFunctions)
  reserved <- c("query", "variant", "vc", "partition", "totalResults", "sampledHits", "usedHits",
    "excludedHits", "nParts", "estimated", "rangeIsLowerBound", "distribution", "sampling")
  if (any(names(scoreFunctions) %in% reserved)) stop("Score names conflict with dispersion result columns.", call. = FALSE)
  if (!is.logical(exactFrequencies) || length(exactFrequencies) != 1 || is.na(exactFrequencies)) {
    stop("exactFrequencies must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit <= 0 ||
      (is.finite(limit) && limit != floor(limit))) {
    stop("searchHitsSampleLimit must be a positive integer or Inf.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != floor(seed)) {
    stop("seed must be a non-negative integer.", call. = FALSE)
  }
}
