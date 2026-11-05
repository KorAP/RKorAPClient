# Metadata grouping and complete category discovery for dispersion analyses.
# These helpers deliberately live in the library, not in analysis scripts.

dispersionDimension <- function(by, regex = NULL) {
  dispersionScalarString(by, "by")
  aliases <- c(source = "corpusTitle", year = "pubDate", region = "pubPlaceKey")
  field <- if (by %in% names(aliases)) unname(aliases[by]) else by
  if (!grepl("^[[:alpha:]][[:alnum:]_.:-]*$", field)) {
    stop("by must be a metadata field name.", call. = FALSE)
  }
  year <- field %in% c("pubDate", "creationDate")
  if (is.null(regex) && identical(by, "source")) regex <- " [0-9]{4}$"
  if (!is.null(regex)) {
    dispersionScalarString(regex, "regex")
    if (year) stop("The year dimension already groups publication dates by year.", call. = FALSE)
    # A trailing-suffix rule has an exact inverse for corpus-size queries:
    # source + optional suffix. Arbitrary replacements need explicit parts.
    if (!endsWith(regex, "$") || endsWith(regex, "\\$") || startsWith(regex, "^") ||
        grepl("(?", regex, fixed = TRUE)) {
      stop("regex must describe a trailing suffix ending in $, e.g. ' [0-9]{4}$'; lookarounds and leading ^ are not supported.", call. = FALSE)
    }
    valid <- tryCatch(!grepl(regex, "", perl = TRUE), error = function(e) FALSE,
                      warning = function(w) FALSE)
    if (!valid) stop("regex must be valid and must not match an empty string.", call. = FALSE)
  }
  list(name = by, field = field, regex = regex, year = year)
}

dispersionMetadataValues <- function(values) {
  if (is.list(values)) {
    if (any(lengths(values) > 1)) {
      stop("Automatic dispersion requires a single metadata value per text; use explicit parts and classify for multivalued fields.", call. = FALSE)
    }
    values <- vapply(values, function(x) if (!length(x)) NA_character_ else as.character(x), character(1))
  }
  values <- as.character(values)
  if (any(grepl("\t", values, fixed = TRUE), na.rm = TRUE)) {
    stop("Automatic dispersion requires single-valued metadata.", call. = FALSE)
  }
  values[is.na(values) | !nzchar(values)] <- NA_character_
  values
}

dispersionGroupValues <- function(values, dimension) {
  values <- dispersionMetadataValues(values)
  if (dimension$year) {
    values <- substr(values, 1, 4)
    values[!is.na(values) & !grepl("^[0-9]{4}$", values)] <- NA_character_
    values[!is.na(values) & values == "0000"] <- NA_character_
  } else if (!is.null(dimension$regex)) {
    values <- sub(dimension$regex, "", values, perl = TRUE)
  }
  if (any(!is.na(values) & !nzchar(values))) {
    stop("The grouping regex removed an entire metadata value; it must remove only a trailing suffix.", call. = FALSE)
  }
  values
}

dispersionRegexLiteral <- function(value) {
  chars <- strsplit(enc2utf8(value), "", fixed = TRUE)[[1]]
  special <- c("\\", ".", "?", "+", "*", "|", "{", "}", "[", "]", "(", ")", '"', "#", "@", "&", "<", ">", "~", "/")
  paste0(ifelse(chars %in% special, paste0("\\", chars), chars), collapse = "")
}

dispersionSuffixRegex <- function(regex) {
  body <- substr(regex, 1, nchar(regex) - 1)
  body <- gsub("\\d", "[0-9]", body, fixed = TRUE)
  body <- gsub("\\s", "[ \t\r\n]", body, fixed = TRUE)
  body
}

dispersionGroupCondition <- function(label, dimension, exclude = FALSE) {
  field <- dimension$field
  if (dimension$year) {
    year <- as.integer(label)
    if (!exclude) return(paste(field, "in", label))
    # Date intervals are used instead of treating a date as a string field.
    boundaries <- c(
      if (year > 1L) paste(field, "until", sprintf("%04d-12-31", year - 1L)),
      if (year < 9999L) paste(field, "since", sprintf("%04d-01-01", year + 1L)))
    return(paste0("(", paste(boundaries, collapse = " | "), ")"))
  }
  value <- if (is.null(dimension$regex)) quoteDispersionValue(label) else
    paste0("/", dispersionRegexLiteral(label), "(", dispersionSuffixRegex(dimension$regex), ")?/")
  paste(field, if (exclude) "!=" else "=", value)
}

dispersionMetadataClassifier <- function(dimension) {
  force(dimension)
  function(matches) dispersionGroupValues(matches[[dimension$field]], dimension)
}

dispersionGroupSetCondition <- function(labels, dimension, exclude = FALSE) {
  if (!length(labels)) return(character())
  if (dimension$year || length(labels) == 1L) {
    conditions <- vapply(labels, dispersionGroupCondition, character(1),
      dimension = dimension, exclude = exclude)
    return(paste0("(", paste(paste0("(", conditions, ")"),
      collapse = if (exclude) " & " else " | "), ")"))
  }
  # Share the metadata field and any suffix across alternatives. Repeating them
  # for every source can push a GET request past the server's URL limit.
  alternatives <- paste(vapply(labels, dispersionRegexLiteral, character(1)), collapse = "|")
  suffix <- if (is.null(dimension$regex)) "" else
    paste0("(", dispersionSuffixRegex(dimension$regex), ")?")
  pattern <- paste0("(", alternatives, ")", suffix)
  paste(dimension$field, if (exclude) "!=" else "=", paste0("/", pattern, "/"))
}

discoverDispersionPartition <- function(kco, by, vc, regex = NULL,
                                        verbose = kco@verbose, initialValues = NULL) {
  dimension <- dispersionDimension(by, regex)
  key <- list("dispersion-partition-v1", kco@apiUrl, kco@indexRevision,
              kco@accessToken, vc, dimension)
  if (kco@cache) {
    cached <- R.cache::loadCache(key = key, dir = KorAPCacheSubDir())
    if (!is.null(cached)) {
      log_info(verbose, "Reusing metadata groups for ", by, "\n")
      warnDispersionCoverage(attr(cached, "partition")$excludedTokens)
      return(cached)
    }
  }
  labels <- if (is.null(initialValues)) character() else
    unique(stats::na.omit(dispersionGroupValues(initialValues, dimension)))
  labels <- as.character(labels)
  present <- if (dimension$year) paste(dimension$field, "since 0001") else
    paste0(dimension$field, " = /.+/")
  log_info(verbose, "Finding all metadata groups for ", by,
           if (!is.null(dimension$regex)) paste0(" (removing ", dimension$regex, ")"), "\n")
  repeat {
    exclusions <- dispersionGroupSetCondition(labels, dimension, exclude = TRUE)
    remaining <- paste(paste0("(", c(present, exclusions), ")"), collapse = " & ")
    remaining <- intersectDispersionVc(vc, remaining)
    probe <- strictDispersionRequest(corpusQuery(kco, "<base/s=t>", vc = remaining,
      metadataOnly = TRUE, fields = dimension$field, verbose = FALSE))
    checkDispersionQuery(probe)
    if (probe@totalResults == 0) break
    probe <- strictDispersionRequest(fetchNext(probe, offset = 0, maxFetch = 1,
      verbose = FALSE, preserveMetadataValues = TRUE))
    checkDispersionQuery(probe)
    if (is.null(probe@collectedMatches) || nrow(probe@collectedMatches) != 1 ||
        !dimension$field %in% names(probe@collectedMatches)) {
      stop("Cannot discover metadata groups: a representative text's metadata are unavailable.", call. = FALSE)
    }
    label <- dispersionGroupValues(probe@collectedMatches[[dimension$field]], dimension)
    if (length(label) != 1 || is.na(label) || label %in% labels) {
      stop("Metadata-group discovery could not exclude an already visited or missing value; no incomplete partition was returned.", call. = FALSE)
    }
    labels <- c(labels, label)
    log_info(verbose, "  ", length(labels), ": ", label, "\n")
  }
  if (!length(labels)) {
    stop("No values found for metadata field '", dimension$field,
         "' in this virtual corpus. Check the field name and corpus selection.", call. = FALSE)
  }
  labels <- sort(labels)
  definitions <- stats::setNames(vapply(labels, dispersionGroupCondition, character(1),
                                         dimension = dimension), labels)
  result <- assembleDispersionPartition(kco, dimension$field, vc, definitions,
    classify = dispersionMetadataClassifier(dimension), fields = dimension$field,
    verbose = verbose, year = dimension$year,
    unionCondition = dispersionGroupSetCondition(labels, dimension))
  attr(result, "partition")$automatic <- TRUE
  attr(result, "partition")$regex <- dimension$regex
  attr(result, "partition")$year <- dimension$year
  if (kco@cache) R.cache::saveCache(result, key = key, dir = KorAPCacheSubDir(), compress = TRUE)
  result
}

prepareDispersionMetadata <- function(query, fields) {
  fields <- unique(c(setdiff(query@fields, c("snippet", "tokens")), fields, "textSigle"))
  query@fields <- fields
  parsed <- httr2::url_parse(query@requestUrl)$query
  parsed$fields <- paste(fields, collapse = ",")
  parsed[["show-tokens"]] <- NULL
  query@requestUrl <- httr2::url_modify(query@requestUrl, query = parsed)
  query
}
