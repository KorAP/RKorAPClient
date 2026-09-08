#' Keeping results in a file of one's own
#'
#' The `cacheAs` parameter of the query functions is a different thing from the
#' `cache` parameter of [KorAPConnection()]. The latter is a transparent
#' speed-up: it stores server responses where the package finds them again, and
#' throwing it away costs nothing but time. The former stores a finished result
#' in a file the caller names and keeps, which is what makes an analysis
#' reproducible: KorAP corpora grow, so the same query returns different numbers
#' next year, and the scores computed from them may change with the package.
#'
#' A cache file therefore records what produced it, and is not reused when that
#' no longer matches what is being asked for.
#'
#' @name cacheAs
#' @keywords internal
NULL

#' Attribute under which cache files record what produced them
#' @noRd
cacheAsAttribute <- "RKorAPClient.cacheAs"

#' First version whose association scores are still computed the same way today
#'
#' `logDice()` and `ll()` were corrected in 1.4.0, so a file written by an
#' earlier version holds numbers that would not be arrived at again, which no
#' comparison of parameters can notice.
#'
#' Deliberately a constant of its own rather than the package version: what a
#' cache file has to be checked against is the generation of the scores, which
#' changes far more rarely than the version does, and reading it out of
#' DESCRIPTION would make the check depend on a release having been prepared.
#' Raise it whenever a score changes.
#' @noRd
cacheAsScoreVersion <- "1.4.0"

#' Append .rds to a cache file name that does not end in it
#' @noRd
cacheAsFileName <- function(cacheAs) {
  if (grepl("\\.rds$", cacheAs, ignore.case = TRUE)) cacheAs else paste0(cacheAs, ".rds")
}

#' What a cached result was computed with
#'
#' Collected from the calling function's frame, so that parameters added in the
#' future are taken into account automatically. `kco` and `cacheAs` are
#' excluded: the former is not a parameter of the analysis, the latter only says
#' where to store it.
#'
#' @param frame environment of the calling query function
#' @param dots its `...` arguments, or `NULL` where it has none
#' @param kco [KorAPConnection()] object
#' @return list to store with, and compare against, a cache file
#' @noRd
cacheAsRecord <- function(frame, dots, kco) {
  parameterNames <- setdiff(
    names(formals(sys.function(sys.parent()))),
    # verbose says how loud the computation is, not what it computes
    c("kco", "cacheAs", "verbose", "...")
  )
  list(
    # what has to match on the next call
    scoreVersion = cacheAsScoreVersion,
    # recorded so that a file can say where its numbers come from
    packageVersion = as.character(utils::packageVersion("RKorAPClient")),
    parameters = mget(parameterNames, envir = frame),
    dots = dots,
    # reusing one cache file for two KorAP instances is a mistake worth catching
    apiUrl = kco@apiUrl,
    # recorded for reference only, deliberately not compared: corpus updates
    # should not invalidate a deliberately kept result
    indexRevision = kco@indexRevision
  )
}

#' Why a cache file cannot be used for the current call
#'
#' @param stored record read from the cache file, `NULL` if it has none
#' @param current record of the call at hand
#' @return a sentence naming the reason, or `NULL` if the file can be used
#' @noRd
cacheAsRejectionReason <- function(stored, current) {
  generation <- if (is.null(stored)) NULL else stored$scoreVersion

  if (is.null(generation) || package_version(generation) < package_version(cacheAsScoreVersion)) {
    writtenBy <- if (is.null(stored)) NULL else stored$packageVersion
    return(if (is.null(writtenBy)) {
      sprintf(
        "was written before RKorAPClient %s, which corrected logDice and ll",
        cacheAsScoreVersion
      )
    } else {
      sprintf(
        "was written by RKorAPClient %s, whose logDice and ll differ from those of %s",
        writtenBy, cacheAsScoreVersion
      )
    })
  }

  differing <- character(0)
  for (name in union(names(stored$parameters), names(current$parameters))) {
    if (!identical(stored$parameters[[name]], current$parameters[[name]])) {
      differing <- c(differing, name)
    }
  }
  if (!identical(stored$dots, current$dots)) {
    differing <- c(differing, "...")
  }
  if (!identical(stored$apiUrl, current$apiUrl)) {
    differing <- c(differing, "KorAP instance")
  }

  if (length(differing) == 0) {
    NULL
  } else {
    sprintf("was created with different parameters (%s)", paste(differing, collapse = ", "))
  }
}

#' What produced a cacheAs file
#'
#' Reads back what a query function recorded in a [cacheAs] file: the parameters
#' it was called with, the KorAP instance it asked, the index revision that
#' instance's corpus had at the time, and the version of RKorAPClient that wrote
#' the file. Useful for saying, of a result kept next to a document, what the
#' numbers in it rest on.
#'
#' @param cacheAs path to the file, with or without its `.rds` extension
#' @return a list with the elements `scoreVersion`, `packageVersion`,
#'   `parameters`, `dots`,
#'   `apiUrl` and `indexRevision`, or `NULL` for a file written by a version
#'   before 1.4.0, which recorded none of this
#'
#' @examples
#' \dontrun{
#' KorAPConnection() |> frequencyQuery("Ameisenplage", cacheAs = "ameisenplage.rds")
#' cacheAsInfo("ameisenplage.rds")
#' }
#'
#' @family cacheAs
#' @export
cacheAsInfo <- function(cacheAs) {
  cacheAs <- cacheAsFileName(cacheAs)
  if (!file.exists(cacheAs)) {
    stop(sprintf("Cache file '%s' does not exist.", cacheAs), call. = FALSE)
  }
  attr(readRDS(cacheAs), cacheAsAttribute)
}

#' Read back a result stored in a cache file, if it is the one being asked for
#'
#' Warns and returns `NULL` where the file exists but does not match, so that
#' the caller recomputes and [writeCacheAs()] overwrites it.
#'
#' @param cacheAs cache file name, already passed through [cacheAsFileName()]
#' @param kco [KorAPConnection()] object, for its `verbose` flag
#' @param record what the call at hand computes, from [cacheAsRecord()]
#' @param what name of the result, for the log and warning messages
#' @return the cached result, or `NULL` if there is none to use
#' @noRd
readCacheAs <- function(cacheAs, kco, record, what) {
  if (!file.exists(cacheAs)) {
    return(NULL)
  }

  cached <- readRDS(cacheAs)
  stored <- attr(cached, cacheAsAttribute)
  attr(cached, cacheAsAttribute) <- NULL

  reason <- cacheAsRejectionReason(stored, record)
  if (is.null(reason)) {
    log_info(kco@verbose, sprintf("Loading %s from cache: %s\n", what, cacheAs))
    return(cached)
  }

  warning(
    sprintf(
      paste0(
        "Cache file '%s' %s.\n",
        "It is recomputed and overwritten; pass a different cacheAs file name to keep it."
      ),
      cacheAs, reason
    ),
    call. = FALSE
  )
  NULL
}

#' Store a result in a cache file, together with what produced it
#'
#' @param cacheAs cache file name, already passed through [cacheAsFileName()]
#' @param kco [KorAPConnection()] object, for its `verbose` flag
#' @param record what produced the result, from [cacheAsRecord()]
#' @param what name of the result, for the log message
#' @param result the result to store
#' @return `result`, invisibly and unchanged
#' @noRd
writeCacheAs <- function(cacheAs, kco, record, what, result) {
  log_info(kco@verbose, sprintf("Saving %s to cache: %s\n", what, cacheAs))
  # only the stored copy carries the record, so that the returned value is the
  # same whether it was cached or not
  cachedResult <- result
  attr(cachedResult, cacheAsAttribute) <- record
  saveRDS(cachedResult, cacheAs)
  invisible(result)
}
