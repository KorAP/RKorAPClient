#' @include logging.R
setGeneric("collocationAnalysis", function(kco, ...) standardGeneric("collocationAnalysis"))

#' Collocation analysis
#'
#' @family collocation analysis functions
#' @aliases collocationAnalysis
#'
#' @description
#'
#' Performs a collocation analysis for the given node (or query)
#' in the given virtual corpus.
#'
#' @details
#' The collocation analysis is currently implemented on the client side, as some of the
#' functionality is not yet provided by the KorAP backend. Mainly for this reason
#' it is very slow (several minutes, up to hours), but on the other hand very flexible.
#' You can, for example, perform the analysis in arbitrary virtual corpora, use complex node queries,
#' and look for expression-internal collocates using the focus function (see examples and demo).
#'
#' To increase speed at the cost of accuracy and possible false negatives,
#' you can decrease searchHitsSampleLimit and/or topCollocatesLimit and/or set exactFrequencies to FALSE.
#'
#' Note that some outdated non-DeReKo back-ends might not yet support returning tokenized matches (warning issued).
#' In this case, the client library will fall back to client-side tokenization which might be slightly less accurate.
#' This might lead to false negatives and to frequencies that differ from corresponding ones acquired via the web
#' user interface.
#'
#'
#' @param lemmatizeNodeQuery     if TRUE, node query will be lemmatized, i.e. `x -> [tt/l=x]`
#' @param minOccur               minimum absolute number of observed co-occurrences to consider a collocate candidate
#' @param topCollocatesLimit     limit analysis to the n most frequent collocates in the search hits sample
#' @param searchHitsSampleLimit  limit the size of the search hits sample
#' @param stopwords              vector of stopwords not to be considered as collocates
#' @param withinSpan             KorAP span specification (see <https://korap.ids-mannheim.de/doc/ql/poliqarp-plus?embedded=true#spans>) for collocations to be searched within. Defaults to `base/s=s`.
#' @param exactFrequencies       if FALSE, extrapolate observed co-occurrence frequencies from frequencies in search hits sample, otherwise retrieve exact co-occurrence frequencies
#' @param seed                   seed for random page collecting order
#' @param expand                 if TRUE, `node` and `vc` parameters are expanded to all of their combinations
#' @param maxRecurse             apply collocation analysis recursively `maxRecurse` times
#' @param addExamples            If TRUE, examples for instances of collocations will be added in a column `example`. This makes a difference in particular if `node` is given as a lemma query.
#' @param thresholdScore         association score function (see \code{\link{association-score-functions}}) to use for computing the threshold that is applied for recursive collocation analysis calls
#' @param threshold              minimum value of `thresholdScore` function call to apply collocation analysis recursively
#' @param localStopwords         vector of stopwords that will not be considered as collocates in the current function call, but that will not be passed to recursive calls
#' @param collocateFilterRegex   allow only collocates matching the regular expression
#' @param multiVcMissingScoreFactor factor that is multiplied with the minimum observed score when imputing missing scores for delta computations between virtual corpora
#' @param vcLabel optional label override for the current virtual corpus (used internally when named VC collections are expanded)
#' @param ...                    more arguments will be passed to [collocationScoreQuery()]
#' @inheritParams collocationScoreQuery,KorAPConnection-method
#' @return Tibble with top collocates, association scores, corresponding URLs for web user interface queries, etc.
#'
#' @importFrom dplyr arrange desc slice_head bind_rows group_by mutate ungroup left_join select row_number all_of first
#' @importFrom purrr pmap
#' @importFrom tidyr expand_grid pivot_wider
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#'
#' # Find top collocates of "Packung" inside and outside the sports domain.
#' KorAPConnection(verbose = TRUE) |>
#'   collocationAnalysis("Packung",
#'     vc = c("textClass=sport", "textClass!=sport"),
#'     leftContextSize = 1, rightContextSize = 1, topCollocatesLimit = 20
#'   ) |>
#'   dplyr::filter(logDice >= 5)
#' }
#'
#' \dontrun{
#'
#' # Identify the most prominent light verb construction with "in ... setzen".
#' # Note that, currently, the use of focus function disallows exactFrequencies.
#' KorAPConnection(verbose = TRUE) |>
#'   collocationAnalysis("focus(in [tt/p=NN] {[tt/l=setzen]})",
#'     leftContextSize = 1, rightContextSize = 0, exactFrequencies = FALSE, topCollocatesLimit = 20
#'   )
#' }
#'
#' @export
setMethod(
  "collocationAnalysis", "KorAPConnection",
  function(kco,
           node,
           vc = "",
           lemmatizeNodeQuery = FALSE,
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           topCollocatesLimit = 200,
           searchHitsSampleLimit = 20000,
           ignoreCollocateCase = FALSE,
           withinSpan = ifelse(exactFrequencies, "base/s=s", ""),
           exactFrequencies = TRUE,
           stopwords = append(RKorAPClient::synsemanticStopwords(), node),
           seed = 7,
           expand = length(vc) != length(node),
           maxRecurse = 0,
           addExamples = FALSE,
           thresholdScore = "logDice",
           threshold = 2.0,
           localStopwords = c(),
           collocateFilterRegex = "^[:alnum:]+-?[:alnum:]*$",
           multiVcMissingScoreFactor = 0.9,
           vcLabel = NA_character_,
           ...) {
    # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  word <- frequency <- O <- NULL

    if (!exactFrequencies && (!is.na(withinSpan) && !is.null(withinSpan) && nchar(withinSpan) > 0)) {
      stop(sprintf("Not empty withinSpan (='%s') requires exactFrequencies=TRUE", withinSpan), call. = FALSE)
    }

    warnIfNotAuthorized(kco)

    if (lemmatizeNodeQuery) {
      node <- lemmatizeWordQuery(node)
    }

    vcNames <- names(vc)
    vc <- unname(unlist(vc, use.names = FALSE))
    if (is.null(vcNames)) {
      vcNames <- rep(NA_character_, length(vc))
    } else {
      vcNames[vcNames == ""] <- NA_character_
      if (length(vcNames) < length(vc)) {
        vcNames <- rep(vcNames, length.out = length(vc))
      }
    }

    label_lookup <- NULL
    if (length(vc) > 0 && any(!is.na(vcNames))) {
      valid_lookup <- !is.na(vcNames)
      label_lookup <- vcNames[valid_lookup]
      names(label_lookup) <- vc[valid_lookup]
    }

    result <- if (length(node) > 1 || length(vc) > 1) {
      grid <- if (expand) {
  tmp_grid <- expand_grid(node = node, idx = seq_along(vc))
  tmp_grid$vc <- vc[tmp_grid$idx]
  tmp_grid$vcLabel <- vcNames[tmp_grid$idx]
  tmp_grid[, setdiff(names(tmp_grid), "idx"), drop = FALSE]
      } else {
        tibble(node = node, vc = vc, vcLabel = vcNames)
      }

      multi_result <- purrr::pmap(grid, function(node, vc, vcLabel, ...) {
        collocationAnalysis(kco,
          node = node,
          vc = vc,
          minOccur = minOccur,
          leftContextSize = leftContextSize,
          rightContextSize = rightContextSize,
          topCollocatesLimit = topCollocatesLimit,
          searchHitsSampleLimit = searchHitsSampleLimit,
          ignoreCollocateCase = ignoreCollocateCase,
          withinSpan = withinSpan,
          exactFrequencies = exactFrequencies,
          stopwords = stopwords,
          addExamples = TRUE,
          localStopwords = localStopwords,
          seed = seed,
          expand = expand,
          multiVcMissingScoreFactor = multiVcMissingScoreFactor,
          vcLabel = vcLabel,
          ...
        )
      }) |>
        bind_rows()

      if (!"vc" %in% names(multi_result) || nrow(multi_result) == 0) {
        multi_result
      } else {
        if (!"label" %in% names(multi_result)) {
          multi_result$label <- NA_character_
        }

        if (!is.null(label_lookup)) {
          override <- unname(label_lookup[multi_result$vc])
          missing_idx <- is.na(multi_result$label) | multi_result$label == ""
          if (any(missing_idx)) {
            multi_result$label[missing_idx] <- override[missing_idx]
          }
        }

        missing_idx <- is.na(multi_result$label) | multi_result$label == ""
        if (any(missing_idx)) {
          multi_result$label[missing_idx] <- queryStringToLabel(multi_result$vc[missing_idx])
        }

        multi_result |>
          add_multi_vc_comparisons(thresholdScore = thresholdScore, missingScoreFactor = multiVcMissingScoreFactor)
      }
    } else {
      if ((is.na(vcLabel) || vcLabel == "") && length(vcNames) >= 1) {
        vcLabel <- vcNames[1]
      }
      set.seed(seed)
      candidates <- collocatesQuery(
        kco,
        node,
        vc = vc,
        minOccur = minOccur,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        searchHitsSampleLimit = searchHitsSampleLimit,
        ignoreCollocateCase = ignoreCollocateCase,
        stopwords = append(stopwords, localStopwords),
        ...
      )

      if (nrow(candidates) > 0) {
        candidates <- candidates |>
          filter(frequency >= minOccur) |>
          slice_head(n = topCollocatesLimit)
        collocationScoreQuery(
          kco,
          node = node,
          collocate = candidates$word,
          vc = vc,
          leftContextSize = leftContextSize,
          rightContextSize = rightContextSize,
          observed = if (exactFrequencies) NA else candidates$frequency,
          ignoreCollocateCase = ignoreCollocateCase,
          withinSpan = withinSpan,
          ...
        ) |>
          filter(O >= minOccur) |>
          dplyr::arrange(dplyr::desc(logDice))
      } else {
        tibble()
      }
    }
      if (!is.na(vcLabel) && vcLabel != "" && "label" %in% names(result)) {
        result$label <- rep(vcLabel, nrow(result))
      }
    if (maxRecurse > 0 & length(result) > 0 && any(!!thresholdScore >= threshold)) {
      recurseWith <- result |>
        filter(!!as.name(thresholdScore) >= threshold)
      result <- collocationAnalysis(
        kco,
        node = paste0("(", buildCollocationQuery(
          removeWithinSpan(recurseWith$node, withinSpan),
          recurseWith$collocate,
          leftContextSize = leftContextSize,
          rightContextSize = rightContextSize,
          withinSpan = ""
        ), ")"),
        vc = vc,
        minOccur = minOccur,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        withinSpan = withinSpan,
        maxRecurse = maxRecurse - 1,
        stopwords = stopwords,
        localStopwords = recurseWith$collocate,
        exactFrequencies = exactFrequencies,
        searchHitsSampleLimit = searchHitsSampleLimit,
        topCollocatesLimit = topCollocatesLimit,
        addExamples = FALSE,
        multiVcMissingScoreFactor = multiVcMissingScoreFactor,
        vcLabel = vcLabel
      ) |>
        bind_rows(result) |>
        filter(logDice >= 2) |>
        filter(O >= minOccur) |>
        dplyr::arrange(dplyr::desc(logDice))
    }
    if (addExamples && length(result) > 0) {
      result$query <- buildCollocationQuery(
        result$node,
        result$collocate,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        withinSpan = withinSpan
      )
      result$example <- findExample(
        kco,
        query = result$query,
        vc = result$vc
      )
    }
    result
  }
)

# #' @export
removeWithinSpan <- function(query, withinSpan) {
  if (withinSpan == "") {
    return(query)
  }
  needle <- sprintf("^\\(contains\\(<%s>, ?(.*)\\){2}$", withinSpan)
  res <- gsub(needle, "\\1", query)
  needle <- sprintf("^contains\\(<%s>, ?(.*)\\)$", withinSpan)
  res <- gsub(needle, "\\1", res)
  return(res)
}

add_multi_vc_comparisons <- function(result, thresholdScore, missingScoreFactor) {
  label <- node <- collocate <- rankWithinLabel <- NULL

  if (!"label" %in% names(result) || dplyr::n_distinct(result$label) < 2) {
    return(result)
  }

  numeric_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  non_score_cols <- c("N", "O", "O1", "O2", "E", "w", "leftContextSize", "rightContextSize", "frequency")
  score_cols <- setdiff(numeric_cols, non_score_cols)

  if (length(score_cols) == 0) {
    return(result)
  }

  ranking_col <- thresholdScore
  if (is.null(ranking_col) || is.na(ranking_col) || !ranking_col %in% score_cols) {
    ranking_col <- if ("logDice" %in% score_cols) "logDice" else score_cols[1]
  }

  ranking_sym <- rlang::sym(ranking_col)

  result <- result |>
    dplyr::group_by(label) |>
    dplyr::mutate(rankWithinLabel = dplyr::row_number(dplyr::desc(!!ranking_sym))) |>
    dplyr::ungroup()

  comparison <- result |>
    dplyr::select(node, collocate, label, rankWithinLabel, dplyr::all_of(score_cols)) |>
    pivot_wider(
      names_from = label,
      values_from = c(rankWithinLabel, dplyr::all_of(score_cols)),
      names_glue = "{.value}_{make.names(label)}",
      values_fn = dplyr::first
    )

  labels <- make.names(unique(result$label))

  if (length(labels) == 2) {
    fill_scores <- function(x, y) {
      min_val <- suppressWarnings(min(c(x, y), na.rm = TRUE))
      if (!is.finite(min_val)) {
        min_val <- 0
      }
      x[is.na(x)] <- missingScoreFactor * min_val
      y[is.na(y)] <- missingScoreFactor * min_val
      list(x = x, y = y)
    }

    fill_ranks <- function(x, y) {
      max_val <- suppressWarnings(max(c(x, y), na.rm = TRUE))
      if (!is.finite(max_val)) {
        max_val <- 0
      }
      x[is.na(x)] <- max_val + 1
      y[is.na(y)] <- max_val + 1
      list(x = x, y = y)
    }

    left_label <- labels[1]
    right_label <- labels[2]

    for (col in score_cols) {
      left_col <- paste0(col, "_", left_label)
      right_col <- paste0(col, "_", right_label)
      if (!all(c(left_col, right_col) %in% names(comparison))) {
        next
      }
      filled <- fill_scores(comparison[[left_col]], comparison[[right_col]])
      comparison[[paste0("delta_", col)]] <- filled$x - filled$y
    }

    left_rank <- paste0("rankWithinLabel_", left_label)
    right_rank <- paste0("rankWithinLabel_", right_label)
    if (all(c(left_rank, right_rank) %in% names(comparison))) {
      filled_rank <- fill_ranks(comparison[[left_rank]], comparison[[right_rank]])
      comparison[["delta_rankWithinLabel"]] <- filled_rank$x - filled_rank$y
    }
  }

  dplyr::left_join(result, comparison, by = c("node", "collocate"))
}

#' @importFrom magrittr debug_pipe
#' @importFrom stringr str_detect
#' @importFrom dplyr as_tibble tibble rename filter anti_join tibble bind_rows case_when
#'
matches2FreqTable <- function(matches,
                              index = 0,
                              minOccur = 5,
                              leftContextSize = 5,
                              rightContextSize = 5,
                              ignoreCollocateCase = FALSE,
                              stopwords = c(),
                              collocateFilterRegex = "^[:alnum:]+-?[:alnum:]*$",
                              oldTable = data.frame(word = rep(NA, 1), frequency = rep(NA, 1)),
                              verbose = TRUE) {
  word <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  frequency <- NULL

  if (nrow(matches) < 1) {
    dplyr::tibble(word = c(), frequency = c())
  } else if (index == 0) {
    if (!"tokens" %in% colnames(matches) || !is.list(matches$tokens)) {
      log_info(verbose, "Outdated KorAP server: Falling back to client side tokenization.\n")
      return(snippet2FreqTable(matches$snippet, minOccur, leftContextSize, rightContextSize,
        ignoreCollocateCase = ignoreCollocateCase,
        stopwords = stopwords, oldTable = oldTable, verbose = verbose
      ))
    }
    log_info(verbose, paste("Joining", nrow(matches), "kwics\n"))
    for (i in seq_len(nrow(matches))) {
      oldTable <- matches2FreqTable(
        matches,
        i,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        collocateFilterRegex = collocateFilterRegex,
        oldTable = oldTable,
        stopwords = stopwords
      )
    }
    log_info(verbose, paste("Aggregating", length(oldTable$word), "tokens\n"))
    oldTable |>
      group_by(word) |>
      mutate(word = dplyr::case_when(ignoreCollocateCase ~ tolower(word), TRUE ~ word)) |>
      summarise(frequency = sum(frequency), .groups = "drop") |>
      arrange(desc(frequency))
  } else {
    stopwordsTable <- dplyr::tibble(word = stopwords)

    left <- tail(unlist(matches$tokens$left[index]), leftContextSize)

    #    cat(paste("left:", left, "\n", collapse=" "))

    right <- head(unlist(matches$tokens$right[index]), rightContextSize)

    #    cat(paste("right:", right, "\n", collapse=" "))

    if (length(left) + length(right) == 0) {
      oldTable
    } else {
      table(c(left, right)) |>
        dplyr::as_tibble(.name_repair = "minimal") |>
        dplyr::rename(word = 1, frequency = 2) |>
        dplyr::filter(str_detect(word, collocateFilterRegex)) |>
        dplyr::anti_join(stopwordsTable, by = "word") |>
        dplyr::bind_rows(oldTable)
    }
  }
}

#' @importFrom magrittr debug_pipe
#' @importFrom stringr str_match str_split str_detect
#' @importFrom dplyr as_tibble tibble rename filter anti_join tibble bind_rows case_when
#'
snippet2FreqTable <- function(snippet,
                              minOccur = 5,
                              leftContextSize = 5,
                              rightContextSize = 5,
                              ignoreCollocateCase = FALSE,
                              stopwords = c(),
                              tokenizeRegex = "([! )(\uc2\uab,.:?\u201e\u201c\'\"]+|&quot;)",
                              collocateFilterRegex = "^[:alnum:]+-?[:alnum:]*$",
                              oldTable = data.frame(word = rep(NA, 1), frequency = rep(NA, 1)),
                              verbose = TRUE) {
  word <- NULL # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  frequency <- NULL

  if (length(snippet) < 1) {
    dplyr::tibble(word = c(), frequency = c())
  } else if (length(snippet) > 1) {
    log_info(verbose, paste("Joining", length(snippet), "kwics\n"))
    for (s in snippet) {
      oldTable <- snippet2FreqTable(
        s,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        collocateFilterRegex = collocateFilterRegex,
        oldTable = oldTable,
        stopwords = stopwords
      )
    }
    log_info(verbose, paste("Aggregating", length(oldTable$word), "tokens\n"))
    oldTable |>
      group_by(word) |>
      mutate(word = dplyr::case_when(ignoreCollocateCase ~ tolower(word), TRUE ~ word)) |>
      summarise(frequency = sum(frequency), .groups = "drop") |>
      arrange(desc(frequency))
  } else {
    stopwordsTable <- dplyr::tibble(word = stopwords)
    match <-
      str_match(
        snippet,
        '<span class="context-left">(<span class="more"></span>)?(.*[^ ]) *</span><span class="match"><mark>.*</mark></span><span class="context-right"> *([^<]*)'
      )

    left <- if (leftContextSize > 0) {
      tail(unlist(str_split(match[1, 3], tokenizeRegex)), leftContextSize)
    } else {
      ""
    }
    #    cat(paste("left:", left, "\n", collapse=" "))

    right <- if (rightContextSize > 0) {
      head(unlist(str_split(match[1, 4], tokenizeRegex)), rightContextSize)
    } else {
      ""
    }
    #    cat(paste("right:", right, "\n", collapse=" "))

    if (is.na(left[1]) || is.na(right[1]) || length(left) + length(right) == 0) {
      oldTable
    } else {
      table(c(left, right)) |>
        dplyr::as_tibble(.name_repair = "minimal") |>
        dplyr::rename(word = 1, frequency = 2) |>
        dplyr::filter(str_detect(word, collocateFilterRegex)) |>
        dplyr::anti_join(stopwordsTable, by = "word") |>
        dplyr::bind_rows(oldTable)
    }
  }
}

#' Preliminary synsemantic stopwords function
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Preliminary synsemantic stopwords function to be used in collocation analysis.
#'
#' @details
#' Currently only suitable for German. See stopwords package for other languages.
#'
#' @param ... future arguments for language detection
#'
#' @family collocation analysis functions
#' @return Vector of synsemantic stopwords.
#' @export
synsemanticStopwords <- function(...) {
  res <- c(
    "der",
    "die",
    "und",
    "in",
    "den",
    "von",
    "mit",
    "das",
    "zu",
    "im",
    "ist",
    "auf",
    "sich",
    "Die",
    "des",
    "dem",
    "nicht",
    "ein",
    "Ein",
    "eine",
    "Eine",
    "es",
    "auch",
    "an",
    "als",
    "am",
    "aus",
    "Der",
    "bei",
    "er",
    "dass",
    "sie",
    "nach",
    "um",
    "Das",
    "zum",
    "noch",
    "war",
    "einen",
    "einer",
    "wie",
    "einem",
    "vor",
    "bis",
    "\u00fcber",
    "so",
    "aber",
    "Eine",
    "diese",
    "Diese",
    "oder"
  )
  return(res)
}


# #' @export
findExample <-
  function(kco,
           query,
           vc = "",
           matchOnly = TRUE) {
    out <- character(length = length(query))

    if (length(vc) < length(query)) {
      vc <- rep(vc, length(query))
    }

    for (i in seq_along(query)) {
      q <- corpusQuery(kco, paste0("(", query[i], ")"), vc = vc[i], metadataOnly = FALSE)
      if (q@totalResults > 0) {
        q <- fetchNext(q, maxFetch = 50, randomizePageOrder = F)
        example <- as.character((q@collectedMatches)$snippet[1])
        out[i] <- if (matchOnly) {
          gsub(".*<mark>(.+)</mark>.*", "\\1", example)
        } else {
          stringr::str_replace(example, "<[^>]*>", "")
        }
      } else {
        out[i] <- ""
      }
    }
    out
  }

collocatesQuery <-
  function(kco,
           query,
           vc = "",
           minOccur = 5,
           leftContextSize = 5,
           rightContextSize = 5,
           searchHitsSampleLimit = 20000,
           ignoreCollocateCase = FALSE,
           stopwords = c(),
           ...) {
    frequency <- NULL
    q <- corpusQuery(kco, query, vc, metadataOnly = F, ...)
    if (q@totalResults == 0) {
      tibble(word = c(), frequency = c())
    } else {
      q <- fetchNext(q, maxFetch = searchHitsSampleLimit, randomizePageOrder = TRUE)
      matches2FreqTable(q@collectedMatches,
        0,
        minOccur = minOccur,
        leftContextSize = leftContextSize,
        rightContextSize = rightContextSize,
        ignoreCollocateCase = ignoreCollocateCase,
        stopwords = stopwords,
        ...,
        verbose = kco@verbose
      ) |>
        mutate(frequency = frequency * q@totalResults / min(q@totalResults, searchHitsSampleLimit)) |>
        filter(frequency >= minOccur)
    }
  }
