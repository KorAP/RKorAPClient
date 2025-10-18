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
    word <- frequency <- O <- NULL

    if (!exactFrequencies && (!is.na(withinSpan) && !is.null(withinSpan) && nzchar(withinSpan))) {
      stop(sprintf("Not empty withinSpan (='%s') requires exactFrequencies=TRUE", withinSpan), call. = FALSE)
    }

    warnIfNotAuthorized(kco)

    if (lemmatizeNodeQuery) {
      node <- lemmatizeWordQuery(node)
    }

    vcNames <- names(vc)
    if (is.null(vcNames)) {
      vcNames <- rep(NA_character_, length(vc))
    }

    label_lookup <- NULL
    if (!is.null(names(vc)) && length(vc) > 0) {
      raw_names <- names(vc)
      if (any(!is.na(raw_names) & raw_names != "")) {
        label_lookup <- stats::setNames(raw_names, vc)
      }
    }

    result <- if (length(node) > 1 || length(vc) > 1) {
      grid <- if (expand) {
        tmp_grid <- tidyr::expand_grid(node = node, idx = seq_along(vc))
        tmp_grid$vc <- vc[tmp_grid$idx]
        tmp_grid$vcLabel <- vcNames[tmp_grid$idx]
        tmp_grid[, c("node", "vc", "vcLabel"), drop = FALSE]
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
          collocateFilterRegex = collocateFilterRegex,
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
        collocateFilterRegex = collocateFilterRegex,
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

    threshold_col <- thresholdScore
    if (maxRecurse > 0 && nrow(result) > 0 && threshold_col %in% names(result)) {
      threshold_values <- result[[threshold_col]]
      eligible_idx <- which(!is.na(threshold_values) & threshold_values >= threshold)
      if (length(eligible_idx) > 0) {
        recurseWith <- result[eligible_idx, , drop = FALSE]
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
          collocateFilterRegex = collocateFilterRegex,
          vcLabel = vcLabel
        ) |>
          bind_rows(result) |>
          filter(logDice >= 2) |>
          filter(O >= minOccur) |>
          dplyr::arrange(dplyr::desc(logDice))
      }
    }

    if (addExamples && nrow(result) > 0) {
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
  label <- node <- collocate <- NULL

  if (!"label" %in% names(result) || dplyr::n_distinct(result$label) < 2) {
    return(result)
  }

  numeric_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  non_score_cols <- c("N", "O", "O1", "O2", "E", "w", "leftContextSize", "rightContextSize", "frequency")
  score_cols <- setdiff(numeric_cols, non_score_cols)

  if (length(score_cols) == 0) {
    return(result)
  }

  comparison <- result |>
    dplyr::select(node, collocate, label, dplyr::all_of(score_cols)) |>
    tidyr::pivot_wider(
      names_from = label,
      values_from = dplyr::all_of(score_cols),
      names_glue = "{.value}_{make.names(label)}",
      values_fn = dplyr::first
    )

  raw_labels <- unique(result$label)
  labels <- make.names(raw_labels)
  label_map <- stats::setNames(raw_labels, labels)

  rank_data <- result |>
    dplyr::distinct(node, collocate)

  for (i in seq_along(raw_labels)) {
    raw_lab <- raw_labels[i]
    safe_lab <- labels[i]
    label_df <- result[result$label == raw_lab, c("node", "collocate", score_cols), drop = FALSE]
    if (nrow(label_df) == 0) {
      next
    }
    label_df <- dplyr::distinct(label_df)
    rank_tbl <- label_df[, c("node", "collocate"), drop = FALSE]
    for (col in score_cols) {
      rank_col_name <- paste0("rank_", safe_lab, "_", col)
      values <- label_df[[col]]
      ranks <- rep(NA_real_, length(values))
      valid_idx <- which(!is.na(values))
      if (length(valid_idx) > 0) {
        ranks[valid_idx] <- rank(-values[valid_idx], ties.method = "first")
      }
      rank_tbl[[rank_col_name]] <- ranks
    }
    rank_data <- dplyr::left_join(rank_data, rank_tbl, by = c("node", "collocate"))
  }

  comparison <- dplyr::left_join(comparison, rank_data, by = c("node", "collocate"))

  rank_replacements <- numeric(0)
  rank_column_names <- grep("^rank_", names(comparison), value = TRUE)
  if (length(rank_column_names) > 0) {
    rank_replacements <- stats::setNames(
      vapply(rank_column_names, function(col) {
        col_values <- comparison[[col]]
        valid_values <- col_values[!is.na(col_values)]
        if (length(valid_values) == 0) {
          nrow(comparison) + 1
        } else {
          suppressWarnings(max(valid_values, na.rm = TRUE)) + 1
        }
      }, numeric(1)),
      rank_column_names
    )
  }

  collapse_label_values <- function(indices, safe_labels_vec) {
    if (length(indices) == 0) {
      return(NA_character_)
    }
    labs <- label_map[safe_labels_vec[indices]]
    fallback <- safe_labels_vec[indices]
    labs[is.na(labs) | labs == ""] <- fallback[is.na(labs) | labs == ""]
    labs <- labs[!is.na(labs) & labs != ""]
    if (length(labs) == 0) {
      return(NA_character_)
    }
    paste(unique(labs), collapse = ", ")
  }

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

    fill_ranks <- function(x, y, left_rank_col, right_rank_col) {
      fallback <- nrow(comparison) + 1
      replacement_left <- rank_replacements[[left_rank_col]]
      if (is.null(replacement_left) || !is.finite(replacement_left)) {
        replacement_left <- fallback
      }
      replacement_right <- rank_replacements[[right_rank_col]]
      if (is.null(replacement_right) || !is.finite(replacement_right)) {
        replacement_right <- fallback
      }
      if (any(is.na(x))) {
        x[is.na(x)] <- replacement_left
      }
      if (any(is.na(y))) {
        y[is.na(y)] <- replacement_right
      }
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
      comparison[[left_col]] <- filled$x
      comparison[[right_col]] <- filled$y
      comparison[[paste0("delta_", col)]] <- filled$x - filled$y
      rank_left <- paste0("rank_", left_label, "_", col)
      rank_right <- paste0("rank_", right_label, "_", col)
      if (all(c(rank_left, rank_right) %in% names(comparison))) {
        filled_rank <- fill_ranks(
          comparison[[rank_left]],
          comparison[[rank_right]],
          rank_left,
          rank_right
        )
        comparison[[paste0("delta_rank_", col)]] <- filled_rank$x - filled_rank$y
      }
    }
  }

  for (col in score_cols) {
    value_cols <- paste0(col, "_", labels)
    existing <- value_cols %in% names(comparison)
    if (!any(existing)) {
      next
    }
    value_cols <- value_cols[existing]
    safe_labels <- labels[existing]

    score_values <- comparison[, value_cols, drop = FALSE]

    winner_label_col <- paste0("winner_", col)
    winner_value_col <- paste0("winner_", col, "_value")
    runner_label_col <- paste0("runner_up_", col)
    runner_value_col <- paste0("runner_up_", col, "_value")
    loser_label_col <- paste0("loser_", col)
    loser_value_col <- paste0("loser_", col, "_value")
    max_delta_col <- paste0("max_delta_", col)

    if (nrow(score_values) == 0) {
      comparison[[winner_label_col]] <- character(0)
      comparison[[winner_value_col]] <- numeric(0)
      comparison[[runner_label_col]] <- character(0)
      comparison[[runner_value_col]] <- numeric(0)
      comparison[[loser_label_col]] <- character(0)
      comparison[[loser_value_col]] <- numeric(0)
      comparison[[max_delta_col]] <- numeric(0)
      next
    }

    score_matrix <- as.matrix(score_values)
    storage.mode(score_matrix) <- "numeric"

    n_rows <- nrow(score_matrix)
    winner_labels <- rep(NA_character_, n_rows)
    winner_values <- rep(NA_real_, n_rows)
    runner_labels <- rep(NA_character_, n_rows)
    runner_values <- rep(NA_real_, n_rows)
    loser_labels <- rep(NA_character_, n_rows)
    loser_values <- rep(NA_real_, n_rows)
    max_deltas <- rep(NA_real_, n_rows)

    if (n_rows > 0) {
      for (i in seq_len(n_rows)) {
        numeric_row <- as.numeric(score_matrix[i, ])
        if (all(is.na(numeric_row))) {
          next
        }

        min_val <- suppressWarnings(min(numeric_row, na.rm = TRUE))
        if (!is.finite(min_val)) {
          min_val <- 0
        }
        numeric_row[is.na(numeric_row)] <- missingScoreFactor * min_val
        score_matrix[i, ] <- numeric_row

        max_val <- suppressWarnings(max(numeric_row, na.rm = TRUE))
        max_idx <- which(numeric_row == max_val)
        winner_labels[i] <- collapse_label_values(max_idx, safe_labels)
        winner_values[i] <- max_val

        unique_vals <- sort(unique(numeric_row), decreasing = TRUE)
        if (length(unique_vals) >= 2) {
          runner_val <- unique_vals[2]
          runner_idx <- which(numeric_row == runner_val)
          runner_labels[i] <- collapse_label_values(runner_idx, safe_labels)
          runner_values[i] <- runner_val
        }

        min_val <- suppressWarnings(min(numeric_row, na.rm = TRUE))
        min_idx <- which(numeric_row == min_val)
        loser_labels[i] <- collapse_label_values(min_idx, safe_labels)
        loser_values[i] <- min_val

        if (is.finite(max_val) && is.finite(min_val)) {
          max_deltas[i] <- max_val - min_val
        }
      }
    }

    comparison[, value_cols] <- score_matrix
    comparison[[winner_label_col]] <- winner_labels
    comparison[[winner_value_col]] <- winner_values
    comparison[[runner_label_col]] <- runner_labels
    comparison[[runner_value_col]] <- runner_values
    comparison[[loser_label_col]] <- loser_labels
    comparison[[loser_value_col]] <- loser_values
    comparison[[max_delta_col]] <- max_deltas
  }

  for (col in score_cols) {
    rank_cols <- paste0("rank_", labels, "_", col)
    existing <- rank_cols %in% names(comparison)
    if (!any(existing)) {
      next
    }
    rank_cols <- rank_cols[existing]
    safe_labels <- labels[existing]
    rank_values <- comparison[, rank_cols, drop = FALSE]

    winner_rank_label_col <- paste0("winner_rank_", col)
    winner_rank_value_col <- paste0("winner_rank_", col, "_value")
    runner_rank_label_col <- paste0("runner_up_rank_", col)
    runner_rank_value_col <- paste0("runner_up_rank_", col, "_value")
    loser_rank_label_col <- paste0("loser_rank_", col)
    loser_rank_value_col <- paste0("loser_rank_", col, "_value")
    max_delta_rank_col <- paste0("max_delta_rank_", col)

    if (nrow(rank_values) == 0) {
      comparison[[winner_rank_label_col]] <- character(0)
      comparison[[winner_rank_value_col]] <- numeric(0)
      comparison[[runner_rank_label_col]] <- character(0)
      comparison[[runner_rank_value_col]] <- numeric(0)
      comparison[[loser_rank_label_col]] <- character(0)
      comparison[[loser_rank_value_col]] <- numeric(0)
      comparison[[max_delta_rank_col]] <- numeric(0)
      next
    }

  rank_matrix <- as.matrix(rank_values)
  storage.mode(rank_matrix) <- "numeric"

    n_rows <- nrow(rank_matrix)
    winner_labels <- rep(NA_character_, n_rows)
    winner_values <- rep(NA_real_, n_rows)
    runner_labels <- rep(NA_character_, n_rows)
    runner_values <- rep(NA_real_, n_rows)
    loser_labels <- rep(NA_character_, n_rows)
    loser_values <- rep(NA_real_, n_rows)
    max_deltas <- rep(NA_real_, n_rows)

    for (i in seq_len(n_rows)) {
      numeric_row <- as.numeric(rank_matrix[i, ])
      if (all(is.na(numeric_row))) {
        next
      }

      if (length(rank_cols) > 0) {
        replacement_vec <- rank_replacements[rank_cols]
        replacement_vec[is.na(replacement_vec)] <- nrow(comparison) + 1
        missing_idx <- which(is.na(numeric_row))
        if (length(missing_idx) > 0) {
          numeric_row[missing_idx] <- replacement_vec[missing_idx]
        }
      }

      valid_idx <- seq_along(numeric_row)
      valid_values <- numeric_row[valid_idx]
      min_val <- suppressWarnings(min(valid_values, na.rm = TRUE))
      min_positions <- valid_idx[which(valid_values == min_val)]
      winner_labels[i] <- collapse_label_values(min_positions, safe_labels)
      winner_values[i] <- min_val

      ordered_vals <- sort(unique(valid_values), decreasing = FALSE)
      if (length(ordered_vals) >= 2) {
        runner_val <- ordered_vals[2]
        runner_positions <- valid_idx[which(valid_values == runner_val)]
        runner_labels[i] <- collapse_label_values(runner_positions, safe_labels)
        runner_values[i] <- runner_val
      }

      max_val <- suppressWarnings(max(valid_values, na.rm = TRUE))
      max_positions <- valid_idx[which(valid_values == max_val)]
      loser_labels[i] <- collapse_label_values(max_positions, safe_labels)
      loser_values[i] <- max_val

      if (is.finite(max_val) && is.finite(min_val)) {
        max_deltas[i] <- max_val - min_val
      }
    }

    comparison[[winner_rank_label_col]] <- winner_labels
    comparison[[winner_rank_value_col]] <- winner_values
    comparison[[runner_rank_label_col]] <- runner_labels
    comparison[[runner_rank_value_col]] <- runner_values
    comparison[[loser_rank_label_col]] <- loser_labels
    comparison[[loser_rank_value_col]] <- loser_values
    comparison[[max_delta_rank_col]] <- max_deltas
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
           collocateFilterRegex = "^[:alnum:]+-?[:alnum:]*$",
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
        collocateFilterRegex = collocateFilterRegex,
        ...,
        verbose = kco@verbose
      ) |>
        mutate(frequency = frequency * q@totalResults / min(q@totalResults, searchHitsSampleLimit)) |>
        filter(frequency >= minOccur)
    }
  }
