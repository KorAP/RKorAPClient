#' Add confidence interval and relative frequency variables
#'
#' Using [prop.test()], `ci` adds three columns to a data frame:
#' 1. relative frequency (`f`)
#' 2. lower bound of a confidence interval (`ci.low`)
#' 3. upper bound of a confidence interval
#'
#'
#' @seealso
#' `ci` is already included in [frequencyQuery()]
#'
#' @param df table with columns for absolute and total frequencies.
#' @param x  column with the observed absolute frequency.
#' @param N  column with the total frequencies
#' @param conf.level confidence level of the returned confidence interval. Must
#'   be a single number between 0 and 1.
#'
#' @rdname misc-functions
#'
#' @export
#' @importFrom stats prop.test
#' @importFrom tibble remove_rownames
#' @importFrom dplyr enquo rename starts_with filter mutate rowwise bind_rows select arrange row_number quo_name
#' @importFrom broom tidy
#' @importFrom tidyr unnest
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#' kco <- KorAPConnection(verbose=TRUE)
#' expand_grid(year=2015:2018, alternatives=c("Hate Speech", "Hatespeech")) %>%
#'   bind_cols(corpusQuery(kco, .$alternatives, sprintf("pubDate in %d", .$year))) %>%
#'   mutate(total=corpusStats(kco, vc=vc)$tokens) %>%
#'   ci() %>%
#'   ggplot(aes(x=year, y=f, fill=query, color=query, ymin=conf.low, ymax=conf.high)) +
#'     geom_point() + geom_line() + geom_ribbon(alpha=.3)
#' }
ci <- function(df,
               x = totalResults,
               N = total,
               conf.level = 0.95) {
  x <- enquo(x)
  N <- enquo(N)
  
  # Ensure df is ungrouped for compatibility with grouped data
  df <- df |> ungroup()
  
  # Add row index to preserve original order
  df <- df |> mutate(.row_index = row_number())
  
  # Initialize result with all NA values
  result <- df %>%
    mutate(f = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  
  # Calculate confidence intervals for valid rows
  # Use the column names from the enquoted expressions
  N_col <- quo_name(N)
  x_col <- quo_name(x)
  valid_indices <- which(df[[N_col]] > 0 & !is.na(df[[N_col]]) & !is.na(df[[x_col]]))
  
  if (length(valid_indices) > 0) {
    valid_data <- df[valid_indices, ]
    
    ci_results <- valid_data %>%
      rowwise %>%
      mutate(tst = list(
        broom::tidy(prop.test(!!x, !!N, conf.level = conf.level)) %>%
          select(estimate, conf.low, conf.high) %>%
          rename(f = estimate)
      )) %>%
      tidyr::unnest(tst) %>%
      select(.row_index, f, conf.low, conf.high)
    
    # Update result with calculated values
    for (i in seq_len(nrow(ci_results))) {
      row_idx <- ci_results$.row_index[i]
      result$f[row_idx] <- ci_results$f[i]
      result$conf.low[row_idx] <- ci_results$conf.low[i]
      result$conf.high[row_idx] <- ci_results$conf.high[i]
    }
  }
  
  # Remove the helper column
  result %>% select(-.row_index)
}

## Mute notes: "Undefined global functions or variables:"
globalVariables(c("totalResults", "total", "estimate", "tst", ".row_index", "f", "conf.low", "conf.high", "N_col", "x_col"))


# ci.old <- function(df, x = totalResults, N = total, conf.level = 0.95) {
#   x <- deparse(substitute(x))
#   N <- deparse(substitute(N))
#   df <- data.frame(df)
#   df$f <- df[,x] / df[,N]
#   df[, c("conf.low", "conf.high")] <- t(sapply(Map(function(a, b) prop.test(a, b, conf.level = conf.level), df[,x], df[,N]), "[[","conf.int"))
#   return(df)
# }
