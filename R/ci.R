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
#' @importFrom dplyr enquo rename starts_with
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
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
  nas <- df %>%
    dplyr::filter(total <= 0) %>%
    mutate(f = NA, conf.low = NA, conf.high = NA)
  df %>%
    dplyr::filter(total > 0) %>%
    rowwise %>%
    mutate(tst = list(
      broom::tidy(prop.test(!!x,!!N, conf.level = conf.level)) %>%
        select(estimate, conf.low, conf.high) %>%
        rename(f = estimate)
    )) %>%
    tidyr::unnest(tst) %>%
    bind_rows(nas)
}

## Mute notes: "Undefined global functions or variables:"
globalVariables(c("totalResults", "total", "estimate", "tst"))


# ci.old <- function(df, x = totalResults, N = total, conf.level = 0.95) {
#   x <- deparse(substitute(x))
#   N <- deparse(substitute(N))
#   df <- data.frame(df)
#   df$f <- df[,x] / df[,N]
#   df[, c("conf.low", "conf.high")] <- t(sapply(Map(function(a, b) prop.test(a, b, conf.level = conf.level), df[,x], df[,N]), "[[","conf.int"))
#   return(df)
# }
