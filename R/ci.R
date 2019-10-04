
#' Add confidence interval and relative frequency variables
#'
#' Using \code{\link{prop.test}}, \code{ci} adds three columns to a data frame:
#' 1. relative frequency (\code{f}) 2. lower bound of a confidence interval
#' (\code{ci.low}) 3. upper bound of a confidence interval
#'
#' @param df table with columns for absolute and total frequencies.
#' @param x  column with the observed absolute frequency.
#' @param N  column with the total frequncies
#' @param conf.level confidence level of the returned confidence interval. Must
#'   be a single number between 0 and 1.
#'
#' @export
#' @importFrom stats prop.test
#' @examples
#' library(ggplot2)
#' kco <- new("KorAPConnection", verbose=TRUE)
#' expand_grid(year=2015:2018, alternatives=c("Hate Speech", "Hatespeech")) %>%
#'   bind_cols(corpusQuery(kco, .$alternatives, sprintf("pubDate in %d", .$year))) %>%
#'   mutate(tokens=corpusStats(kco, vc=vc)$tokens) %>%
#'   ci() %>%
#'   ggplot(aes(x=year, y=f, fill=query, color=query, ymin=conf.low, ymax=conf.high)) +
#'     geom_point() + geom_line() + geom_ribbon(alpha=.3)
#'
ci <- function(df, x = totalResults, N = tokens, conf.level = 0.95) {
  x <- enquo(x)
  N <- enquo(N)
  df %>%
    rowwise %>%
    mutate(tst = list(broom::tidy(prop.test(!!x, !!N, conf.level = conf.level)) %>%
                        select("estimate", starts_with("conf.")) %>%
                        rename(f = estimate)
    )) %>%
    tidyr::unnest(tst)
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


# ci.old <- function(df, x = totalResults, N = tokens, conf.level = 0.95) {
#   x <- deparse(substitute(x))
#   N <- deparse(substitute(N))
#   df <- data.frame(df)
#   df$f <- df[,x] / df[,N]
#   df[, c("conf.low", "conf.high")] <- t(sapply(Map(function(a, b) prop.test(a, b, conf.level = conf.level), df[,x], df[,N]), "[[","conf.int"))
#   return(df)
# }
