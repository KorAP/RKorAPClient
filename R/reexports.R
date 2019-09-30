
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
#' @importFrom dplyr mutate
#' @export
dplyr::mutate
#' @importFrom dplyr select
#' @export
dplyr::select
#' @importFrom dplyr group_by
#' @export
dplyr::group_by
#' @importFrom dplyr summarise
#' @export
dplyr::summarise
#' @importFrom tidyr complete
#' @export
tidyr::complete
