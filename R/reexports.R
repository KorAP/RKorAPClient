
#' Pipe operator
#'
#' See `magrittr::[\%>\%][magrittr::pipe]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
#' @importFrom dplyr bind_cols
#' @export
dplyr::bind_cols
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
#' @importFrom dplyr n
#' @export
dplyr::n
#' @importFrom tidyr complete
#' @export
tidyr::complete
#' @importFrom tidyr expand_grid
#' @export
tidyr::expand_grid
#' @importFrom lubridate year
#' @export
lubridate::year
#' @importFrom tibble as_tibble rownames_to_column
#' @export
tibble::as_tibble
tibble::rownames_to_column
#' @importFrom broom tidy
#' @export
broom::tidy
#' @importFrom ggplot2 ggplot geom_bar geom_line
#' @export
