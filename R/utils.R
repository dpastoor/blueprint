# capture named dots as a named list
dots <- function (...)
{
  rlang::eval_bare(substitute(alist(...)))
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
