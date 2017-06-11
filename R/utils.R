# capture named dots as a named list
dots <- function (...) {
  list(...)
}

named_indices <- function(.x) {
  return(which(.x != ""))
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
