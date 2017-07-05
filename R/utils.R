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

#' strip names from a list
#' @param .x list
#' @export
strip_names <- function(.x) {
  names(.x) <- NULL
  return(.x)
}

#' detect if a filepath is for a directory
#' @param x vector of file paths
#' @export
is_dir <- function(x) {
  isTRUE(file.info(x)$isdir)
}
