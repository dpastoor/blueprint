# capture named dots as a named list
dots <- function (...) {
  list(...)
}

named_indices <- function(.x) {
  return(which(.x != ""))
}

# for templates, so that will collapse organization, unless it was a completely empty line, in which
# it will retain the break
collapse <- function(.x) {
  splits <- split(.x, cumsum(.x == ""))
  paste0(purrr::map(splits, paste0, collapse = ""), collapse = "\n")
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
