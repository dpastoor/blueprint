#' chainable function to access add_params method
#' @export
parameters <- function(bp, ...) {
    bp <- bp$clone()
    bp$add_params(...)
    return(bp)
}

#' chainable method to add_constants
#' @export
constants <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_constants(...)
  return(bp)
}

#' chainable method to add heirarchies
#' @export
heirarchies <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_heirarchies(...)
  return(bp)
}

# chainable method to add_residual_error
#' @export
residual_error <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_residual_error(...)
  return(bp)
}

# chainable method to set the data
#' @export
with_data <- function(bp, .data) {
  bp <- bp$clone()
  bp$data <- .data
  return(bp)
}

#' chainable method to set path to data file
#' @export
from_path <- function(bp, .path) {
  bp <- bp$clone()
  bp$datapath <- .path
  return(bp)
}

#' chainable method to set template
#' @export
use_template <- function(bp, .template) {
  bp <- bp$clone()
  bp$template <- .template
  return(bp)
}

#' chainable method to render
#' @export
render <- function(bp) {
  bp$render()
}
