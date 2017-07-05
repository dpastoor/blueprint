
params <- function(bp, ...) {
    bp <- bp$clone()
    bp$add_params(...)
    return(bp)
}

constants <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_constants(...)
  return(bp)
}
heirarchy <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_heirarchy(...)
  return(bp)
}
residual_error <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_residual_error(...)
  return(bp)
}
with_data <- function(bp, .data) {
  bp <- bp$clone()
  bp$data <- .data
  return(bp)
}

from_path <- function(bp, .path) {
  bp <- bp$clone()
  bp$datapath <- .path
  return(bp)
}
use_template <- function(bp, .template) {
  bp <- bp$clone()
  bp$template <- .template
  return(bp)
}
render <- function(bp) {
  bp$render()
}
