#' block matrix
#' @param ... elements to coerce into a block matrix
#' @param .fix whether the values should be fixed
#' @param .params parameters associated with the elements of the matrix
#' @example
#' block(0.04, 0.01, 0.09, .params = vars(CL, V))
#' # can also set as correlation matrix
#' block(0.04, 0.61, 0.09, correlation = TRUE, .params = vars(CL, V))
block <- function(..., .fix = FALSE, .params) {
  output_matrix <- mrgsolve::bmat(...)
  return(output_matrix)
}
