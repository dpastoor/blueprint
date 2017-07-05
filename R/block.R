#' block matrix
#' @param ... elements to coerce into a block matrix
#' @param .fix whether the values should be fixed
#' @param .params parameters associated with the elements of the matrix
#' @examples
#' block(0.04, 0.01, 0.09, .params = vars(CL, V))
#' # can also set as correlation matrix
#' block(0.04, 0.61, 0.09, .params = c("CL", "V"), correlation = TRUE)
block <- function(..., .params = NULL, .fix = FALSE, correlation = FALSE) {
  output_matrix <- mrgsolve::bmat(..., correlation = correlation)
  return(list(type = "block",
              params = .params,
              correlation = correlation,
              values = output_matrix)
         )
}
