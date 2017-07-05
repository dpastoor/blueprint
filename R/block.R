#' block matrix
#' @param ... elements to coerce into a block matrix
#' @param .fix whether the values should be fixed
#' @param .params parameters associated with the elements of the matrix
#' @param correlation whether to create as a correlation matrix
#' @param .comment comment to append
#' @examples
#' block(0.04, 0.01, 0.09, .params = vars(CL, V))
#' # can also set as correlation matrix
#' block(0.04, 0.61, 0.09, .params = c("CL", "V"), correlation = TRUE)
#' @export
block <- function(..., .params, .fix = FALSE, correlation = FALSE, .comment = NULL) {
  output_matrix <- mrgsolve::bmat(..., correlation = correlation)
  return(list(block = TRUE,
              params = .params,
              fix = .fix,
              correlation = correlation,
              matrix = output_matrix,
              value = stringify_matrix(output_matrix, .fix),
              nomega = length(.params),
              comment = .comment
              )
         )
}

#' diagonal values
#' @param .value value
#' @param .fix whether fixed DEFAULT: FALSE
#' @param .comment comment
#' @export
omega_param <- function(.value, .fix = FALSE, .comment = NULL) {
  # note omega param takes param and value singular, vs the block
  return(list(block = FALSE,
              fix = .fix,
              correlation = FALSE,
              value = .value,
              comment = .comment))

}
#' diagonal values
#' @param .value value
#' @param .fix whether fixed DEFAULT: FALSE
#' @param .comment comment
#' @export
sigma_param <- function(.value, .fix = FALSE, .comment = NULL) {
  # note omega param takes param and value singular, vs the block
  return(list(type = "diagonal",
              fix = .fix,
              correlation = FALSE,
              value = .value,
              comment = .comment))

}
