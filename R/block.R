#' block matrix
#' @param ... elements to coerce into a block matrix
#' @param fix whether the values should be fixed
#' @param param_names parameters associated with the elements of the matrix
#' @param correlation whether to create as a correlation matrix
#' @param comment comment to append
#' @examples
#' block(0.04, 0.01, 0.09, param_names = c("CL", "V"))
#' # can also set as correlation matrix
#' block(0.04, 0.61, 0.09, param_names = c("CL", "V"), correlation = TRUE)
#' @export
block <- function(..., param_names, fix = FALSE, correlation = FALSE, comment = NULL) {
  output_matrix <- mrgsolve::bmat(..., correlation = correlation)
  output <- list(block = TRUE,
              params = param_names,
              fix = fix,
              correlation = correlation,
              matrix = output_matrix,
              value = stringify_matrix(output_matrix, fix),
              num_params = length(param_names),
              comment = comment
              )
  class(output) <- c("omega", "block")
  return(output)
}


#' diagonal values
#' @param value value
#' @param link link parameter name
#' @param fix whether fixed DEFAULT: FALSE
#' @param comment comment
#' @export
omega_param <- function(value, link, fix = FALSE, comment = NULL) {
  # note omega param takes param and value singular, vs the block
  output <- list(block = FALSE,
       link = link,
       fix = fix,
       correlation = FALSE,
       value = value,
       comment = comment)
  class(output) <- "omega"
  return(output)

}
#' diagonal values
#' @param value value
#' @param fix whether fixed DEFAULT: FALSE
#' @param comment comment
#' @export
sigma_param <- function(value, fix = FALSE, comment = NULL) {
  # note omega param takes param and value singular, vs the block
  return(list(type = "diagonal",
              fix = fix,
              correlation = FALSE,
              value = value,
              comment = comment))

}
