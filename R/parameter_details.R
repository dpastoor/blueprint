#' create details about a parameter
#' @param .value initial value of a parameter
#' @param .lower_bound a lower bound
#' @param .upper_bound an upper bound
#' @param .fixed whether the parameter should be fixed
#' @param covariate_relationships covariate relationships to a parameter
#' @export
param <- function(.value,
                  .comment = NULL,
                  .lower_bound = NULL,
                  .upper_bound = NULL,
                  .fixed = FALSE,
                  covariate_relationships = NULL) {
  output <- list(
    value = .value,
    comment = .comment,
    lower_bound = .lower_bound,
    upper_bound = .upper_bound,
    fixed = .fixed,
    covariate_relationships = covariate_relationships
  )
  if (!is.null(.lower_bound) || !is.null(.upper_bound)) {
    lb <- ifelse(is.null(.lower_bound), -Inf, .lower_bound)
    ub <- ifelse(is.null(.upper_bound), Inf, .upper_bound)
    if (ub < lb) {
      stop("upper bound must be greater than lower bound")
    }
    if (
      !dplyr::between(.value, lb, ub)
    ) {
      stop("bounds must enclose value")
    }
  }
  return(output)
}

#' create a constant parameter
#' @param .value value to hold
#' @param .comment comment
#' @export
const <- function(.value, .comment) {
  return(list(value = .value, comment = .comment))
}
