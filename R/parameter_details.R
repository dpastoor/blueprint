#' create details about a parameter
#' @param .value initial value of a parameter
#' @param .bounds a vector of lower and upper bounds
#' @param .fixed whether the parameter should be fixed
#' @param covariate_relationships covariate relationships to a parameter
#' @export
param <- function(.value,
                .bounds = NULL,
                .fixed = FALSE,
                covariate_relationships = NULL) {
  output <- list(
    value = .value,
    bounds = .bounds,
    fixed = .fixed,
    covariate_relationships = covariate_relationships
  )
  if (!is.null(.bounds)) {
    if (
      length(.bounds) != 2 ||
      .bounds[1] > .bounds[2] ||
      !dplyr::between(.value, .bounds[1], .bounds[2])
    ) {
      stop("bounds must enclose value, and should be a vector, eg c(0, Inf)")
    }
    output$bounds <- .bounds

  }
  return(output)
}
