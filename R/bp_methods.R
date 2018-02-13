#' generic setting of parameters for blueprints
#' @param .bp blueprint object
#' @param ... parameters to set
#' @param .overwrite whether to overwrite a param if it already exists
#' @rdname parameters
#' @export
parameters <- function(x, ...) {
  UseMethod("parameters", x)
}

#' generic setting of parameters for blueprints
#' @param .bp blueprint object
#' @param ... parameters to set
#' @param .overwrite whether to overwrite a param if it already exists
#' @rdname constants
#' @export
constants <- function(x, ...) {
  UseMethod("constants", x)
}
