#' chainable function to access add_accepts method
#' @param bp blueprint object
#' @param x vector of accept strings
#' @param overwrite whether to overwrite existing accept strings, default FALSE
#' @examples
#' bp <- Blueprint$new("nonmem")
#' bp %>%
#' accept(c("TIME.LT.100", "DV.GT.5"))
#' @export
accept <- function(bp, x, overwrite = FALSE) {
  bp <- bp$clone()
  bp$add_accepts(x, .overwrite = overwrite)
  return(bp)
}

#' chainable function to access add_ignores method
#' @param bp blueprint object
#' @param x vector of ignore strings
#' @param overwrite whether to overwrite existing ignore strings, default FALSE
#' @examples
#' bp <- Blueprint$new("nonmem")
#' bp %>%
#' ignore(c("TIME.GT.100", "DV.LT.1"))
#' @export
ignore <- function(bp, x, overwrite = FALSE) {
  bp <- bp$clone()
  bp$add_ignores(x, .overwrite = overwrite)
  return(bp)
}

#' chainable function to access add_params method
#'
#' @param bp blueprint object
#' @param ... params to pass
#' @export
parameters <- function(bp, ...) {
    bp <- bp$clone()
    bp$add_params(...)
    return(bp)
}

#' chainable method to add_constants
#'
#' @param bp blueprint object
#' @param ... params to pass
#' @export
constants <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_constants(...)
  return(bp)
}

#' chainable method to add hierarchies
#'
#' @param bp blueprint object
#' @param ... params to pass
#' @export
hierarchies <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_hierarchies(...)
  return(bp)
}

#' chainable method to add hooks
#'
#' @param bp blueprint object
#' @param ... params to pass
#' @details
#' hooks can be added into any template, and should
#' follow the naming convention hooks:<major_element>:<location>
#'
#' For example, {{hooks:pk:begin}} might be
#' placed at the top of $PK in a template
#'
#' the  [hooks()] function will inject a string into that template
#' by matching the name.
#' @examples
#' library(dplyr)
#' bp <- Blueprint$new("nonmem")
#'
#' bp$template <- "
#' $PK
#' {{hooks:pk:begin}}
#'
#' ;; stuff in $PK
#'
#' {{hooks:pk:end}}
#' "
#' bp %>%
#' hooks("pk:begin" = ";; comment at top of PK",
#'       "pk:end" = "V2 = S2/1000") %>%
#' render() %>%
#' # to print lines nicely
#' cat()
#' @export
hooks <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_hooks(...)
  return(bp)
}

#' chainable method to add_residual_error
#'
#' @param bp blueprint object
#' @param ... params to pass
#' @export
residual_error <- function(bp, ...) {
  bp <- bp$clone()
  bp$add_residual_error(...)
  return(bp)
}

#' chainable method to set the data
#'
#' @param bp blueprint object
#' @param data dataset
#' @export
with_data <- function(bp, data) {
  bp <- bp$clone()
  bp$data <- data
  return(bp)
}

#' chainable method to set path to data file
#'
#' @param bp blueprint object
#' @param path path for data file for a control stream
#' @export
from_path <- function(bp, path) {
  bp <- bp$clone()
  bp$datapath <- path
  return(bp)
}

#' chainable method to set template
#'
#' @param bp blueprint object
#' @param template template to use
#' @export
use_template <- function(bp, template) {
  bp <- bp$clone()
  bp$template <- template
  return(bp)
}

#' chainable method to specify the model type
#'
#' @param bp blueprint object
#' @param model_type type of model
#' @examples
#' bp <- Blueprint$new("nonmem")
#' models <- available_models("nonmem")
#' bp %>% model_type(models$one_cmt_iv)
#' @export
model_type <- function(bp, model_type) {
  bp <- bp$clone()
  bp$routine <- model_type
  return(bp)
}

#' chainable method to render
#'
#' @param bp blueprint object
#' @export
render <- function(bp) {
  bp$render()
}
