
bounds_check <- function(value, .lb, .ub) {
    if (.ub < .lb) {
      stop("upper bound must be greater than lower bound")
    }
    if (!dplyr::between(value, .lb, .ub)) {
      stop(glue::glue("bounds must enclose value {.value}"))
    }
  return(invisible())
}

#' @export
parameter <- function(
                  value,
                  name = NULL,
                  comment = NULL,
                  lower_bound = -Inf,
                  upper_bound = Inf,
                  fixed = FALSE,
                  covariate_relationships = NULL,
                  link = name
                  ) {

    bounds_check(value, lower_bound, upper_bound)

    initialized <- list(
      name = name,
      value = value,
      comment = comment,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      fixed = fixed,
      covariate_relationships = covariate_relationships,
      link = link
    )
    class(initialized) <- "parameter"
    return(initialized)
}


value.parameter <- function(x) {
  x$value
}

update.parameter <- function(x, ...) {
  update_elements <- dots(...)
  not_present <- setdiff(names(update_elements), names(x))
  if (length(not_present)) {
    stop(glue::glue("the following update elements are invalid:
                    {paste0(not_present, collapse = ', ')} "))
  }
  modifyList(x, update_elements)
}


value.parameter <- function (x) {
  x$value
}


lower_bound.parameter <- function (x) {
  x$lower_bound
}


upper_bound.parameter <- function (x) {
  x$upper_bound
}


link.parameter <- function (x) {
  x$link
}


name.parameter <- function (x) {
  x$name
}

fixed.parameter <- function (x) {
  x$fixed
}

covariate_relationships.parameter <- function (x) {
  x$covariate_relationships
}
