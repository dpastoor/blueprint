
bounds_check <- function(value, .lb, .ub) {
    if (.ub < .lb) {
      stop("upper bound must be greater than lower bound")
    }
    if (!dplyr::between(value, .lb, .ub)) {
      stop(glue::glue("bounds must enclose value {.value}"))
    }
  return(TRUE)
}

#' create a parameter
#'
#' @param value value of parameter
#' @param name name of parameter, Default: NULL
#' @param comment comments about the parameter, Default: NULL
#' @param lower_bound lower bound, Default: -Inf
#' @param upper_bound upper bound, Default: Inf
#' @param fixed whether parameter should be fixed, Default: FALSE
#' @param covariate_relationships covariate relationships, Default: NULL
#' @param link name to link to a hierarchy, Default: name
#' @return object with class parameter
#' @examples
#' cl <- parameter(1.5, "CL")
#' V <- parameter(10.5, "TVV", lower_bound = 0, link = "V")
#' @rdname parameter
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

#' update a parameter
#' @rdname update
#' @method update parameter
#' @importFrom utils modifyList
#' @export
update.parameter <- function(x, ...) {
  update_elements <- dots(...)
  not_present <- setdiff(names(update_elements), names(x))
  if (length(not_present)) {
    stop(glue::glue("the following update elements are invalid:
                    {paste0(not_present, collapse = ', ')} "))
  }
  modifyList(x, update_elements)
}

#' get the value for a parameter
#' @rdname value
#' @method value parameter
#' @export
value.parameter <- function (x, ...) {
  x$value
}

#' get the lower bound for a parameter
#' @rdname lower_bound
#' @method lower_bound parameter
#' @export
lower_bound.parameter <- function (x, ...) {
  x$lower_bound
}

#' get the upper bound for a parameter
#' @rdname upper_bound
#' @method upper_bound parameter
#' @export
upper_bound.parameter <- function (x, ...) {
  x$upper_bound
}

#' get the link for a parameter
#' @rdname link
#' @method link parameter
#' @export
link.parameter <- function (x, ...) {
  x$link
}

#' get the name of the parameter
#' @rdname name
#' @method name parameter
#' @export
name.parameter <- function (x, ...) {
  x$name
}

#' get whether a parameter is fixed
#' @rdname fixed
#' @method fixed parameter
#' @export
fixed.parameter <- function (x, ...) {
  x$fixed
}

#' get covariate relationships for a parameter
#' @rdname covariate_relationships
#' @method covariate_relationships parameter
#' @export
covariate_relationships.parameter <- function (x, ...) {
  x$covariate_relationships
}
