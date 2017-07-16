#' create a constant parameter
#' @param .value value to hold
#' @param .comment comment
#' @export
const <- function(.value, .comment) {
  return(list(value = .value, comment = .comment))
}

#' Parameter class
#' @export
Parameter <- R6Class("Parameter",
                  public = list(
                  initialize =  function(
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

                  private$name <- name
                  private$value <- value
                  private$comment <- comment
                  private$lower_bound <- lower_bound
                  private$upper_bound <- upper_bound
                  private$fixed <- fixed
                  private$covariate_relationships <- covariate_relationships
                  private$link <- link
                  },

                  ## getters ------
                  get_name = function() {
                    private$name
                  },
                  get_value = function() {
                    private$value
                  },
                  get_comment = function() {
                    private$comment
                  },
                  get_lower_bound = function() {
                    private$lower_bound
                  },
                  get_upper_bound = function() {
                    private$upper_bound
                  },
                  get_fixed = function() {
                    private$fixed
                  },
                  get_covariate_relationships = function() {
                    private$covariate_relationships
                  },
                  get_link = function() {
                    private$link
                  },
                  get_all_information = function() {
                    list(
                      "name" = private$name,
                      "value" = private$value,
                      "comment" = private$comment,
                      "lower_bound" = private$lower_bound,
                      "upper_bound" = private$upper_bound,
                      "fixed" = private$fixed,
                      "covariate_relationships" = private$covariate_relationships,
                      "link" = private$link
                    )
                  },
                  ## setters ------
                  set_name = function(.x, link = FALSE) {
                    private$name <- .x
                    if (link) {
                      private$link <- .x
                    }
                  },
                  set_value = function(.x) {
                    private$value <- .x
                  },
                  set_comment = function(.x) {
                    private$comment <- .x
                  },
                  set_lower_bound = function(.x) {
                    # so can use NULL to wipe out user settings so will go back to -Inf
                    if (is.null(.x)) {
                      .x <- -Inf
                    }
                    bounds_check(private$value, .x, private$upper_bound)
                    private$lower_bound <- .x
                  },
                  set_upper_bound = function(.x) {
                    # so can use NULL to wipe out user settings so will go back to Inf
                    if (is.null(.x)) {
                      .x <- Inf
                    }
                    bounds_check(private$value, private$lower_bound, private$upper_bound)
                    private$upper_bound <- .x
                  },
                  set_fixed = function(.x) {
                    if (!rlang::is_bare_logical(.x, 1)) {
                      stop("fixed must be a boolean value TRUE/FALSE")
                    }
                    private$fixed <- .x
                  },
                  set_covariate_relationships = function(.x) {
                    stop("not actually implemented - need to set and update if exists")
                    private$covariate_relationships <- .x
                  },
                  set_link = function(.x) {
                    private$link <- .x
                  }
                  ),
                 private = list(
                  name = NULL,
                  value = NULL,
                  comment = NULL,
                  lower_bound = NULL,
                  upper_bound = NULL,
                  fixed = NULL,
                  covariate_relationships = NULL,
                  link = NULL
                 )
)

bounds_check <- function(.value, .lb, .ub) {
    if (.ub < .lb) {
      stop("upper bound must be greater than lower bound")
    }
    if (!dplyr::between(.value, .lb, .ub)) {
      stop(glue::glue("bounds must enclose value {.value}"))
    }
  return(invisible())
}
