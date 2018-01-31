#' generic setting of parameters for blueprints
#' @param .bp blueprint object
#' @param ... parameters to set
#' @param .overwrite whether to overwrite a param if it already exists
#' @rdname export
#' @export
parameters.blueprint <- function(.bp, ..., .overwrite = TRUE) {
  param_list <- dots(...)
  param_names <- names(param_list)
  # clear out any null parameters
  null_indices <- purrr::map_lgl(param_list, is.null)
  to_remove <- param_names[null_indices]
  purrr::walk(to_remove, function(.x) {
    .bp$parameters[[.x]] <- NULL
  })
  param_list <- param_list[!null_indices]
  param_names <- param_names[!null_indices]
  if (!length(param_list)) {
    return(invisible())
  }
  constructed_params <- map2(param_list, param_names, function(param_info, .pn) {
    # if numeric assume shorthand value only
    # CL = 4.5
    if (is_bare_numeric(param_info)) {
      return(parameter(param_info, name = .pn))
    }
    if (!inherits(param_info, "parameter")) {
      stop(sprintf("incorrect specification for %s,
                   please construct a parameter specification with `parameter()`", .pn))
    }

    # for now will force a name for all parameters, through the add_param
    # call, eg add_param(<name> = parameter()), so even if a name
    # is set, it will override.
    param_info <- update(param_info, name = .pn)
    # if link null, set equal to name
    if (is.null(link(param_info))) {
      param_info <- update(param_info, link = name(param_info))
    }
    return(param_info)
    })
  # in case anything was just given as a parameter block
  constructed_param_names <- purrr::map(constructed_params, ~ name(.x))
  final_parameters <- modifyList(.bp$parameters,
                                 set_names(constructed_params, constructed_param_names)) %>%
    discard(is.null)
  # if get a case where everything is overwritten set to empty list
  if (!length(final_parameters)) {
    # don't want a named list just a bare empty list
    final_parameters <- list()
  }
  .bp$parameters <- final_parameters
  return(.bp)
}

#' generic setting of parameters for blueprints
#' @export
#' @rdname parameters
parameters.nonmem_blueprint <- function(.bp, ...) {
  NextMethod(.bp, ...)
}
