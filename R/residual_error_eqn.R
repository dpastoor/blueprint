#' get the residual error equation
#' @param elements residual error elements
#' @param ipred the name for the IPRED variable
#' @param type type of model, nonmem or mrgsolve
#' @details
#' currently supports ADD and PROP
#' @examples
#' get_residual_error_eqn("ADD", "IPRED", "nonmem")
#' get_residual_error_eqn(c("ADD", "PROP"), "IPRED", "nonmem")
#' get_residual_error_eqn(c("PROP"), "CP", "mrgsolve")
#' @export
get_residual_error_eqn <- function(elements, ipred, type) {
  SUPPORTED_ELEMENTS <- c("ADD", "PROP")
  if (!all(elements %in% SUPPORTED_ELEMENTS)) {
    stop(glue("residual error eqn mapping only supports:
              {paste0(SUPPORTED_ELEMENTS, collapse = '/')}"))
  }
  name_mapping <- switch(type,
    nonmem = c("EPS(1)", "EPS(2)"),
    mrgsolve = elements
  )
  if (!length(elements)) {
    return(ipred)
  }
  if (length(elements) == 1) {
    return(switch(elements,
      ADD = glue("{ipred} + {name_mapping[1]}"),
      PROP = glue("{ipred}*(1+{ipred}*{name_mapping[1]})")
    ))
  }
  prop <- ifelse(type == "nonmem", glue("EPS({which(elements == 'PROP')})"), "PROP")
  add <- ifelse(type == "nonmem", glue("EPS({which(elements == 'ADD')})"), "ADD")
  return(glue("{ipred}*(1+{ipred}*{prop}) + {add}"))
}
