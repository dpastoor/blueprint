#' get the residual error equation
#' @param elements residual error elements
#' @param ipred the name for the IPRED variable
#' @details
#' currently supports ADD and PROP
#' @examples
#' get_residual_error_eqn("ADD", "IPRED")
#' get_residual_error_eqn(c("ADD", "PROP"), "IPRED")
#' get_residual_error_eqn(c("PROP"), "CP")
#' @export
get_residual_error_eqn <- function(elements, ipred) {
  SUPPORTED_ELEMENTS <- c("ADD", "PROP")
  if (!all(elements %in% SUPPORTED_ELEMENTS)) {
    stop(glue("residual error eqn mapping only supports:
              {paste0(SUPPORTED_ELEMENTS, collapse = '/')}"))
  }
  if (!length(elements)) {
    return(ipred)
  }
  if (length(elements) == 1) {
    return(switch(elements,
      ADD = glue("{ipred} + ADD"),
      PROP = glue("{ipred}*(1+{ipred}*PROP)")
    ))
  }
  if (all(c("ADD", "PROP") %in% elements)) {
    return(glue("{ipred}*(1+{ipred}*PROP) + ADD"))
  }
}
