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
