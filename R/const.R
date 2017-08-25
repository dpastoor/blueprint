#' create a constant parameter
#' @param value value to hold
#' @param comment comment
#' @export
const <- function(value, comment) {
  return(structure(list(value = value, comment = comment), class = "const"))
}
