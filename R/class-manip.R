#' prepend classes to a set of object classes
#' @param .x object
#' @param new_classes character string or vector of new classes to prepend
#' @keywords internal
prepend_classes <- function(.x, new_classes) {
  if (!is.character(new_classes)) {
    stop("new classes must be a character vector, not: ",
         typeof(new_classes),
         call. = FALSE)
  }
  all_classes <- c(new_classes, class(.x))
  class(.x) <- all_classes
  return(.x)
}

#' append classes to a set of object classes
#' @param .x object
#' @param new_classes character string or vector of new classes to append
#' @keywords internal
append_classes <- function(.x, new_classes) {
  if (!is.character(new_classes)) {
    stop("new classes must be a character vector, not: ",
         typeof(new_classes),
         call. = FALSE)
  }
  all_classes <- c(class(.x), new_classes)
  class(.x) <- all_classes
  return(.x)
}

