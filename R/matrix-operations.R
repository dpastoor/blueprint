
#' return the scaled covariance matrix as a string to embed in a file, in scientific notation
#' @param .mat matrix
#' @param .fixed whether to include FIXED in the first line (for nonmem)
#' @export
stringify_matrix <- function(.mat, .fixed = TRUE) {
  num_rows <- dim(.mat)[1]
  matrix_elements <- .mat[lower.tri(.mat, diag = TRUE)]
  # the row_indices logic below holds for 3, but does not work for 2
  if (num_rows <= 2) {
    mat_list <- purrr::map(1:num_rows, ~ {
      if (.x == 1) {
        indices <- 1
      } else {
        indices <- c(2, 3)
      }
      return(paste0(sprintf("%e", matrix_elements[indices]), collapse = " "))
      # clear out the elements that have been placed in the list
    })
  } else {
    row_indices <- c(0, 2:num_rows)
    mat_list <- purrr::map(1:num_rows, ~ {
      indices <- .x + row_indices[1:.x]
      return(paste0(sprintf("%e", matrix_elements[indices]), collapse = " "))
      # clear out the elements that have been placed in the list
    })
  }
  if (.fixed) {
    mat_list[[1]] <- sprintf("%s FIXED", mat_list[[1]])
  }
  return(paste0(mat_list, collapse = " \n"))
}
