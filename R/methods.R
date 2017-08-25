update <- function(x, ...) {
  UseMethod("update", x)
}
value <- function (x, ...) {
  UseMethod("value", x)
}

lower_bound <- function (x, ...) {
  UseMethod("lower_bound", x)
}


upper_bound <- function (x, ...) {
  UseMethod("upper_bound", x)
}


link <- function (x, ...) {
  UseMethod("link", x)
}


name <- function (x, ...) {
  UseMethod("name", x)
}


fixed <- function (x, ...) {
  UseMethod("fixed", x)
}


covariate_relationships <- function (x, ...) {
  UseMethod("covariate_relationships", x)
}
