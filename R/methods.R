#' @title update a parameters information
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5)
#' cl <- update(cl, lower_bound = 0)
#' @rdname update
#' @export
update <- function(x, ...) {
  UseMethod("update", x)
}

#' @title get the value of a parameter
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5)
#' value(cl)
#' @rdname value
#' @export
value <- function (x, ...) {
  UseMethod("value", x)
}

#' @title get a parameters lower bound
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5)
#' lower_bound(cl)
#' cl <- update(cl, lower_bound = 0)
#' lower_bound(cl)
#' @rdname lower_bound
#' @export
lower_bound <- function (x, ...) {
  UseMethod("lower_bound", x)
}

#' @title get a parameters upper bound
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5, lower_bound = 0)
#' upper_bound(cl)
#' cl <- update(cl, upper_bound = 100)
#' upper_bound(cl)
#' @rdname upper_bound
#' @export
upper_bound <- function (x, ...) {
  UseMethod("upper_bound", x)
}

#' @title get a parameters link to a hierarchy
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5, name = "CL")
#' link(cl)
#' cl2 <- parameter(1.5, name = "TVCL", link = "CL")
#' link(cl2)
#' @rdname link
#' @export
link <- function (x, ...) {
  UseMethod("link", x)
}

#' @title get a parameters name
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5, name = "TVCL")
#' name(cl)
#' @rdname name
#' @export
name <- function (x, ...) {
  UseMethod("name", x)
}

#' @title get whether a parameter is fixed
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5, fixed = TRUE)
#' fixed(cl)
#' cl <- update(cl, fixed = FALSE)
#' fixed(cl)
#' @rdname fixed
#' @export
fixed <- function (x, ...) {
  UseMethod("fixed", x)
}

#' @title get covariate relationships for a parameter
#' @param x parameter
#' @param ... other arguments
#' @examples
#' cl <- parameter(1.5, covariate_relationships = list(WT = ("WT/70")))
#' covariate_relationships(cl)
#' @rdname covariate_relationships
#' @export
covariate_relationships <- function (x, ...) {
  UseMethod("covariate_relationships", x)
}
