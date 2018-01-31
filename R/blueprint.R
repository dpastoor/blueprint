#' create a blueprint object

#' @param .type type of model object to create (eg: nonmem/mrgsolve)
#' @return object with class parameter
#' @examples
#' bp <- new_blueprint("nonmem")
#'
#' # can just start templating, but will need to set a type before rendering
#' bp <- new_blueprint()
#' @rdname parameter
#' @export
new_blueprint <- function(
  .type = NULL
) {
  SUPPORTED_TYPES <- c("nonmem", "mrgsolve")
  if (is.null(.type)) {
    message("no model type specified, please add via `type()` before calling render")
  } else {
    if (!(.type %in% SUPPORTED_TYPES)) {
      warning(glue("only support types: {paste0(SUPPORTED_TYPES, collapse = ',')}"))
    }
    message(glue("initializing new blueprint of type: {.type}"))
  }

  partials <- NULL
  equation_mapper <- NULL
  if (!is.null(.type)) {
    partials <- load_partials(.type)
    equation_mapper <- equation_derivations(.type)
  } else {
    message("no type specified, no pre-loaded templates initialized")
  }

  initialized <- list(
    # these will be specific to the classes
    hooks = NULL,
    partials = partials,
    equation_mapper = equation_mapper,

    constants = list(),
    parameters = list(),
    omegas = list(),
    sigmas = list()
  )

  class(initialized) <- "blueprint"

  if (!is.null(.type)) {
    initialized <- prepend_classes(
      initialized,
      glue("{.lt}_blueprint", .lt = tolower(.type))
    )
  }
  return(initialized)
}

print.nonmem_blueprint <- function(x, ...) {
  cat("nonmem blueprint with settings: \n\n")
  cat(glue("{length(x$constants)} constants \n\n"))
  cat(glue("{length(x$parameters)} parameters \n\n"))
  cat(glue("{length(x$omegas)} omegas \n\n"))
  cat(glue("{length(x$hooks)} hooks \n\n"))
}

print.blueprint <- function(x, ...) {
  print("generic blueprint")
}
