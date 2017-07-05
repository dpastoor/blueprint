#' base blueprint class
#' @importFrom R6 R6Class
#' @importFrom purrr map is_numeric is_list set_names map_dbl
#' @export
Blueprint <-
  R6::R6Class("blueprint",
     public = list(
       partials = NULL,
       initialize = function() {
         # TODO: remove, just using this as a placeholder
         message("initializing new blueprint")
         self$partials <- load_nonmem_partials()
       },
       add_constants = function(..., .overwrite = TRUE) {
           param_list <- dots(...)
           param_names <- names(param_list)
           constructed_params <- map(param_names, function(.pn) {
            param_info <- param_list[[.pn]]

            if (is.null(param_info)) {
              return(NULL)
            }
            # if numeric assume shorthand value only
            # CL = 4.5
            if (is_numeric(param_info)) {
              return(const(param_info,.comment = ""))
            }
            # for now going to make the big assumption people will
            # actually use param() to create full parameter specifications,
            # maybe should create an actual class and check for it
            # but for now going to trust
            if (!is_list(param_info)) {
              stop(sprintf("incorrect specification for %s, please use const()", .pn))
            }
              return(param_info)
           })
           final_parameters <- modifyList(private$constants,
                                            set_names(constructed_params, param_names))
           # if get a case where everything is overwritten set to empty list
           if(is.null(final_parameters)) {
             final_parameters <- list()
           }
           private$constants <- purrr::map2(final_parameters, names(final_parameters), function(.p, .n) {
              .p$name <- .n
              return(.p)
           })
           return(names(constructed_params))
       },
       # add_params adds parameters specified either shorthand CL = 5,
       # or via param(), it returns a vector of parameter names
       # for any created
       add_params = function(..., .overwrite = TRUE) {
           param_list <- dots(...)
           param_names <- names(param_list)
           constructed_params <- map(param_names, function(.pn) {
            param_info <- param_list[[.pn]]

            if (is.null(param_info)) {
              return(NULL)
            }
            # if numeric assume shorthand value only
            # CL = 4.5
            if (is_numeric(param_info)) {
              return(param(param_info))
            }
            # for now going to make the big assumption people will
            # actually use param() to create full parameter specifications,
            # maybe should create an actual class and check for it
            # but for now going to trust
            if (!is_list(param_info)) {
              stop(sprintf("incorrect specification for %s, please use param()", .pn))
            }
              return(param_info)
           })
           final_parameters <- modifyList(private$parameters,
                                            set_names(constructed_params, param_names))
           # if get a case where everything is overwritten set to empty list
           if(is.null(final_parameters)) {
             final_parameters <- list()
           }
           private$parameters <- final_parameters
           return(names(constructed_params))
       },
       get_param = function(.x, .value_only = FALSE){
         param_info <- private$parameters[[.x]]
         if (.value_only) {
            return(set_names(.x, param_info$value))
         }
         return(param_info)
       },
       get_params = function(.value_only = FALSE){
         if (.value_only) {
           return(
             set_names(map_dbl(private$parameters, ~ .x$value), names(private$parameters))
           )
         }
         return(private$parameters)
       },
       add_heirarchy = function(...){
         # TODO: currently basically add_param but tweaked to save to omega - should refactor
           omega_list <- dots(...)
           omega_names <- names(omega_list)
           omega_names <- omega_names[!is.null(omega_names)]
           if (length(omega_list) != length(omega_names)) {
             stop("all elements must be named - even blocks!")
           }
           constructed_omegas <- map(omega_names, function(.pn) {
            omega_info <- omega_list[[.pn]]

            if (is.null(omega_info)) {
              return(NULL)
            }
            # if numeric assume shorthand value only
            # CL = 4.5
            if (is_numeric(omega_info)) {
              return(omega_param(omega_info, .pn))
            }
            # for now going to make the big assumption people will
            # actually use block()/omega_param to create full omegas specifications,
            # maybe should create an actual class and check for it
            # but for now going to trust
            if (!is_list(omega_info)) {
              stop(sprintf("incorrect specification for %s, please use omega_omega()", .pn))
            }
              return(omega_info)
           })
           final_omegas <- modifyList(private$omega,
                                            set_names(constructed_omegas, omega_names))
           # if get a case where everything is overwritten set to empty list
           if(is.null(final_omegas)) {
             final_omegas <- list()
           }
           private$omega <- final_omegas
           return(names(constructed_omegas))
       },
       get_all_elements = function() {
         return(
           list(
             parameters = private$parameters,
             omega = private$omega,
             sigma = private$sigma
           )
         )
       }
     ),
     private = list(
       # given a dataset to extract information for the model
       data = NULL,
       # parameters is a named list of lists, by parameter name
       # taking the form param = list(value, bounds, fixed, covariate_relationships)
       parameters = list(),
       omega = list(),
       sigma = list()

     )
)
