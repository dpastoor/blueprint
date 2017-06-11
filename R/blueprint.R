#' base blueprint class
#' @importFrom R6 R6Class
#' @importFrom purrr map is_numeric is_list set_names
#' @export
Blueprint <-
  R6::R6Class("blueprint",
     public = list(
       initialize = function() {
         # TODO: remove, just using this as a placeholder
         message("initializing new blueprint")
       },

       # add_params adds parameters specified either shorthand CL = 5,
       # or via param(), it returns a vector of parameter names
       # for any created
       add_params = function(..., .overwrite = TRUE) {
           param_list <- dots(...)
           param_names <- names(param_list)
           constructed_params <- map(param_names, function(.pn) {
            param_info <- param_list[[.pn]]
            # if numeric assume shorthand value only
            # CL = 4.5
            if (is_numeric(param_info)) {
              return(param(param_info))
            }
            # for now going to make the big assumption people will
            # actually use param() to create full parameter specifications,
            # maybe should create an actual class and check for it
            # but for now going to trust
            if (!is_list(.pn)) {
              stop(sprintf("incorrect specification for %s, please use param()", .pn))
            }
              return(.pn)
           })
           private$parameters <- modifyList(private$parameters,
                                            set_names(constructed_params, param_names))
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
             set_names(map_dbl(private$parameters, ~ .x$value), names(parameters))
           )
         }
         return(private$paramaters)
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
