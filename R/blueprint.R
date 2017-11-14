#' base blueprint class
#' @importFrom R6 R6Class
#' @importFrom purrr map map2 is_list set_names map_dbl map_lgl discard keep flatten
#' @importFrom rlang is_bare_numeric
#' @importFrom glue glue
#' @export
Blueprint <-
  R6::R6Class("blueprint",
     public = list(
       partials = NULL,
       type = NULL,
       routine = NULL,
       initialize = function(type = "nonmem") {
         # TODO: remove, just using this as a placeholder
         message(sprintf("initializing new blueprint of type: %s", type))
         if (missing(type)) {
           type <- NULL
         }
         SUPPORTED_TYPES <- c("nonmem", "mrgsolve")
         if (!(type %in% SUPPORTED_TYPES) && !is.null(type)) {
           stop(glue("only support types: {paste0(SUPPORTED_TYPES, collapse = ',')}"))
         }
         if (!is.null(type)) {
          self$type <- type
          self$partials <- load_partials(type)
          private$equation_mapper <- equation_derivations(type)
         } else {
           message("no type specified, no pre-loaded templates initialized")
         }
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
            if (is_bare_numeric(param_info)) {
              return(const(param_info, comment = .pn))
            }
            if (!inherits(param_info, "const")) {
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
       add_ignores = function(ignores, ..., .overwrite = FALSE) {
         if (.overwrite) {
           private$ignore_strings <- ignores
           return(self)
         }
         private$ignore_strings <- c(private$ignore_strings, ignores)
         return(self)
       },
       add_accepts = function(accepts, ..., .overwrite = FALSE) {
         if (.overwrite) {
           private$accept_strings <- accepts
           return(self)
         }
         private$accept_strings <- c(private$accept_strings, accepts)
         return(self)
       },
       # add_params adds parameters specified either shorthand CL = 5,
       # or via param(), it returns a vector of parameter names
       # for any created
       add_params = function(..., .overwrite = TRUE) {
           param_list <- dots(...)
           param_names <- names(param_list)
           # clear out any null parameters
           null_indices <- purrr::map_lgl(param_list, is.null)
           to_remove <- param_names[null_indices]
           purrr::walk(to_remove, function(.x) {
             private$parameters[[.x]] <- NULL
           })
           param_list <- param_list[!null_indices]
           param_names <- param_names[!null_indices]
           if (!length(param_list)) {
             return(invisible())
           }
           constructed_params <- map2(param_list, param_names, function(param_info, .pn) {
              # if numeric assume shorthand value only
              # CL = 4.5
              if (is_bare_numeric(param_info)) {
                return(parameter(param_info, name = .pn))
              }
              if (!inherits(param_info, "parameter")) {
                stop(sprintf("incorrect specification for %s,
                             please construct a parameter specification with `parameter()`", .pn))
              }

             # for now will force a name for all parameters, through the add_param
             # call, eg add_param(<name> = parameter()), so even if a name
             # is set, it will override.
              param_info <- update(param_info, name = .pn)
             # if link null, set equal to name
             if (is.null(link(param_info))) {
               param_info <- update(param_info, link = name(param_info))
             }
             return(param_info)
           })
           # in case anything was just given as a parameter block
           constructed_param_names <- purrr::map(constructed_params, ~ name(.x))
           final_parameters <- modifyList(private$parameters,
                                            set_names(constructed_params, constructed_param_names)) %>%
             discard(is.null)
           # if get a case where everything is overwritten set to empty list
           if (!length(final_parameters)) {
             # don't want a named list just a bare empty list
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
       add_hierarchies = function(...){
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
            if (is_bare_numeric(omega_info)) {
              return(omega_param(omega_info, .pn, fixed = FALSE))
            }
            # for now going to make the big assumption people will
            # actually use block()/omega_param to create full omegas specifications,
            # maybe should create an actual class and check for it
            # but for now going to trust
            if (!inherits(omega_info, "omega") && !inherits(omega_info, "block")) {
              stop(sprintf("incorrect specification for %s, please use omega_param() or block()", .pn))
            }
              return(omega_info)
           })
           final_omegas <- modifyList(private$omegas,
                                            set_names(constructed_omegas, omega_names)) %>%
             discard(is.null)
           # if get a case where everything is overwritten set to empty list
           if (!length(final_omegas)) {
             # don't want a named list just a bare empty list
             final_omegas <- list()
           }
           block_names <- final_omegas %>%
             keep(~ .x$block) %>%
             map(~ .x$params) %>%
             flatten()
           diag_names <- final_omegas %>%
             discard(~ .x$block) %>%
             names(.)

           both_names <- intersect(block_names, diag_names)
           if (length(both_names)) {
             stop(glue::glue("detected omega elements in both a diagonal and block element for: {params}",
                             params = paste0(both_names, collapse = ", ")))
           }
           private$omegas <- final_omegas
           return(names(constructed_omegas))
       },
       add_residual_error = function(...){
        # support additive, additive + prop, and prop error to start
         ACCEPTED_NAMES <- c("ADD", "PROP")
         # TODO: currently basically add_param but tweaked to save to sigma - should refactor
         sigma_list <- dots(...)
         sigma_names <- names(sigma_list)
         sigma_names <- sigma_names[!is.null(sigma_names)]
         if (length(sigma_list) != length(sigma_names)) {
           stop("all elements must be named - even blocks!")
         }
         constructed_sigmas <- map2(sigma_list, sigma_names, function(sigma_info, .pn) {
           if (is.null(sigma_info)) {
             return(NULL)
           }
           # if numeric assume shorthand value only
           # CL = 4.5
           if (is_bare_numeric(sigma_info)) {
             if (!(.pn %in% ACCEPTED_NAMES)) {
               stop(glue("the only residual error names currently supported are:
                         {paste0(ACCEPTED_NAMES, collapse = ',')}"))
             }
             return(sigma_param(sigma_info, .pn, FALSE, comment = .pn))
           }

           if (!inherits(sigma_info, "sigma") && !inherits(sigma_info, "block")) {
             stop(sprintf("incorrect specification for %s, please use sigma_param() or block()", .pn))
           }
           if (inherits(sigma_info, "block")) {
             if (!all(names(sigma_info) %in% ACCEPTED_NAMES)) {
               # improve this error message
               stop(glue("the only residual error names currently supported are:
                         {paste0(ACCEPTED_NAMES, collapse = ',')}"))
             }
           }
           return(sigma_info)
         })
         constructed_sigmas <- purrr::map2(constructed_sigmas,
                                           sigma_names,
                                           .f = function(.sigma, .name) {
           if (is.null(.sigma$name)) {
             .sigma$name <- .name
           }
           return(.sigma)
         })
         final_sigmas <- modifyList(private$sigmas,
                                    set_names(constructed_sigmas, sigma_names)) %>%
           discard(is.null)
         # if get a case where everything is overwritten set to empty list
         if (!length(final_sigmas)) {
           final_sigmas <- list()
         }
           block_names <- final_sigmas %>%
             keep(~ .x$block) %>%
             map(~ .x$params) %>%
             flatten()
           diag_names <- final_sigmas %>%
             discard(~ .x$block) %>%
             names(.)

           both_names <- intersect(block_names, diag_names)
           if (length(both_names)) {
             stop(glue::glue("detected omega elements in both a diagonal and block element for: {params}",
                             params = paste0(both_names, collapse = ", ")))
           }
         private$sigmas <- final_sigmas
         return(names(constructed_sigmas))
       },
       get_all_elements = function() {
         return(
           list(
             constants = private$constants,
             parameters = private$parameters,
             omegas = private$omegas,
             sigmas = private$sigmas
           )
         )
       },
       render = function() {
         if (is.null(private$templ)) {
            stop("no template defined")
         }
         settings <- purrr::map(self$get_all_elements(), strip_names)
         ipred <- ifelse(self$type == "mrgsolve", "CP", "IPRED")
         resid_error <- get_residual_error_eqn(
           purrr::flatten_chr(map(settings$sigmas, names)),
           ipred,
           self$type
           )
         whisker::whisker.render(self$template,
                        modifyList(settings,
                                   list(
                                     equations = private$equation_mapper(self$get_all_elements()),
                                     routine = self$routine,
                                     input = paste0(names(private$dat), collapse = " "),
                                     ignore = private$ignore_strings,
                                     accept = private$accept_strings,
                                     data = private$datpath,
                                     residual_error_eqn = resid_error
                                   )),
                        partials = self$partials
         )
       }
     ),
     # active bindings available via self
     active = list(
       data = function(.data) {
         if (missing(.data)) {
           return(private$dat)
         }
         private$dat <- .data
       },

       datapath = function(.datapath) {
         if (missing(.datapath)) {
           return(private$datpath)
         }
         private$datpath <- .datapath
       },
       template = function(.template) {
         if (missing(.template)) {
           return(private$templ)
         }
         private$templ <- .template
       }
     ),
     private = list(
       # given a a dataset to extract information for the model
       datpath = NULL,
       dat = NULL,
       templ = NULL,
       constants = list(),
       accept_strings = NULL,
       ignore_strings = NULL,
       parameters = list(),
       omegas = list(),
       sigmas = list(),
       equation_mapper = NULL
     )
)
