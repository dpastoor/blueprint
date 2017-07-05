#' derive equations
derive_equations <- function(elements) {
  params <- elements$parameters
  omegas <- elements$omegas

  param_names <- names(params)

  omega_names <- purrr::map2(omegas, names(omegas), function(.omega, .name) {
    #
    if (.omega$block) {
      return(.omega$params)
    }
    # for diagonal elements just return the name
    return(.name)
  }) %>% purrr::flatten() %>% unlist()

  both_names <- intersect(param_names, omega_names)
  no_random_effect <- setdiff(param_names, omega_names)
  tvs <- purrr::map(param_names, ~ sprintf("TV%s = THETA(%s)", .x, which(param_names == .x)))
  fulls <- purrr::map(both_names, function(.p) {
    sprintf("%s = TV%s*EXP(ETA(%s))", .p, .p, which(omega_names == .p))
  })
  nofulls <- purrr::map(no_random_effect, function(.p) {
    sprintf("%s = TV%s", .p, .p)
  })
  return(unlist(c(tvs, fulls, nofulls)))
}
