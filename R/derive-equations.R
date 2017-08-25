equation_derivations = function(type) {
  switch(type,
         mrgsolve = derive_equations_mrgsolve,
         nonmem = derive_equations_nonmem
         )
}

# derive equations for nonmem
derive_equations_nonmem <- function(elements) {
  params <- elements$parameters
  omegas <- elements$omegas

  # this should be refactored to links for clarity
  param_names <- purrr::map_chr(params, ~ link(.x))
  if (length(omegas)) {
    omega_names <- purrr::map2(omegas, names(omegas), function(.omega, .name) {
      #
      if (.omega$block) {
        return(.omega$params)
      }
      # for diagonal elements just return the name
      return(.name)
    }) %>% purrr::flatten() %>% unlist()
  } else {
    omega_names <- c()
  }

  both_names <- intersect(param_names, omega_names)
  no_random_effect <- setdiff(param_names, omega_names)

  nofulls <- purrr::map(no_random_effect, function(.p) {
    sprintf("%s = THETA(%s)", .p, which(param_names == .p))
  })

  if (length(both_names)) {
    tvs <- purrr::map(both_names, ~ sprintf("TV%s = THETA(%s)", .x, which(param_names == .x)))
    fulls <- purrr::map(both_names, function(.p) {
      sprintf("%s = TV%s*EXP(ETA(%s))", .p, .p, which(omega_names == .p))
    })
  } else {
    fulls <- list()
    tvs <- list()
  }
  # need to flatten one level so not a list of lists
  return(purrr::flatten(list(tvs, fulls, nofulls)))
}

# derive equations
derive_equations_mrgsolve <- function(elements) {
  params <- elements$parameters
  omegas <- elements$omegas

  param_names <- purrr::map_chr(params, ~ link(.x))
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
  fulls <- purrr::map(both_names, function(.p) {
    sprintf("%s = TV%s*exp(ETA%s)", .p, .p, which(omega_names == .p))
  })
  nofulls <- purrr::map(no_random_effect, function(.p) {
    sprintf("%s = TV%s", .p, .p)
  })
  # need to flatten one level so not a list of lists
  return(purrr::flatten(list(fulls, nofulls)))
}
