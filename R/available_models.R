#' available models
#' @param type software type
#' @export
available_models <- function(type){
  if (!(type %in% c("nonmem", "mrgsolve"))) {
    stop("currently only support mrgsolve and nonmem types")
  }
  nonmem_types <- list(
    one_cmt_iv  = "ADVAN1 TRANS2",
    one_cmt_oral  = "ADVAN2 TRANS2",
    two_cmt_iv  = "ADVAN3 TRANS2",
    two_cmt_oral  = "ADVAN4 TRANS2",
    ode = "ADVAN13"
  )
  mrgsolve_types <- list(
    one_cmt_iv = "[PKMODEL] \n ncmt=1, trans=2 \n\n [CMT] @annotated \n CENT: Central Compartment \n",
    one_cmt_oral = "[PKMODEL] \n ncmt=1, trans=2, depot = true \n\n [CMT] @annotated \n GUT: Gut Compartment \n CENT: Central Compartment \n ",
    two_cmt_iv = "[PKMODEL] \n ncmt=1, trans=2 \n\n [CMT] @annotated \n CENT: Central Compartment \n",
    two_cmt_oral = "[PKMODEL] \n ncmt=2, trans=2, depot = true \n\n [CMT] @annotated \n GUT: Gut Compartment \n CENT: Central Compartment \n PERIPH Peripheral Compartment \n",
    ode = "[ODE] \n"
  )
  if (type == "nonmem") {
    return(nonmem_types)
  }
  return(mrgsolve)
}
