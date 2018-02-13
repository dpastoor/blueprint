context("blueprint")




describe("Blueprint", {
  blueprint <- suppressWarnings(new_blueprint())
  cl_def <- parameter(5, name = "CL")
  cl_def_comment <-
    parameter(value = 5,
              name = "CL",
              comment = "TVCL; (L/hr)")
  v_def <-
    parameter(
      value = 48.2,
      name = "V",
      comment = "TVV; (L)",
      lower_bound = 0,
      upper_bound = 200
    )
  cl_def_fixed <-
    parameter(
      value = 5,
      name = "CL",
      comment = "TVCL; (L/hr)",
      fixed = TRUE
    )
  it("can be initialized with no type", {
    expect_message(new_blueprint(), "no model type specified")
    blueprint <- new_blueprint()
    expect_equal(blueprint, structure(
      list(
        hooks = NULL,
        partials = NULL,
        equation_mapper = NULL,
        constants = list(),
        parameters = list(),
        omegas = list(),
        sigmas = list()
      ),
      .Names = c(
        "hooks",
        "partials",
        "equation_mapper",
        "constants",
        "parameters",
        "omegas",
        "sigmas"
      ),
      class = "blueprint"
    ))
  })

  it("can be initialized with a type", {
    expect_message(new_blueprint("nonmem"),
                   "initializing new blueprint of type: nonmem")

  })

  it("can take a shorthand parameter definition", {
    bp_cl <- blueprint %>%
      parameters(CL = 5)
  })

  it("can take a complete parameter definition", {
    bp_cl <- blueprint %>%
      parameters(CL = cl_def)
    blueprint$add_params(CL = cl_def)
    cl <- bp_cl$parameters$CL
    expect_equal(cl, cl_def)
    # to maintain equivalence to test below
    expect_true(isTRUE(all.equal(cl, cl_def)))
  })

  it("sets a new name when specified during parameter addition", {
    bp_cl <- blueprint %>%
      parameters(TVCL = cl_def)
    cl <- bp_cl$parameters$TVCL
    expect_false(isTRUE(all.equal(cl, cl_def)))
    expect_equal(link(cl), "CL")
    expect_equal(name(cl), "TVCL")
    expect_equal(value(cl), value(cl_def))
  })

  it("can take a partial parameter definition", {
    bp_cl <- blueprint %>%
      parameters(CL = parameter(5, comment = "TVCL; (L/hr)"))
    cl <- blueprint$parameters$CL
    expect_equal(cl, cl_def_comment)
  })

  it("can remove a parameter by setting it to NULL", {
    bp_cl <- blueprint %>%
    parameters(CL = 5)
    cl <- bp_cl$parameters$CL
    expect_equal(cl, cl_def)
    bp_cl <- bp_cl %>% parameters(CL = NULL)
    expect_null(blueprint$parameters$CL)
  })

  it("removing all parameters will still give back an empty param list",
     {

    bp_cl <- blueprint %>%
    parameters(CL = 5)
       cl <- bp_cl$parameters$CL
       expect_equal(cl, cl_def)
    bp_cl <- bp_cl %>% parameters(CL = NULL)
       expect_equal(length(bp_cl$parameters), 0)
       expect_true(is.list(bp_cl$parameters))
     })

  it("can handle constants", {
    bp_c <- blueprint %>%
      constants(STD_WT = 70)

    expect_equal(blueprint$constants,
                 structure(list(
                   STD_WT = structure(
                     list(
                       value = 70,
                       comment = "STD_WT",
                       name = "STD_WT"
                     ),
                     .Names = c("value", "comment", "name"),
                     class = "const"
                   )
                 ), .Names = "STD_WT"))
    bp_c <- blueprint %>%
      constants(CL_WT = const(0.75, "allometric exponent"))
    expect_equal(blueprint$constants,
                 structure(
                   list(
                     STD_WT = structure(
                       list(
                         value = 70,
                         comment = "STD_WT",
                         name = "STD_WT"
                       ),
                       .Names = c("value", "comment", "name"),
                       class = "const"
                     ),
                     CL_WT = structure(
                       list(
                         value = 0.75,
                         comment = "allometric exponent",
                         name = "CL_WT"
                       ),
                       .Names = c("value", "comment", "name"),
                       class = "const"
                     )
                   ),
                   .Names = c("STD_WT", "CL_WT")
                 ))
  })
})
