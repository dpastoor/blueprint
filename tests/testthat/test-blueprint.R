context("blueprint")

cl_def <- parameter(5, name = "CL")
cl_def_comment <- parameter(value = 5, name = "CL", comment = "TVCL; (L/hr)")
v_def <- parameter(value = 48.2, name = "V", comment = "TVV; (L)", lower_bound = 0, upper_bound = 200)
cl_def_fixed <- parameter(value = 5, name = "CL", comment = "TVCL; (L/hr)", fixed = TRUE)

describe("Blueprint", {
  it("can take a shorthand parameter definition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = 5)
    cl <- blueprint$get_param("CL")
    expect_equal(cl, cl_def)
  })

  it("can take a complete parameter definition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = cl_def)
    cl <- blueprint$get_param("CL")
    expect_equal(cl, cl_def)
    # to maintain equivalence to test below
    expect_true(isTRUE(all.equal(cl, cl_def)))
  })

  it("sets a new name when specified during parameter addition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(TVCL = cl_def)
    cl <- blueprint$get_param("TVCL")
    expect_false(isTRUE(all.equal(cl, cl_def)))
    expect_equal(link(cl), "CL")
    expect_equal(name(cl), "TVCL")
  })

  it("can take a partial parameter definition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = parameter(5, comment = "TVCL; (L/hr)"))
    cl <- blueprint$get_param("CL")
    expect_equal(cl, cl_def_comment)
  })

  it("can remove a parameter by setting it to NULL", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = 5)
    cl <- blueprint$get_param("CL")
    expect_equal(cl, cl_def)
    blueprint$add_params(CL = NULL)
    expect_null(blueprint$get_param("CL"))
  })

  it("removing all parameters will still give back an empty param list", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = 5)
    cl <- blueprint$get_param("CL")
    expect_equal(cl, cl_def)
    blueprint$add_params(CL = NULL)
    expect_equal(length(blueprint$get_params()), 0)
    expect_true(is.list(blueprint$get_params()))
  })

  it("can handle constants", {
    blueprint <- Blueprint$new()
    blueprint$add_constants(STD_WT = 70)

    expect_equal(blueprint$get_all_elements()$constants,
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
  blueprint$add_constants(CL_WT = const(0.75, "allometric exponent"))
  expect_equal(blueprint$get_all_elements()$constants,
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
