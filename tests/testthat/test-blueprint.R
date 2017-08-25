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
    expect_equal(cl$get_link(), "CL")
    expect_equal(cl$get_name(), "TVCL")
  })

  it("can take a partial parameter definition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = Parameter$new(5, comment = "TVCL; (L/hr)"))
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
})
