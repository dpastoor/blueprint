context("blueprint")

cl_def <- list(value = 5, comment = NULL, lower_bound = NULL, upper_bound = NULL, fixed = FALSE, covariate_relationships = NULL)
cl_def_comment <- list(value = 5, comment = "TVCL; (L/hr)", lower_bound = NULL, upper_bound = NULL, fixed = FALSE, covariate_relationships = NULL)
v_def <- list(value = 48.2, comment = "TVV; (L)", lower_bound = 0, upper_bound = 200, fixed = FALSE, covariate_relationships = NULL)
cl_def_fixed <- list(value = 5, comment = "TVCL; (L/hr)", lower_bound = NULL, upper_bound = NULL, fixed = TRUE, covariate_relationships = NULL)

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
  })

  it("can take a partial parameter definition", {
    blueprint <- Blueprint$new()
    blueprint$add_params(CL = param(5, "TVCL; (L/hr)"))
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
