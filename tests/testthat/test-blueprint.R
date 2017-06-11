context("blueprint")

cl_def <- list(value = 5, bounds = NULL, fixed = FALSE, covariate_relationships = NULL)
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
})
