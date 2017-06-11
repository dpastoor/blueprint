context("parameter_details")

describe("parameter_details()", {
  it("creates a parameter given a value", {
    expect_equal(parameter_details(6),
                  list(value = 6,
                       bounds = NULL,
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )
  })
  it("saves bounds", {
    expect_equal(parameter_details(6, c(3, Inf)),
                  list(value = 6,
                       bounds = c(3, Inf),
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )

  })
  it("errors when bounds improperly specified", {
    expect_error(parameter_details(6, c(7)))
  })
  it("errors when bounds not in range", {
    expect_error(parameter_details(6, c(7, Inf)))
  })
})
