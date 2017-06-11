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
})
