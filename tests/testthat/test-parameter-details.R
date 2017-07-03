context("param")

describe("param() works", {
  it("creates a parameter given a value", {
    expect_equal(param(6),
                  list(value = 6,
                       comment = NULL,
                       lower_bound = NULL,
                       upper_bound = NULL,
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )
  })
  it("saves bounds", {
    expect_equal(param(6, .lower_bound = 3),
                  list(value = 6,
                       comment = NULL,
                       lower_bound = 3,
                       upper_bound = NULL,
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )

  })
  it("errors when bounds not in range", {
    expect_error(param(6, .lower_bound = 2, .upper_bound = 4))
    expect_error(param(6, .lower_bound = 7))
  })
})
