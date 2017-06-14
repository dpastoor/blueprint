context("param")

describe("param() works", {
  it("creates a parameter given a value", {
    expect_equal(param(6),
                  list(value = 6,
                       comment = NULL,
                       bounds = NULL,
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )
  })
  it("saves bounds", {
    expect_equal(param(6, .bounds = c(3, Inf)),
                  list(value = 6,
                       comment = NULL,
                       bounds = c(3, Inf),
                       fixed = FALSE,
                       covariate_relationships = NULL
                       )
                  )

  })
  it("errors when bounds improperly specified", {
    expect_error(param(6, .bounds = c(7)))
  })
  it("errors when bounds not in range", {
    expect_error(param(6, .bounds = c(7, Inf)))
  })
})
