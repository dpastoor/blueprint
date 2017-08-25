context("parameters")

describe("Parameter works", {
  it("creates a basic parameter given a value", {
    param1 <- parameter(6)
    expect_equal(value(param1), 6)
    expect_equal(lower_bound(param1), -Inf)
    expect_equal(upper_bound(param1), Inf)
  })
  it("saves bounds", {
    param2 <- parameter(6, lower_bound = 3)
    expect_equal(value(param2), 6)
    expect_equal(lower_bound(param2), 3)
  })
  it("errors when bounds not in range", {
    expect_error(parameter(6, lower_bound = 2, upper_bound = 4))
    expect_error(parameter(6, lower_bound = 7))
    expect_error(parameter(6, upper_bound = 3))
  })
  it("constructs all elements", {
    expect_equal(parameter(6, lower_bound = 3),
         structure(list(name = NULL, value = 6, comment = NULL, lower_bound = 3,
                        upper_bound = Inf, fixed = FALSE, covariate_relationships = NULL,
                        link = NULL),
                   .Names = c("name", "value", "comment", "lower_bound",
                            "upper_bound", "fixed", "covariate_relationships", "link"), class = "parameter"))
  })
  it("errors when updating elements that don't exist", {
    param1 <- parameter(6)
    expect_error(update(param1, notExist = 2))
  })
})
