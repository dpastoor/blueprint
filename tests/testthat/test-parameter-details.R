context("parameters")

describe("Parameter works", {
  it("creates a basic parameter given a value", {
    param1 <- Parameter$new("tmp", 6)
    expect_equal(param1$get_value(), 6)
    expect_equal(param1$get_lower_bound(), -Inf)
    expect_equal(param1$get_upper_bound(), Inf)
  })
  it("saves bounds", {
    param2 <- Parameter$new("tmp", 6, lower_bound = 3)
    expect_equal(param2$get_value(), 6)
    expect_equal(param2$get_lower_bound(), 3)
  })
  it("errors when bounds not in range", {
    expect_error(Parameter$new("tmp", 6, lower_bound = 2, upper_bound = 4))
    expect_error(Parameter$new("tmp", 6, lower_bound = 7))
    expect_error(Parameter$new("tmp", 6, upper_bound = 3))
  })
  it("can give its private fields as a list for templating", {
    expect_equal(Parameter$new("tmp", 6, lower_bound = 3)$get_all_information(),
         structure(list(name = "tmp", value = 6, comment = NULL, lower_bound = 3,
                        upper_bound = Inf, fixed = FALSE, covariate_relationships = NULL,
                        link = "tmp"),
                   .Names = c("name", "value", "comment", "lower_bound",
                            "upper_bound", "fixed", "covariate_relationships", "link")))
  })
})
