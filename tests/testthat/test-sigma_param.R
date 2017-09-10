context("sigma-param")

describe("sigma_param() works", {
  it("can be initialized correctly", {
    input <- sigma_param(0.04, name = "PROP")
    output <-
      structure(
        list(
          block = FALSE,
          name = "PROP",
          fix = FALSE,
          correlation = FALSE,
          value = 0.04,
          comment = NULL
        ),
        .Names = c("block", "name", "fix",
                   "correlation", "value", "comment"),
        class = "sigma"
      )
    expect_equal(input,
                 output)
  })
})
