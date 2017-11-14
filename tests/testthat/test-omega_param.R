context("omega-param")

describe("omega_param() works", {
  it("can be initialized correctly", {
    input <- omega_param(0.04, link = "CL")
    output <-
      structure(
        list(
          block = FALSE,
          link = "CL",
          fixed = FALSE,
          correlation = FALSE,
          value = 0.04,
          comment = NULL
        ),
        .Names = c("block", "link",
                   "fixed", "correlation", "value", "comment"),
        class = "omega"
      )
    expect_equal(input,
                 output)
  })
})
