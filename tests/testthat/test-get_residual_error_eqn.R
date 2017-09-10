context("get_residual_error_eqn")

describe("getting residual error mapping works", {
  it("returns ipred if no residual error structure specified", {
    expect_equal(
      get_residual_error_eqn(c(), "IPRED"),
      "IPRED"
     )
  })
  it("errors if non-supported elements detected", {
    expect_error(get_residual_error_eqn("BAD", "IPRED"))
  })
  it("sets up simple error structures", {
    expect_equal(
      get_residual_error_eqn("ADD", "IPRED"),
      glue::glue("IPRED + ADD")
    )
    expect_equal(
      get_residual_error_eqn("PROP", "IPRED"),
      glue::glue("IPRED*(1+IPRED*PROP)")
    )
  })
  it("sets up combined error structure", {
    expect_equal(
      get_residual_error_eqn(c("PROP", "ADD"), "IPRED"),
      glue::glue("IPRED*(1+IPRED*PROP) + ADD")
    )
  })
})
