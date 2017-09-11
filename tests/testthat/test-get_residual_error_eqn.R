context("get_residual_error_eqn")

describe("getting residual error mapping works", {
  it("returns ipred if no residual error structure specified", {
    expect_equal(
      get_residual_error_eqn(c(), "IPRED", "nonmem"),
      "IPRED"
     )
  })
  it("errors if non-supported elements detected", {
    expect_error(get_residual_error_eqn("BAD", "IPRED", "nonmem"))
  })
  it("sets up simple error structures", {
    expect_equal(
      get_residual_error_eqn("ADD", "IPRED", "nonmem"),
      glue::glue("IPRED + EPS(1)")
    )
    expect_equal(
      get_residual_error_eqn("PROP", "IPRED", "nonmem"),
      glue::glue("IPRED*(1+IPRED*EPS(1))")
    )
  })
  it("sets up combined error structure", {
    expect_equal(
      get_residual_error_eqn(c("PROP", "ADD"), "IPRED", "nonmem"),
      glue::glue("IPRED*(1+IPRED*EPS(1)) + EPS(2)")
    )
  })
})
