context("test-partials-nonmem-omega.R")


describe("nonmem omega partials work", {
  blueprint <- Blueprint$new("nonmem")
  blueprint$template <- "
{{#omegas}}
{{> omega}}
{{/omegas}}
"
  it("renders hierarchies properly", {
  blueprint$add_hierarchies(b1 = block(0.4, 0.6, 0.09, param_names = c("CL", "V"), correlation = TRUE))
  expect_equal(blueprint$render(), "\n$OMEGA BLOCK(2)\n4.000000e-01 \n1.138420e-01 9.000000e-02\n\n")

  blueprint$add_hierarchies(b1 = NULL)
  blueprint$add_hierarchies(b1 = block(0.1, 0.6, 0.09, param_names = c("CL", "V"), correlation = TRUE, fixed = TRUE))
  expect_equal(blueprint$render(), "\n$OMEGA BLOCK(2)\n1.000000e-01 FIXED \n5.692100e-02 9.000000e-02\n\n")

  blueprint$add_hierarchies(b1 = NULL)
  })
  it("renders diagnoals properly", {
    blueprint$add_hierarchies(CL = omega_param(0.4, link = "CL"))
    expect_equal(blueprint$render(), "\n$OMEGA\n0.4 \n\n")
    blueprint$add_hierarchies(CL = NULL)
    blueprint$add_hierarchies(CL = omega_param(0.4, link = "CL", fixed = TRUE))
    expect_equal(blueprint$render(), "\n$OMEGA\n0.4  FIX \n\n")
    blueprint$add_hierarchies(V = omega_param(0.4, link = "V"),
                              KA = omega_param(0.1, link = "KA", fixed = TRUE))
    expect_equal(blueprint$render(), "\n$OMEGA\n0.4  FIX \n$OMEGA\n0.4 \n$OMEGA\n0.1  FIX \n\n")
  })
})
