context("blueprint:mrgsolve")

eq_template <- "
{{#equations}}
  {{.}}
{{/equations}}
"

resid_error_template <- "
DV = {{residual_error_eqn}}
"

describe("mrgsolve blueprint", {
  cl <- parameter(1.5, name = "CL", comment = "mg/L")
  v <- parameter(14.5, name = "V", comment = "mg/L")
  ka <- parameter(0.5, name = "KA", comment = "1/hr")
  blueprint <- Blueprint$new("mrgsolve")
  blueprint$add_params(CL = cl, V = v, KA = ka)
  blueprint$template <- eq_template

  it("can handle a basic template", {
    expect_equal(blueprint$render(),
                 "\n  CL = TVCL\n  V = TVV\n  KA = TVKA\n")
  })
  it("can handle a parameter with diagonal random effect hierarchy", {
    blueprint$add_hierarchies(CL = 0.4)
    expect_equal(blueprint$render(),
                 "\n  TVCL = CL\n  CL = TVCL*exp(ETA1)\n  V = TVV\n  KA = TVKA\n")
  })

  it("can handle a parameter with block random effect hierarchy", {
    # fails as adds 2 CL's so should error
    expect_error(blueprint$add_hierarchy(bl = block(0.1, 0.01, 0.1, param_names = c("CL", "V"))))
    # can clear clearance
    blueprint$add_hierarchies(CL = NULL)
    expect_equal(blueprint$get_all_elements()$omegas, list())
    blueprint$add_hierarchies(bl = block(0.1, 0.01, 0.1, param_names = c("CL", "V")))
    expect_equal(blueprint$render(),
                 "\n  TVCL = CL\n  TVV = V\n  CL = TVCL*exp(ETA1)\n  V = TVV*exp(ETA2)\n  KA = TVKA\n")
  })
})

describe("residual error works for mrgsolve", {
  blueprint <- Blueprint$new("mrgsolve")
  blueprint$template <- resid_error_template
  it("works for simple error structures", {
  bp <- blueprint$clone()
  bp$add_residual_error(ADD = 0.1)
  expect_equal(bp$render(), "\nDV = CP + ADD\n")

  bp <- blueprint$clone()
  bp$add_residual_error(PROP = 0.1)
  expect_equal(bp$render(), "\nDV = CP*(1+PROP)\n")
  })
  it("works iteratively", {
  bp <- blueprint$clone()
  bp$add_residual_error(ADD = 0.1)
  expect_equal(bp$render(), "\nDV = CP + ADD\n")

  bp$add_residual_error(PROP = 0.1)
  expect_equal(bp$render(), "\nDV = CP*(1+PROP) + ADD\n")
  })
})
