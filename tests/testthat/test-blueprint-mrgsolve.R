context("blueprint:mrgsolve")

eq_template <- "
{{#equations}}
  {{.}}
{{/equations}}
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
  it("can handle a parameter with diagonal random effect heirarchy", {
    blueprint$add_heirarchies(CL = 0.4)
    expect_equal(blueprint$render(),
                 "\n  TVCL = CL\n  CL = TVCL*exp(ETA1)\n  V = TVV\n  KA = TVKA\n")
  })

  it("can handle a parameter with block random effect heirarchy", {
    # fails as adds 2 CL's so should error
    expect_error(blueprint$add_heirarchy(bl = block(0.1, 0.01, 0.1, param_names = c("CL", "V"))))
    # can clear clearance
    blueprint$add_heirarchies(CL = NULL)
    expect_equal(blueprint$get_all_elements()$omegas, list())
    blueprint$add_heirarchies(bl = block(0.1, 0.01, 0.1, param_names = c("CL", "V")))
    expect_equal(blueprint$render(),
                 "\n  TVCL = CL\n  TVV = V\n  CL = TVCL*exp(ETA1)\n  V = TVV*exp(ETA2)\n  KA = TVKA\n")
  })
})
