context("blueprint:nonmem")

eq_template <- "
{{#equations}}
  {{.}}
{{/equations}}
"

describe("nonmem blueprint", {
  cl <- parameter(1.5, name = "CL", comment = "mg/L")
  v <- parameter(14.5, name = "V", comment = "mg/L")
  ka <- parameter(0.5, name = "KA", comment = "1/hr")
  blueprint <- Blueprint$new("nonmem")
  blueprint$add_params(CL = cl, V = v, KA = ka)
  blueprint$template <- eq_template

  it("can handle a basic template", {
    blueprint$render()
  })
  it("can handle a parameter with diagonal random effect heirarchy", {
    blueprint$add_heirarchy(CL = 0.4)
    blueprint$render()
  })
  it("can handle a parameter with block random effect heirarchy", {
    # fails as adds 2 CL's therefore double renders the equations
    blueprint$add_heirarchy(bl = block(0.1, 0.01, 0.1, .params = c("CL", "V")))
    blueprint$render()
  })

})
