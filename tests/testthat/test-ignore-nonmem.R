context("ignore:nonmem")


describe("ignore statements are generated for nonmem", {
  tmpl <- "
{{#ignore}}
IGNORE=({{{.}}})
{{/ignore}}
"
  it("can construct a full ignore statement", {
    bp <- Blueprint$new("nonmem")
    bp$template <- tmpl
    bp$add_ignores("BQL.EQN.1")
    expect_equal(bp$render(), "\nIGNORE=(BQL.EQN.1)\n")
    bp$add_ignores("OBSNUM.GT.10")
    expect_equal(bp$render(), "\nIGNORE=(BQL.EQN.1)\nIGNORE=(OBSNUM.GT.10)\n")
    bp$add_ignores("TIME>=10", .overwrite = TRUE)
    bp$render()
  })
  it("can overwrite", {
    bp <- Blueprint$new("nonmem")
    bp$template <- tmpl
    bp$add_ignores("BQL.EQN.1")
    bp$add_ignores("OBSNUM.GT.10")
    bp$add_ignores("TIME>=10", .overwrite = TRUE)
    expect_equal(bp$render(), "\nIGNORE=(TIME>=10)\n")
  })
})
