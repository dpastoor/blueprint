context("accept:nonmem")


describe("accept statements are generated for nonmem", {
  tmpl <- "
{{#accept}}
IGNORE=({{{.}}})
{{/accept}}
"
  it("can construct a full accept statement", {
    bp <- Blueprint$new("nonmem")
    bp$template <- tmpl
    bp$add_accepts("BQL.EQN.1")
    expect_equal(bp$render(), "\nACCEPT=(BQL.EQN.1)\n")
    bp$add_accepts("OBSNUM.GT.10")
    expect_equal(bp$render(), "\nACCEPT=(BQL.EQN.1)\nACCEPT=(OBSNUM.GT.10)\n")
    bp$add_accepts("TIME>=10", .overwrite = TRUE)
    bp$render()
  })

  it("can overwrite", {
    bp <- Blueprint$new("nonmem")
    bp$template <- tmpl
    bp$add_accepts("BQL.EQN.1")
    bp$add_accepts("OBSNUM.GT.10")
    bp$add_accepts("TIME>=10", .overwrite = TRUE)
    expect_equal(bp$render(), "\nACCEPT=(TIME>=10)\n")
  })
})
