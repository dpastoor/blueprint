context("test-hooks.R")


describe("hooks works()", {
bp <- Blueprint$new("nonmem")
bp$template <- "
{{hooks:pk:begin}}
"

  it("a basic hook can be injected", {
    bpt <- bp$clone()
    bpt$add_hooks("pk:begin" = "injected")
    expect_equal(bpt$render(), "\ninjected\n")
  })

  it("can override existing hooks", {
    bpt <- bp$clone()
    bpt$add_hooks("pk:begin" = "injected")
    bpt$add_hooks("hooks:pk:begin" = "injected2")
    expect_equal(bpt$render(), "\ninjected2\n")
  })
})
