context("available_models")

describe("available_models works", {
  it("it gives nonmem and mrgsolve models", {
   models_nonmem <- available_models("nonmem")
   models_mrgsolve <- available_models("nonmem")
   expect_true("one_cmt_iv" %in% names(models_nonmem))
   expect_true("one_cmt_iv" %in% names(models_mrgsolve))
  })
  it("can be code completed since its a list", {
    expect_equal(models_nonmem$one_cmt_iv, "ADVAN1 TRANS2")
  })
  it("errors given an unknown type", {
   expect_error(available_nonmem("misspelled"))
  })
})
