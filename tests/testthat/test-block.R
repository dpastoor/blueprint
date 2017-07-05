context("block")

describe("block() works", {
  it("can be initialized correctly", {
    input <- block(0.4, 0.6, 0.09, .params = c("CL", "V"), correlation = TRUE)
    expect_equal(input, list(type = "block", params = c("CL", "V"), correlation = TRUE, fix = FALSE,
                  values = structure(c(0.4,
                                       0.113841995766062,
                                       0.113841995766062,
                                       0.09),
                                     .Dim = c(2L,
                                            2L))
                  )
                 )
  })
})
