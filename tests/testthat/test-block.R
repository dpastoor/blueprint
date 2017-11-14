context("block")

describe("block() works", {
  it("can be initialized correctly", {
    input <- block(0.4, 0.6, 0.09, param_names = c("CL", "V"), correlation = TRUE)
    output <- structure(list(block = TRUE, params = c("CL", "V"), fixed = FALSE,
                     correlation = TRUE, matrix = structure(c(0.4, 0.113841995766062,
                                                              0.113841995766062, 0.09), .Dim = c(2L, 2L)),
                     value = "\n4.000000e-01 \n1.138420e-01 9.000000e-02",
                     num_params = 2L, comment = NULL), .Names = c("block", "params",
                                                                  "fixed", "correlation",
                                                                  "matrix", "value", "num_params", "comment"
                     ), class = c("block"))
    expect_equal(input, output)
  })
})
