context("block")

describe("block() works", {
  it("can be initialized correctly", {
    input <- block(0.4, 0.6, 0.09, param_names = c("CL", "V"), correlation = TRUE)
    expect_equal(input, list(block = TRUE,
                             params = c("CL", "V"),
                             fix = FALSE,
                             correlation = TRUE,
                  matrix = structure(c(0.4,
                                       0.113841995766062,
                                       0.113841995766062,
                                       0.09),
                                     .Dim = c(2L,
                                            2L)),
                  value = stringify_matrix(structure(c(0.4,
                                                        0.113841995766062,
                                                        0.113841995766062,
                                                        0.09),
                                                      .Dim = c(2L,
                                                               2L))),
                  num_params = 2,
                  comment = NULL
                  )
                 )
  })
})
