context("test-class-manip.R")

describe("classes can be prepended", {
  x <- 1
  class(x) <- "test_class"

  it("can prepend a single class", {
    expect_equal(class(prepend_classes(x, "n1")), c("n1", "test_class"))
  })
  it("can prepend multiple classes", {
    expect_equal(class(prepend_classes(x, c("n1", "n2"))), c("n1", "n2", "test_class"))
  })
  it("fails if not given correct inputs", {
    expect_error(prepend_classes(x, list("n1", "n2")),
    "new classes must be a character vector, not: list"
    )
  })
})

describe("classes can be appended", {
  x <- 1
  class(x) <- "test_class"

  it("can append a single class", {
    expect_equal(class(append_classes(x, "n1")), c("test_class", "n1"))
  })
  it("can append multiple classes", {
    expect_equal(class(append_classes(x, c("n1", "n2"))), c("test_class", "n1", "n2"))
  })
  it("fails if not given correct inputs", {
    expect_error(append_classes(x, list("n1", "n2")),
    "new classes must be a character vector, not: list"
    )
  })
})
