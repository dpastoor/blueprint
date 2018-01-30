purrr::map(c("value", "lower_bound", "upper_bound", "link", "name", "fixed", "covariate_relationships"), function(.fun) {
glue::glue('<<.fun>>.parameter <- function (x) {
     x$<<.fun>>
}
  ', .open = "<<", .close = ">>")
})


test_param <- parameter(10)

value(test_param)

purrr::map(1:3, function(.x) {
  value(test_param) <- value(test_param) + .x
})
value(test_param) <-  11
test_param
test_param$value <- 10

update(test_param, value = 12, lower_bound = 0)
