# capture named dots as a named list
dots <- function (...)
{
  rlang::eval_bare(substitute(alist(...)))
}
