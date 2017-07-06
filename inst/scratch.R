blueprint <- Blueprint$new("mrgsolve")

blueprint$add_params(CL = 5, V = 34)
blueprint$add_constants(STD_WT = 70)
blueprint$add_heirarchy(b1 = block(0.04, 0.61, 0.09, .params = c("V", "CL"), correlation = TRUE))
blueprint$add_residual_error(prop = sigma_param(0.04, .comment = "Proportional Error"))

blueprint$get_all_elements()

blueprint$data <- head(Theoph)
blueprint$datapath <- "path/to/my/data.csv"
tmplt <- load_templates("mrgsolve")
blueprint$template <- tmplt$one_cmt_iv
#blueprint$template <- tmplt$`1-cmt-iv`

cat(blueprint$render())

