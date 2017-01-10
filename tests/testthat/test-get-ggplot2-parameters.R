context("ggplot2-parameters")

test_that("parameters", {

    ee(length(get_stat_params("dotplot")), 9)
    ee(length(get_stat_params("histogram")), 4)
    ee(length(get_stat_params("smooth")), 4)
    ee(length(get_stat_params("violin")), 3)

})
