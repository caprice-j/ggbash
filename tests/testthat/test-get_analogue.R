library(ggbash)
context("get_analogue")

ee <- testthat::expect_equal

test_that("use case 1", {
    possibilities <- c("axis.text", "axis.text.x", "axis.ticks",
                       "legend", "text")
    a <- get_analogue("axs.t", possibilities)
    ee(a[1], "axis.text")
    ee(a[2], "text")
    ee(a[3], "axis.ticks")
    ee(a[4], "axis.text.x")
    ee(a[5], "legend")

})
