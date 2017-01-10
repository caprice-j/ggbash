context("ggplot2-docs")

# nolint start
test_that("geom_abline", {
    ee(bash("gg mtcars wt mpg + point + smooth method='lm' se=FALSE"),
       "ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method='lm', se=FALSE)")
})
# nolint end
