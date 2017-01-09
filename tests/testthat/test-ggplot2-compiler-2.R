library(ggbash)
library(futile.logger)
context("ggbash-compiler-2")

test_that("ggplot2 prefix match for theme element's configuration", {
    gbash("gg iris + point Sepal.W Sepal.L + theme text: siz=1")
})

