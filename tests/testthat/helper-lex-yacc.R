
# This file will be executed before other test-* files.

`%+%` <- function(left, right) paste0(left, right)
`%++%` <- function(left, right) paste0(left, right)

ee <- testthat::expect_equal
en <- function(...) testthat::expect_error(..., regexp = NA)

# ggbash(gg(iris) + point(Sepal.W, Sepal.L))
# ggbash("gg(iris) + point(Sepal.W, Sepal.L)")
# ggbash("gg(iris) " %+% "msoe")
