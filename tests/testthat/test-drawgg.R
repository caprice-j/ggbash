library(ggbash)
context('drawgg')

test_that("drawgg test by iris", {
    s <- split_by_space
    d <- drawgg
    t <- expect_true
    e <- expect_equal
    p <- function(list) paste(unlist(list), collapse = ' ')

    column_i <- d(iris, s('point 1 2'))
    t(p(column_i$conf) == "x=Sepal.Length y=Sepal.Width")
    e(column_i$cmd, 'ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))')

    full_var <- d(iris, s('point Petal.Width Sepal.Length'))
    t(p(full_var$conf) == 'x=Petal.Width y=Sepal.Length')
    e(full_var$cmd, 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Sepal.Length))')

})
