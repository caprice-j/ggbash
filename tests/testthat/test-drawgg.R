library(ggbash)
context('drawgg-without-functions')

s <- split_by_space
# fixme implement doEval = FALSE
d <- drawgg
t <- expect_true
e <- expect_equal
p <- function(list) paste(unlist(list), collapse = ' ')

test_that("drawgg test by iris", {
    column_i <- d(iris, s('point 1 2'))
    t(p(column_i$conf) == "x=Sepal.Length y=Sepal.Width")
    e(column_i$cmd, 'ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))')

    full_var <- d(iris, s('point Petal.Width Sepal.Length'))
    t(p(full_var$conf) == 'x=Petal.Width y=Sepal.Length')
    e(full_var$cmd, 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Sepal.Length))')

    part_var <- d(iris, s('point Petal.W Petal.L'))
    t(p(part_var$conf) == 'x=Petal.Width y=Petal.Length')
    e(part_var$cmd, 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Petal.Length))')
})

test_that("drawgg test by mtcars", {
    column_i <- d(mtcars, s('point 1 2 color=3'))
    t(p(column_i$conf) == "x=mpg y=cyl colour=disp") # color is converted to colour
    e(column_i$cmd, 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, colour=disp))')

    full_var <- d(mtcars, s('point drat qsec colour=am size=cyl stroke=vs'))
    t(p(full_var$conf) == 'x=drat y=qsec colour=am size=cyl stroke=vs')
    e(full_var$cmd, 'ggplot(mtcars) + geom_point(aes(x=drat, y=qsec, colour=am, size=cyl, stroke=vs))')

    part_var <- d(mtcars, s('p d q c=a s=c'))
    t(p(part_var$conf) == 'x=disp y=qsec colour=am size=cyl')
    e(part_var$cmd, 'ggplot(mtcars) + geom_point(aes(x=disp, y=qsec, colour=am, size=cyl))')
})
