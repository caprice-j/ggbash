library(ggbash)
context('drawgg-without-functions')

# fixme implement doEval = FALSE
d <- drawgg
p <- function(list) paste(unlist(list), collapse = ' ')
s <- split_by_space
et <- expect_true
ee <- expect_equal

test_that("drawgg edge cases", {
    expect_error(drawgg(NULL), 'dataset is not set')
})

test_that("drawgg test by iris", {
    column_i <- d(iris, s('point 1 2'))
    et(p(column_i$conf) == "x=Sepal.Length y=Sepal.Width")
    ee(column_i$cmd, 'ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))')

    full_var <- d(iris, s('point Petal.Width Sepal.Length'))
    et(p(full_var$conf) == 'x=Petal.Width y=Sepal.Length')
    ee(full_var$cmd, 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Sepal.Length))')

    part_var <- d(iris, s('point Petal.W Petal.L'))
    et(p(part_var$conf) == 'x=Petal.Width y=Petal.Length')
    ee(part_var$cmd, 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Petal.Length))')
})

test_that("drawgg test by mtcars", {
    column_i <- d(mtcars, s('point 1 2 color=3'))
    et(p(column_i$conf) == "x=mpg y=cyl colour=disp") # color is converted to colour
    ee(column_i$cmd, 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, colour=disp))')

    full_var <- d(mtcars, s('point drat qsec colour=am size=cyl stroke=vs'))
    et(p(full_var$conf) == 'x=drat y=qsec colour=am size=cyl stroke=vs')
    ee(full_var$cmd, 'ggplot(mtcars) + geom_point(aes(x=drat, y=qsec, colour=am, size=cyl, stroke=vs))')

    part_var <- d(mtcars, s('p di q c=a si=cy'))
    et(p(part_var$conf) == 'x=disp y=qsec colour=am size=cyl')
    ee(part_var$cmd, 'ggplot(mtcars) + geom_point(aes(x=disp, y=qsec, colour=am, size=cyl))')

    geom_exceptions <- function(){
        d(mtcars, s('p     1 2'))
        d(mtcars, s('po    1 2'))
        d(mtcars, s('poi   1 2'))
        d(mtcars, s('poin  1 2'))
        d(mtcars, s('point 1 2'))
        d(mtcars, s('l     1 2'))
        d(mtcars, s('li    1 2'))
        d(mtcars, s('lin   1 2'))
        d(mtcars, s('line  1 2'))
    }

    expect_warning(geom_exceptions(), regexp = NA) # no warning expected

    expect_error(d(mtcars, s('xyz  1 2')), 'no such prefix: xyz')
})
