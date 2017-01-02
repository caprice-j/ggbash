library(ggbash)
context('drawgg-without-functions')

# fixme implement doEval = FALSE
d <- drawgg
p <- function(list) paste(unlist(list), collapse = ' ')
r <- function(str) gsub('ggplot2::', '', str)
s <- split_by_space
et <- expect_true
ee <- expect_equal

test_that("drawgg edge cases", {
    expect_error(drawgg(NULL), 'dataset is not set')
})

test_that("drawgg test by iris", {
    column_i <- d(iris, s('point 1 2'))
    et(p(column_i$conf$aes) == "x=Sepal.Length y=Sepal.Width")
    ee(r(column_i$cmd), 'ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))')

    full_var <- d(iris, s('point Petal.Width Sepal.Length'))
    et(p(full_var$conf$aes) == 'x=Petal.Width y=Sepal.Length')
    ee(r(full_var$cmd), 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Sepal.Length))')

    part_var <- d(iris, s('point Petal.W Petal.L'))
    et(p(part_var$conf$aes) == 'x=Petal.Width y=Petal.Length')
    ee(r(part_var$cmd), 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Petal.Length))')
})

test_that("drawgg test by mtcars", {
    column_i <- d(mtcars, s('point 1 2 color=3'))
    et(p(column_i$conf$aes) == "x=mpg y=cyl colour=disp") # color is converted to colour
    ee(r(column_i$cmd), 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, colour=disp))')

    full_var <- d(mtcars, s('point drat qsec colour=am size=cyl stroke=vs'))
    et(p(full_var$conf$aes) == 'x=drat y=qsec colour=am size=cyl stroke=vs')
    ee(r(full_var$cmd), 'ggplot(mtcars) + geom_point(aes(x=drat, y=qsec, colour=am, size=cyl, stroke=vs))')

    part_var <- d(mtcars, s('p di q c=a si=cy'))
    et(p(part_var$conf$aes) == 'x=disp y=qsec colour=am size=cyl')
    ee(r(part_var$cmd), 'ggplot(mtcars) + geom_point(aes(x=disp, y=qsec, colour=am, size=cyl))')

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

test_that("drawgg non-aes test", {
    non_aes1 <- d(mtcars, 'point  1 2 color="blue"')
    ee(non_aes1$cmd, 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl), colour="blue")')
    ee(p(non_aes1$conf$non_aes), 'colour="blue"')
    # MAYBE-LATER write tests of non_aes1$cmd_verbose after deciding its format

    non_aes2 <- d(mtcars, 'point  1 2 size=carb color="blue" shape="18"')
    ee(non_aes2$cmd,
       'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, size=carb), colour="blue", shape=18)')
    ee(p(non_aes2$conf$non_aes), 'colour="blue" shape=18')
})
