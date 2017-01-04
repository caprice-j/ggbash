library(ggbash)
context('drawgg-without-functions')

# fixme implement doEval = FALSE
b <- build_geom
#d <- drawgg
l <- function(ds_str, ...) lapply(..., function(x) build_geom(set_ggbash_dataset(ds_str), x))
d <- function(ds_str, ...) drawgg(set_ggbash_dataset(ds_str), l(ds_str, ...))
r <- function(str) gsub('ggplot2::', '', str)
et <- expect_true
ee <- expect_equal

test_that("drawgg edge cases", {
    expect_equal(drawgg(NULL), NULL)
})

test_that("drawgg test by iris", {
    column_i <- d('iris', 'point Sepal.L Sepal.W')
    ee(r(column_i$cmd), 'ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width))')

    full_var <- d('iris', 'point Petal.Width Sepal.Length')
    ee(r(full_var$cmd), 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Sepal.Length))')

    part_var <- d('iris', 'point Petal.W Petal.L')
    ee(r(part_var$cmd), 'ggplot(iris) + geom_point(aes(x=Petal.Width, y=Petal.Length))')
})

test_that("drawgg test by mtcars", {
    column_i <- d('mtcars', 'point mpg cyl color=disp')
    ee(r(column_i$cmd), 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, colour=disp))')

    full_var <- d('mtcars', 'point drat qsec colour=am size=cyl')
    ee(r(full_var$cmd), 'ggplot(mtcars) + geom_point(aes(x=drat, y=qsec, colour=am, size=cyl))')

    part_var <- d('mtcars', 'p di q c=a si=cy')
    ee(r(part_var$cmd), 'ggplot(mtcars) + geom_point(aes(x=disp, y=qsec, colour=am, size=cyl))')

    geom_exceptions <- function(){
        d('mtcars', 'p     mpg cyl')
        d('mtcars', 'po    mpg cyl')
        d('mtcars', 'poi   mpg cyl')
        d('mtcars', 'poin  mpg cyl')
        d('mtcars', 'point mpg cyl')
        d('mtcars', 'l     mpg cyl')
        d('mtcars', 'li    mpg cyl')
        d('mtcars', 'lin   mpg cyl')
        d('mtcars', 'line  mpg cyl')
    }

    expect_warning(geom_exceptions(), regexp = NA) # no warning expected

    expect_error(d('mtcars', 'xyz  mpg cyl'), 'no such prefix: xyz')
})

test_that("drawgg non-aes test", {
    non_aes1 <- d('mtcars', 'point  mpg cyl color="blue"')
    ee(non_aes1$cmd, 'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl), colour="blue")')
    # MAYBE-LATER write tests of non_aes1$cmd_verbose after deciding its format

    non_aes2 <- d('mtcars', 'point  mpg cyl size=carb color="blue" shape=18')
    ee(non_aes2$cmd,
       'ggplot(mtcars) + geom_point(aes(x=mpg, y=cyl, size=carb), colour="blue", shape=18)')
})
