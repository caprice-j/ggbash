library(ggbash)
context('build-geom')


test_that('build geom', {
    b <- build_geom(mtcars, 'p cyl mpg c=vs si=a shape=18')

    expect_equal(
        b$geomstr,
        'geom_point(aes(x=cyl, y=mpg, colour=vs, size=am), shape=18)'
    )

    expect_true(
        all(b$conf$aes == c("x=cyl", "y=mpg", "colour=vs", "size=am"))
    )

    expect_true(all(b$conf$non_aes == c("shape=18")))
})

test_that('build stat geom', {

})


