library(ggbash)
library(futile.logger)
context('ggbash-REPL')

test_that('ggbash', {
    expect_equal(exec_ggbash('exit'), TRUE)
    expect_equal(exec_ggbash('quit'), TRUE)
    expect_equal(exec_ggbash('q'), TRUE)

    expect_output(exec_ggbash('show iris'), 'setosa')

    expect_message(capture.output(exec_ggbash('gg iris + p Sepal.W Sepal.L | echo')), 'geom_point')
    expect_message(exec_ggbash('echo hi'), 'hi')

    expect_equal(exec_ggbash('gg iris + p Sepal.W Sepal.L | copy'), FALSE)
    expect_equal(exec_ggbash('gg iris + p Sepal.W Sepal.L'), FALSE)

    out <- ggbash('gg iris + point Sepal.W Sepal.L + line Sepal.W Sepal.L')
    expect_equal(
        out,
        'ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length)) + geom_line(aes(x=Sepal.Width, y=Sepal.Length))')

    expect_message(
        ggbash('gg iris + point Petal.Width Petal.Length', clipboard = 1),
        'copied to clipboard')

    out <- ggbash('gg mtcars x=mpg y=cyl + point + smooth')
    expect_equal(out, 'ggplot(mtcars, aes(mpg, cyl)) + geom_point() + geom_smooth()')
    # TODO gg mtcars x=mpg y=cyl + point worked,
    # TODO gg mtcars   mpg   cyl + point should work
})
