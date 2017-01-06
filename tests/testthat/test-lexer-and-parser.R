library(ggbash)
library(futile.logger)
context('ggbash-lexer-and-perser')

glex  <- rly::lex(module=Ggplot2Lexer)

ee <- testthat::expect_equal

test_that('ggplot2 lexer', {

    glex$input('gg iris + point abc def + smooth ghi jkl')
    out <- glex$token()
    ee(out$type,  'GGPLOT')
    ee(out$value, 'ggplot2::ggplot(iris')
    ee(out$lineno, 1)
    ee(out$lexpos, 1)
    ee(glex$token()$value, ' + geom_point')
    ee(glex$token()$value, 'abc')
    ee(glex$token()$value, 'def')
    ee(glex$token()$value, ' + geom_smooth')
    # Note: improper regex for GGPLOT sometimes
    #       results in ggplot(hi (all characters before g'hi' are replaced)
    ee(glex$token()$value, 'ghi')
    ee(glex$token()$value, 'jkl')
    ee(glex$token()$value, NULL)

    glex$input('gg mtcars + point cyl mpg colour="blue" size=4 + smooth colour="blue"')
    ee(glex$token()$value, 'ggplot2::ggplot(mtcars')
    ee(glex$token()$value, ' + geom_point')
    ee(glex$token()$value, 'cyl')
    ee(glex$token()$value, 'mpg')
    ee(glex$token()$value, 'colour=\"blue\"')
    ee(glex$token()$value, 'size=4')
    ee(glex$token()$value, ' + geom_smooth')
    ee(glex$token()$value, 'colour="blue"')
    ee(glex$token()$value, NULL)
})

test_that('ggplot2 parser', {
    prsr <- rly::yacc(Ggplot2Parser)
    ee(prsr$parse('gg iris', glex), 'ggplot2::ggplot(iris)')
    ee(prsr$parse('gg iris Sepal.Width', glex), 'ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width))')
    ee(prsr$parse('gg iris Sepal.Width Sepal.Length', glex),
       'ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length))')
    ee(prsr$parse('gg iris Sepal.Width Sepal.Length + point', glex),
       'ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length)) + ggplot2::geom_point()')
    # FIXME how should I deal with non-existing column names?
    ee(prsr$parse('gg iris SepalWidth + point', glex),
       'ggplot2::ggplot(iris, ggplot2::aes()) + ggplot2::geom_point()')

    ee(prsr$parse('gg iris + point', glex), 'ggplot2::ggplot(iris) + ggplot2::geom_point()')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))')
    #ee(prsr$parse('gg iris + rect Sepal.W Sepal.L', glex),
    #   "ggplot2::ggplot(iris) + ggplot2::geom_rect(ggplot2::aes(ymin=Sepal.Width, ymax=Sepal.Length))") # FIXME only last 2
    ee(prsr$parse('gg iris + rect Sepal.W Sepal.L Petal.L Petal.W', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_rect(ggplot2::aes(xmin=Sepal.Width, xmax=Sepal.Length, ymin=Petal.Length, ymax=Petal.Width))')

    ee(prsr$parse('gg iris + point Sepal.W Sepal.L + smooth', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + ggplot2::geom_smooth()')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W', glex),
        'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + ggplot2::geom_smooth(ggplot2::aes(y=Sepal.Width))')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W Sepal.L', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + ggplot2::geom_smooth(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))'
    )

    ee(prsr$parse('gg iris + point Sepal.W Sepal.L colour="blue"', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length), colour="blue")')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L colour="blue" size=4', glex),
        'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length), colour="blue", size=4)')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L colour="blue" size=4 + smooth Sepal.W Sepal.L', glex),
        'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length), colour="blue", size=4) + ggplot2::geom_smooth(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))')
    ee(prsr$parse('gg iris + point Sepal.W Sepal.L colour="blue" size=4 + smooth colour="blue"', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length), colour="blue", size=4) + ggplot2::geom_smooth(colour="blue")')
    ee(prsr$parse('gg iris + p Sepal.W Sepal.L c="blue" si=4 + sm c="blue"', glex),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length), colour="blue", size=4) + ggplot2::geom_smooth(colour="blue")')

    ee(prsr$parse('gg iris Sepal.W Sepal.L + geom_point + geom_smooth', glex),
       'ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length)) + ggplot2::geom_point() + ggplot2::geom_smooth()')
})
