library(ggbash)
library(futile.logger) # could not find appender.finder
context('dogfood-v0.2')

g <- rly::lex(module=Ggplot2Lexer)
p <- rly::yacc(Ggplot2Parser)

function() {
    source('~/Dropbox/CU2016/ggbash/R/partial_match_utils.R')
    source('~/Dropbox/CU2016/ggbash/R/ggbash.R')
    source('~/Dropbox/CU2016/ggbash/R/ggplot2-compiler.R')
}

ee <- testthat::expect_equal

test_that('cases', {
    char <- 'gg iris + point Sepal.W Sepal.L col=Species + theme legend.key: colour="black"'
    g$input(char)

    ee(p$parse(char, g),
    "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length, colour=Species)) + ggplot2::theme(legend.key = ggplot2::element_rect(colour=\"black\"))")

    # spaces between theme element name and its configurations
    expect_error(p$parse('gg iris + point Sepal.W Sepal.L + theme text : colour="blue"', g), regexp=NA)

    expect_equal(p$parse('gg mtcars + point m cyl + theme axis.ticks: size=1.5', g),
                 "ggplot2::ggplot(mtcars) + ggplot2::geom_point(ggplot2::aes(x=mpg, y=cyl)) + ggplot2::theme(axis.ticks = ggplot2::element_line(size=1.5))")

})

test_that('cases 2', {
    # TODO add theme_bw()
    # gg iris + point Sepal.W Sepal.L col=Species size=5 + theme text: size=20 family="Baskerville" face="bold" + theme_bw()
    expect_equal(
        p$parse('gg iris + point Sepal.W Sepal.L + theme_bw', g),
        p$parse('gg iris + point Sepal.W Sepal.L + theme bw', g)
    )

    # TODO how do we deal with units?
    #  + theme(axis.ticks.length = unit(.85, "cm"))

    # fixed characters
    g$input('gg iris + point Sepal.W Sepal.L + theme legend.position: "none"')
    g$token(); g$token(); g$token(); g$token(); g$token(); g$token();
    ee(g$token()$value, '"none"')
    ee(p$parse('gg iris + point Sepal.W Sepal.L + theme legend.position: "none"', g),
       'ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(legend.position = \"none\")')

    # fixed logicals
    g$input('gg iris + point Sepal.W Sepal.L + theme panel.ontop: TRUE')
    g$token(); g$token(); g$token(); g$token(); g$token(); g$token();
    ee(g$token()$value, 'TRUE')

    for (boolean_input in c('TRUE','FALSE','T','F','t','f','true','false','True','False'))
        g$input(boolean_input); ee(g$token()$type, 'BOOLEAN')

    espace <- function(input, expected) {
        l <- rly::lex(module=Ggplot2Lexer)
        l$input(input)
        l$token(); l$token(); l$token(); l$token();
        ee(l$token()$value, expected)
    }

    # fixed whitespaces for CHARAES
    espace('ggplot mtcars + p mpg cyl c="red" ',         'c="red"')
    espace('ggplot mtcars + p mpg cyl c ="red" ',        'c ="red"')        # single space before =
    espace('ggplot mtcars + p mpg cyl c        ="red" ', 'c        ="red"') #       spaces before =
    espace('ggplot mtcars + p mpg cyl c= "red" ',        'c= "red"')        # single space after =
    espace('ggplot mtcars + p mpg cyl c=        "red" ', 'c=        "red"') #       spaces after =
    espace('ggplot mtcars + p mpg cyl c = "red" ',       'c = "red"') # both
    espace('ggplot mtcars + p mpg cyl c  =  "red" ',     'c  =  "red"') # multi-both

    # fixed whitespaces for CONSTAES
    espace('ggplot mtcars + p mpg cyl size=5 ',         'size=5')
    espace('ggplot mtcars + p mpg cyl size =5 ',        'size =5')        # single space before =
    espace('ggplot mtcars + p mpg cyl size        =5 ', 'size        =5') #       spaces before =
    espace('ggplot mtcars + p mpg cyl size= 5 ',        'size= 5')        # single space after =
    espace('ggplot mtcars + p mpg cyl size=        5 ', 'size=        5') #       spaces after =
    espace('ggplot mtcars + p mpg cyl size = 5 ',       'size = 5') # both
    espace('ggplot mtcars + p mpg cyl size  =  5 ',     'size  =  5') # multi-both

    # TODO partial match for theme elements

    # TODO  theme(legend.position = c(.5, .5))

    # TODO recover from non-existing configuration settings
    # gg iris + point Sepal.W Sepal.L c=Sp + theme legend : angle=45

})
