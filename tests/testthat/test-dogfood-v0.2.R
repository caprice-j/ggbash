library(ggbash)
library(futile.logger) # could not find appender.finder
context('dogfood-v0.2')

g <- rly::lex(module=Ggplot2Lexer)
p <- rly::yacc(Ggplot2Parser)

function() {
    source('~/Dropbox/CU2016/ggbash/R/partial_match_utils.R')
    source('~/Dropbox/CU2016/ggbash/R/ggbash.R')
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


    # TODO how do we deal with units?
    #  + theme(axis.ticks.length = unit(.85, "cm"))

    # TODO characters and logicals
    # gg iris + point Sepal.W Sepal.L + theme axis.ticks: size=1.5 legend.position: "none"

    # TODO partial match for theme elements

    # TODO  theme(legend.position = c(.5, .5))

    # TODO recover from non-existing configuration settings
    # gg iris + point Sepal.W Sepal.L c=Sp + theme legend : angle=45

    # TODO whitespaces
    # ggplot iris +      p       Sepal.W       Sepal.L      c     ="red" s   =5
})
