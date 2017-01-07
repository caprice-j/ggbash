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


