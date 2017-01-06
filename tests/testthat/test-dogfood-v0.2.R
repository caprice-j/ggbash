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

test_that('case1', {
    char <- 'gg iris + point Sepal.W Sepal.L col=Species + theme legend.key: colour="black"'
    g$input(char)

    ee(p$parse(char, g),
    "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length, colour=Species)) + ggplot2::theme(legend.key = ggplot2::element_rect(colour=\"black\"))")
})
