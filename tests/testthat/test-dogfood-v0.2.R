library(ggbash)
library(futile.logger) # could not find appender.finder
context("dogfood-v0.2")

g <- rly::lex(Ggplot2Lexer)
p <- rly::yacc(Ggplot2Parser)
`%+%` <- function(left, right) paste0(left, right)

function() {
    source("~/Dropbox/CU2016/ggbash/R/partial_match_utils.R")
    source("~/Dropbox/CU2016/ggbash/R/ggbash.R")
    source("~/Dropbox/CU2016/ggbash/R/ggplot2-compiler.R")
}

ee <- testthat::expect_equal

test_that("cases", {
    char <-
        "gg iris + point Sepal.W Sepal.L col=Species " %+%
        "+ theme legend.key: colour='black'"
    g$input(char)

    ee(p$parse(char, g),
        "ggplot2::ggplot(iris)" %+%
        " + ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, " %+%
        "y=Sepal.Length, colour=Species)) + ggplot2::theme(" %+%
        "legend.key = ggplot2::element_rect(colour='black'))")

    # spaces between theme element name and its configurations
    expect_error(p$parse("gg iris + point Sepal.W Sepal.L" %+%
                         " + theme text : colour='blue'", g), regexp = NA)

    expect_equal(
        p$parse("gg mtcars + point m cyl + theme axis.ticks: size=1.5", g),
        "ggplot2::ggplot(mtcars) + " %+%
        "ggplot2::geom_point(ggplot2::aes(x=mpg, y=cyl)) + " %+%
        "ggplot2::theme(axis.ticks = ggplot2::element_line(size=1.5))")

})

test_that("cases 2", {
    # fixed adding theme_bw()
    expect_equal(
        p$parse("gg iris + point Sepal.W Sepal.L + theme_bw", g),
        p$parse("gg iris + point Sepal.W Sepal.L + theme bw", g)
    )

    # fixed characters
    g$input("gg iris + point Sepal.W Sepal.L + theme legend.pos: \"none\"")
    g$token(); g$token(); g$token(); g$token(); g$token(); g$token();
    ee(g$token()$value, "\"none\"")
    ee(
        p$parse("gg iris + point Sepal.W Sepal.L " %+%
                "+ theme legend.position: \"none\"", g),
        "ggplot2::ggplot(iris) + " %+%
        "ggplot2::geom_point(ggplot2::aes(x=Sepal.Width," %+%
        " y=Sepal.Length)) + ggplot2::theme(legend.position = \"none\")")

    # fixed logicals
    g$input("gg iris + point Sepal.W Sepal.L + theme panel.ontop: TRUE")
    g$token(); g$token(); g$token(); g$token(); g$token(); g$token();
    ee(g$token()$value, "TRUE")

    for (boolean_input in c("TRUE", "FALSE", "T", "F", "t", "f",
                            "true", "false", "True", "False"))
        g$input(boolean_input); ee(g$token()$type, "BOOLEAN")

    espace <- function(input, expected) {
        prefix <- "ggplot mtcars + p mpg cyl "
        `%+%` <- function(left, right) paste0(left, right)
        l <- rly::lex(Ggplot2Lexer)
        l$input(prefix %+% input)
        l$token(); l$token(); l$token(); l$token();
        ee(l$token()$value, expected)
    }



    # fixed whitespaces for CHARAES

    espace("c=\"red\" ",         "c=\"red\"")
    espace("c =\"red\" ",        "c =\"red\"")        # single space before =
    espace("c        =\"red\" ", "c        =\"red\"") #       spaces before =
    espace("c= \"red\" ",        "c= \"red\"")        # single space after =
    espace("c=        \"red\" ", "c=        \"red\"") #       spaces after =
    espace("c = \"red\" ",       "c = \"red\"")       # both
    espace("c  =  \"red\" ",     "c  =  \"red\"")     # multi and both

    # fixed whitespaces for CONSTAES
    espace("size=5 ",         "size=5")
    espace("size =5 ",        "size =5")        # single space before =
    espace("size        =5 ", "size        =5") #       spaces before =
    espace("size= 5 ",        "size= 5")        # single space after =
    espace("size=        5 ", "size=        5") #       spaces after =
    espace("size = 5 ",       "size = 5")       # both
    espace("size  =  5 ",     "size  =  5")     # multi and both

    # TODO partial match for theme elements
    expected <- "ggplot2::ggplot(iris) + " %+%
        "ggplot2::geom_point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))" %+%
        " + ggplot2::theme(text = ggplot2::element_text(size=3))"
    prefix <- "gg iris + point Sepal.W Sepal.L + "
    ee(p$parse(prefix %+% "theme text: size=3", g), expected)
    ee(p$parse(prefix %+% "theme te:   size=3", g), expected)

    gbash <- function(str)
        rly::yacc(Ggplot2Parser)$parse(str, rly::lex(Ggplot2Lexer))

    # fixed units handling
    g <- rly::lex(Ggplot2Lexer);
    g$input("gg mtcars mpg hp + point + theme axis.ticks.len: .20  cm")
    g$token(); g$token(); g$token(); g$token(); g$token();
    ee(g$token()$value, "axis.ticks.len:")
    ee(g$token()$value, ".20  cm")
    ee(gbash("gg mtcars mpg hp + point + theme axis.ticks.len: .20  cm"),
        "ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp))" %+%
        " + ggplot2::geom_point()" %+%
        " + ggplot2::theme(axis.ticks.length = grid::unit(.20,'cm'))")

    prefix1 <- "gg mtcars mpg hp + p + theme axis.ticks.l: "
    prefix2 <-
        "ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + " %+%
        "ggplot2::geom_point()" %+%
        " + ggplot2::theme(axis.ticks.length = grid::unit(3.5,'in"
    ee(gbash(prefix1 %+% "3.5 inches "), paste0(prefix2, "ches'))"))
    ee(gbash(prefix1 %+% "3.5 inch   "), paste0(prefix2, "ch'))"))
    ee(gbash(prefix1 %+% "3.5 in     "), paste0(prefix2, "'))"))

    # TODO gg mtcars mpg hp + point col=factor(cyl)

    # TODO  theme(legend.position = c(.5, .5))

    # TODO recover from non-existing configuration settings
    # gg iris + point Sepal.W Sepal.L c=Sp + theme legend : angle=45

})
