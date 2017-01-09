
# This file will be executed before other test-* files.

lex  <- rly::lex(Ggplot2Lexer)
yacc <- rly::yacc(Ggplot2Parser)

gbash <- function(str)
    rly::yacc(Ggplot2Parser)$parse(str, rly::lex(Ggplot2Lexer))

`%+%` <- function(left, right) paste0(left, right)

ee <- testthat::expect_equal
