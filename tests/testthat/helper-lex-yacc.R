
lex  <- rly::lex(Ggplot2Lexer)
yacc <- rly::yacc(Ggplot2Parser)

gbash <- function(str)
    rly::yacc(Ggplot2Parser)$parse(str, rly::lex(Ggplot2Lexer))

`%+%` <- function(left, right) paste0(left, right)

print("echocho\n\n")


ee <- testthat::expect_equal
