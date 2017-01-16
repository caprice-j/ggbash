
# This file will be executed before other test-* files.

lex  <- rly::lex(Ggplot2Lexer)
yacc <- rly::yacc(Ggplot2Parser)

gbash <- function(ggbash_symbols) {
    is_string <- tryCatch(class(ggbash_symbols) == "character",
                          error = function(err) {FALSE})
    if (is_string) {
        cmd <- ggbash_symbols
    } else {
        raw_cmd <- deparse(substitute(ggbash_symbols),
                           width.cutoff = 500) # arbitrary large
        cmd <- raw_cmd
    }
    compile_ggbash(cmd)
}

# FIXME duplicate
bash <- function(ggbash_symbols) {
    is_string <- tryCatch(class(ggbash_symbols) == "character",
                          error = function(err) {FALSE})
    if (is_string) {
        cmd <- ggbash_symbols
    } else {
        raw_cmd <- deparse(substitute(ggbash_symbols),
                           width.cutoff = 500) # arbitrary large
        cmd <- raw_cmd
    }
    gsub("ggplot2::", "", compile_ggbash(cmd))
}




`%+%` <- function(left, right) paste0(left, right)
`%++%` <- function(left, right) paste0(left, right)

ee <- testthat::expect_equal
en <- function(...) testthat::expect_error(..., regexp = NA)

# ggbash(gg(iris) + point(Sepal.W, Sepal.L))
# ggbash("gg(iris) + point(Sepal.W, Sepal.L)")
# ggbash("gg(iris) " %+% "msoe")
