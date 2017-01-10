library(ggbash)
context("string-manipulation")

test_that("splitting", {
    expect_equal(
        split_by_pipe(input = "gg iris | point 1 2 color=3 | copy"),
        c("gg iris ", " point 1 2 color=3 ", " copy")
    )

    expect_equal(
        split_by_space(input = "point 1 2 color=3"),
        c("point", "1", "2", "color=3")
    )

    expect_equal(
        split_by_space(input = "  point 1 2 color=3   "),
        c("point", "1", "2", "color=3")
    )
})

test_that("partial-match", {
    partial_unique_name <- function(strv, i) names(partial_unique(strv, i))

    expect_equal(
        unlist(partial_unique(colnames(iris), i = 100), use.names = FALSE),
        c("Sepal.Length", "Sepal.Width",
          "Petal.Length", "Petal.Width", "Species")
    )

    expect_equal(
        partial_unique_name(colnames(iris), i = 2),
        c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W", "Sp")
    )

    expect_equal(
        partial_unique_name(colnames(iris), i = 3),
        c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W", "Spe")
    )
})

test_that("parse_ggbash_non_aes", {
    ee(parse_ggbash_non_aes("xinter=5", c("xintercept", "linetype", "size")),
       "xintercept=5")
    ee(parse_ggbash_non_aes("xinter = 5", c("xintercept", "linetype", "size")),
       "xintercept=5")
})
