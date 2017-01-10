library(ggbash)
context("find-first-partial-match")

test_that("find_first", {

    f <- find_first

    expect_equal(f("a", c(     "b", "c")), NULL)
    expect_equal(f("a", c("a", "b", "c")), 1)
    expect_message(f("a", c("a", "aa", "c")), "Ambiguous match.")

})
