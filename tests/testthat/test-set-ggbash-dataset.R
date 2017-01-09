library(ggbash)
context("set-ggbash-dataset")

test_that("set ggbash dataset", {
    expect_error(set_ggbash_dataset("iris"), regexp = NA)       # data frame
    expect_error(set_ggbash_dataset("euro.cross"), regexp = NA) # matrix
})
