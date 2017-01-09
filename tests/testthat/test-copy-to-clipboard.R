library(ggbash)
context("copy-to-clipboard")

test_that("copy_to_clipboard", {
    expect_error(copy_to_clipboard(), regexp = NA)
})
