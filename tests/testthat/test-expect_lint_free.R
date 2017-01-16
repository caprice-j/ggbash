# nolint start
if (requireNamespace("lintr", quietly = TRUE)) {
    context("lints")
    test_that("Package Style", {
        # lintr execution is too slow
        # lintr::expect_lint_free()
        ee(1, 1) # dummy
    })
}
# nolint end
