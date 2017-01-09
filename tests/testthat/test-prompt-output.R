library(ggbash)
context("prompt")

test_that("output", {
    e_mtcars <- function(m)
        expect_message(show_dataset_column_indices("mtcars"), m)
    expect_equal(show_dataset_column_indices(NULL), NULL)

    e_mtcars(" 1: mpg \t 6: wt  \t11: carb\t\n")
    e_mtcars(" 2: cyl \t 7: qsec\t\n")
    e_mtcars(" 3: disp\t 8: vs  \t\n")
    e_mtcars(" 4: hp  \t 9: am  \t\n")
    e_mtcars(" 5: drat\t10: gear\t")

    assign("ggbash_test_df",
           value = data.frame(matrix(rep(1, 200), ncol = 100)),
           envir = .GlobalEnv) # failed without envir = .GlobalEnv
    expect_message(
        show_dataset_column_indices("ggbash_test_df"),
        paste0("  1: X1  \t 16: X16 \t 31: X31 \t 46: X46",
                " \t 61: X61 \t 76: X76 \t 91: X91")
    )
})
