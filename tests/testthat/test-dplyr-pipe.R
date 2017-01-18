context("dplyr/tidyr")

test_that("dplyr-tidyr", {

    ee(rm_piped_dataset("ggplot(ggbash_piped, x=3, y=4)"), "ggplot(x=3, y=4)")
    ee(rm_piped_dataset("ggplot(x=3, y=4)"), "ggplot(x=3, y=4)")

    ee(add_piped_dataset("ggplot(x=Sepal.W, y=Sepal.L)"),
       "ggplot(ggbash_piped, x=Sepal.W, y=Sepal.L)")

    expect_error(iris %>% ggbash(gg() + point(Sepal.W, Sepal.L)), regexp = NA)

    expect_error(
        iris %>%
            dplyr::mutate(newcolumn_please_partial_match_me = Sepal.Width-2) %>%
            ggbash(gg() + point(newcolumn, Sepal.L)),
        regexp = NA
    )

})
