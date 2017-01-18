context("dplyr/tidyr")

test_that("dplyr-tidyr", {
    expect_error(iris %>% ggbash(gg() + point(Sepal.W, Sepal.L)), regexp = NA)

    expect_error(
        iris %>%
            dplyr::mutate(newcolumn_please_partial_match_me = Sepal.Width-2) %>%
            ggbash(gg() + point(newcolumn, Sepal.L)),
        regexp = NA
    )

})
