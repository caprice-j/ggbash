context("parse-after-equal")

test_that("parse-after", {

    cv <- colnames(iris)
    ee(parse_after_equal("Sepal.W", cv, TRUE), "Sepal.Width")
    ee(parse_after_equal("1 + Sepal.W", cv, TRUE), "1 + Sepal.Width")
    ee(parse_after_equal("paste0('length is :', Sepal.W)", cv, TRUE),
       "paste0('length is :', Sepal.Width)")
    ee(parse_after_equal("Sepal.W %>% as.character %>% as.factor", cv, TRUE),
       "Sepal.Width %>% as.character %>% as.factor")
    ee(parse_after_equal("1", cv, TRUE), "1")
    ee(parse_after_equal("..density..", cv, TRUE), "..density..")

})
