library(ggbash)
context('dogfood-v0.3')

test_that('dogfood', {
    expect_error(ggbash("gg iris + text Sepal.W Sepal.L"))
    # TODO gg iris + text Sepal.W Sepal.L Species col=Spe angle=45 check_overlap=TRUE
    # geom_text()$geom_params
    # gg iris + text Sepal.W Sepal.L Species col=Spe angle=45 + theme cl
})
