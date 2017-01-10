library(ggbash)
context("dogfood-v0.3")

test_that("dogfood v0.3", {
    expect_error(ggbash("gg iris + text Sepal.W Sepal.L"))
    # gg iris + text Sepal.W Sepal.L Species col=Spe angle=45 + theme cl
    ggbash(" gg iris + text Sepal.W Sepal.L Sp check_overlap=TRUE")

    ee(gbash(" gg iris + text Sepal.W Sepal.L Sp check_overlap=TRUE"),
       "ggplot2::ggplot(iris) + ggplot2::geom_text(ggplot2::aes(" %+%
           "x=Sepal.Width, y=Sepal.Length, label=Species), check_overlap=TRUE)")

    ee(gbash(" gg iris + text Sepal.W Sepal.L Sp che=TRUE"),
       "ggplot2::ggplot(iris) + ggplot2::geom_text(ggplot2::aes(" %+%
           "x=Sepal.Width, y=Sepal.Length, label=Species), check_overlap=TRUE)")

    expect_match(gbash(" gg iris + text Sepal.W Sepal.L Sp chk=TRUE"),
                 "INVALID_TOKEN_HERE")

    ggbash("gg iris + point Sepal.L Sepal.W col=Sp siz=Petal.W + theme legend.pos: \"bottom\" axis.text: face=\"bold\"")

    lex$input("gg iris + point Sepal.L Sepal.W col=Sp siz=Petal.W + theme legend.pos: \"bottom\" axis.text: face=\"bold\"")

})
