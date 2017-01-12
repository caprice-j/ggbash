library(ggbash)
context("dogfood-v0.3")

test_that("dogfood v0.3", {
    expect_error(ggbash("gg iris + text Sepal.W Sepal.L"))

    ee(gbash(" gg iris + text Sepal.W Sepal.L Sp check_overlap=TRUE"),
       "ggplot2::ggplot(iris) + ggplot2::geom_text(ggplot2::aes(" %+%
           "x=Sepal.Width, y=Sepal.Length, label=Species), check_overlap=TRUE)")

    ee(gbash(" gg iris + text Sepal.W Sepal.L Sp che=TRUE"),
       "ggplot2::ggplot(iris) + ggplot2::geom_text(ggplot2::aes(" %+%
           "x=Sepal.Width, y=Sepal.Length, label=Species), check_overlap=TRUE)")

    ee(
        bash(" gg iris + text Sepal.W Sepal.L Sp chk=TRUE"),
        "ggplot(iris) + geom_text(aes(x=Sepal.Width, " %+%
            "y=Sepal.Length, label=Species), check_overlap=TRUE)")

    # ee(
    # gbash("gg iris + point Sepal.W Sepal.L + theme legend.position: \"none\""),
    #     "ggplot2::ggplot(iris) + " %+%
    #         "ggplot2::geom_point(ggplot2::aes(x=Sepal.Width," %+%
    #         " y=Sepal.Length)) + ggplot2::theme(legend.position = \"none\")")

    # MAYBE-LATER bash("gg iris Sepal.W Sepal.L + poitn")
})
