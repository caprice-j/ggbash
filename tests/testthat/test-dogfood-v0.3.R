library(ggbash)
context("dogfood-v0.3")

test_that("dogfood v0.3", {
    expect_error(print(ggbash("gg iris + text Sepal.W Sepal.L")))

    ee(bash(" gg iris + text Sepal.W Sepal.L Sp check_overlap=TRUE"),
       paste0("ggplot(iris) + geom_text(aes(x=Sepal.Width, y=Sepal.Length,",
           " label=Species), check_overlap=TRUE)"))

    ee(
        bash(" gg iris + text Sepal.W Sepal.L Sp chk=TRUE"),
        "ggplot(iris) + geom_text(aes(x=Sepal.Width, " %++%
            "y=Sepal.Length, label=Species), check_overlap=TRUE)")

    # ee(
    # gbash("gg iris + point Sepal.W Sepal.L + theme legend.position \"none\""),
    #     "ggplot2::ggplot(iris) + " %++%
    #         "ggplot2::geom_point(ggplot2::aes(x=Sepal.Width," %++%
    #         " y=Sepal.Length)) + ggplot2::theme(legend.position = \"none\")")

    ee(
        bash("g iris + p Sepal.W Sepal.L c=Sp sz=7 + theme txt sz=20 f=\"bold\""),
        "ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, " %++%
            "colour=Species), size=7) + " %++%
            "theme(text = element_text(size=20, face=\"bold\"))"
        )
    # MAYBE-LATER bash("gg iris Sepal.W Sepal.L + poitn")
})
