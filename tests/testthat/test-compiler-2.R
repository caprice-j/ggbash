library(ggbash)
library(futile.logger)
context("ggbash-compiler-2")

test_that("ggplot2 prefix match for theme element's configuration", {
    ee(gbash("gg iris # point Sepal.W Sepal.L # theme text size=1"),
       "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(" %++%
           "x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(text = " %++%
           "ggplot2::element_text(size=1))"
    )

        ee(gbash("gg iris # point Sepal.W Sepal.L # theme text siz=1"),
       "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(" %++%
       "x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(text = " %++%
       "ggplot2::element_text(size=1))"
    )

    ee(bash("gg iris # point Sepal.W Sepal.L # theme text vj=5.5"),
       "ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length))" %++%
        " + theme(text = element_text(vjust=5.5))"
    )

    expect_message(
        suppressWarnings(gbash("gg iris  point Sepal.W Sepal.L  theme text")),
        "COMPILE ERROR"
    )
    ee(gbash("gg mtcars # text mpg cyl"),
       "ggplot2::ggplot(mtcars) + " %++%
       "ggplot2::geom_text(ggplot2::aes(x=mpg, y=cyl))")

})
