library(ggbash)
library(futile.logger)
context("ggbash-compiler-2")

test_that("ggplot2 prefix match for theme element's configuration", {
    ee(gbash("gg iris + point Sepal.W Sepal.L + theme text: size=1"),
       "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(" %+%
           "x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(text = " %+%
           "ggplot2::element_text(size=1))"
    )

        ee(gbash("gg iris + point Sepal.W Sepal.L + theme text: siz=1"),
       "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(" %+%
       "x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(text = " %+%
       "ggplot2::element_text(size=1))"
    )

    ee(gbash("gg iris + point Sepal.W Sepal.L + theme text: vj=\"dotted\""),
       "ggplot2::ggplot(iris) + ggplot2::geom_point(ggplot2::aes(" %+%
       "x=Sepal.Width, y=Sepal.Length)) + ggplot2::theme(text = " %+%
       "ggplot2::element_text(vjust=\"dotted\"))"
    )

    # FIXME gg iris  point Sepal.W Sepal.L  theme text:
    ee(gbash("gg mtcars + text mpg cyl"),
       "ggplot2::ggplot(mtcars) + " %+%
       "ggplot2::geom_text(ggplot2::aes(x=mpg, y=cyl))")


})
