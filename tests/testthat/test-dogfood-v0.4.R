library(ggbash)
context("dogfood-v0.4")

test_that("NSE", {

    ee(bash(gg(iris)
            + point(Sepal.W, Sepal.L)
            + point(Sepal.W, Sepal.L, data=iris2)),
        "ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length)) +" %++%
            " geom_point(aes(x=Sepal.Width, y=Sepal.Length), data=iris2)"
    )

    ee(find_first_by_prefix("p", c("price", "p")), 2)
})
