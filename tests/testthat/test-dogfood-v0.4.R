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

    assign("test_df",
           data.frame( x = 1:10, y = 11:20, gr = c(rep(1,5), rep(2,5)) ),
           envir = .GlobalEnv)

    # test_df %>% ggbash(gg() + p(x=x, y=y, group=1) + line(x=x, y=y, group=1))

    iris %>% ggbash(gg(x=Sepal.W, y=Sepal.L) + p() + line)
})
