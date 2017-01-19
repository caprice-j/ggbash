library(ggbash)
context("dogfood-v0.4")

suppressPackageStartupMessages( library(dplyr) )

assign("iris2",
       iris %>%
           rename(green.width = Sepal.Width, green.length = Sepal.Length,
                  bloom.width = Petal.Width, bloom.length = Petal.Length),
       envir = .GlobalEnv)


test_that("NSE", {

    ee(find_first_by_prefix("p", c("price", "p")), 2)

    assign("test_df",
           data.frame( x = 1:10, y = 11:20, gr = c(rep(1,5), rep(2,5)) ),
           envir = .GlobalEnv)

    # test_df %>% ggbash(gg() + p(x=x, y=y, group=1) + line(x=x, y=y, group=1))

    iris %>% ggbash(gg(x=Sepal.W, y=Sepal.L) + p() + line)

    # Gelman 1.1a.
    data.frame( y = seq(-7, 10, .02) ) %>%
        #     P(y=1) = P(θ=1)    P(y=1|θ=1)   + P(θ=2)    P(y=1|θ=2)
        mutate( dens =  .5  *  dnorm(y, 1, 2) +  .5  *  dnorm(y, 2, 2) ) %>%
        ggbash(gg(x=y, y=dens) + point)

    ee(
        ggbash(gg(iris)
               + p(Sepal.W, Sepal.L)
               + p(green.w, green.l, data=iris2)
               + p(green.w, Sepal.L, data=bind_cols(iris, iris2), color="blue"),
               as_string = TRUE),
        "ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length))" %++%
        " + geom_point(aes(x=green.width, y=green.length), data=iris2)" %++%
        " + geom_point(aes(x=green.width, y=Sepal.Length), " %++%
        "data=bind_cols(iris,iris2), colour=\"blue\")"
    )
})
