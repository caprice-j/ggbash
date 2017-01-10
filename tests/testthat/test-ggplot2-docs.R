context("ggplot2-docs-2.1.0")

# nolint start
test_that("geom_abline", {
    # p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

    ggbash("gg mtcars wt mpg + point + vline xintercept = 5")

    # TODO p + geom_vline(xintercept = 1:5)
    # ggbash("gg mtcars wt mpg + point + vline xintercept = 1:5")

    ggbash("gg mtcars wt mpg + point + hline yintercept = 20")

    ggbash("gg mtcars wt mpg + point + abline") # outside the range of the data

    ggbash("gg mtcars wt mpg + point + abline intercept = 20")

    ggbash("gg mtcars wt mpg + point + abline intercept = 37 slope = -5")

    ee(bash("gg mtcars wt mpg + point + smooth method='lm' se=FALSE"),
       "ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method='lm', se=FALSE)")

    # ggplot(mtcars, aes(mpg, wt, colour = wt)) +
    #     geom_point() +
    #     geom_hline(aes(yintercept = wt, colour = wt), mean_wt) +
    #     facet_wrap(~ cyl)
    mean_wt <- data.frame(cyl = c(4, 6, 8),
                          wt = c(2.28, 3.11, 4.00))
    # ggbash("gg mtcars wt mpg + point + hline yint = wt")

})

test_that("geom_bar", {
    ggbash("g mpg x=class + bar")

    ee(bash("g mpg x=class + bar weight=displ"),
       "ggplot(mpg, aes(class)) + geom_bar(aes(weight=displ))")
})

# nolint end
