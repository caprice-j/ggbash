context("ggplot2-docs-2.1.0")
# nolint start

assign('mpg', ggplot2::mpg, envir = .GlobalEnv)
assign("diamonds", ggplot2::diamonds, envir = .GlobalEnv)

test_that("geom_abline", {
    # p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

    gbash("gg mtcars wt mpg + point + vline xintercept = 5")

    # TODO p + geom_vline(xintercept = 1:5)
    # ggbash("gg mtcars wt mpg + point + vline xintercept = 1:5")

    gbash("gg mtcars wt mpg + point + hline yintercept = 20")

    gbash("gg mtcars wt mpg + point + abline") # outside the range of the data

    gbash("gg mtcars wt mpg + point + abline intercept = 20")

    gbash("gg mtcars wt mpg + point + abline intercept = 37 slope = -5")

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
    gbash("g mpg x=class + bar")

    ee(bash("g mpg x=class + bar weight=displ"),
       "ggplot(mpg, aes(class)) + geom_bar(aes(weight=displ))")

    assign('dfgg',
           data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2)),
           envir = .GlobalEnv)
    ee(bash("gg dfgg trt outcome + bar stat='identity'"),
       "ggplot(dfgg, aes(trt, outcome)) + geom_bar(stat='identity')")

    gbash("gg dfgg trt outcome + point")

    assign('dfgg',
           data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4))),
            envir = .GlobalEnv)
    gbash("gg dfgg x + bar")

    ee(bash("gg dfgg x + h binwidth=0.5"),
       "ggplot(dfgg, aes(x)) + geom_histogram(binwidth=0.5)")

    gbash("gg mpg class + bar fill=drv")
    ee(bash("gg mpg class + bar fill=drv position='dodge'"),
       "ggplot(mpg, aes(class)) + geom_bar(aes(fill=drv), position='dodge')")

#reorder_size <- function(x) {
#    factor(x, levels = names(sort(table(x))))
#}
#ggplot(mpg, aes(reorder_size(class))) + geom_bar()

})

test_that("geom_bin2d", {

    ggbash("gg diamonds x y + bin2d")
    # FIXME no xlim ylim

    ggbash("gg diamonds x y + bin2d bins=10")
    ggbash("gg diamonds x y + bin2d bins=30")
    #
    # d + geom_bin2d(binwidth = c(0.1, 0.1))
})

test_that("geom_boxplot", {
    ggbash("gg mpg x=class,y=hwy + box")
    ggbash("gg mpg x=class,y=hwy + box + jitter width=.2")
    # FIXME p + geom_boxplot() + coord_flip()
    #ggbash("gg mpg x=class,y=hwy + box notch=TRUE") # FIXME non-aes
    #ggbash("gg mpg x=class,y=hwy + box varwidth=TRUE") # FIXME non-aes
    ggbash("gg mpg x=class,y=hwy + box fill='white', color='#3366FF'") # FIXME non-aes
    #ggbash("gg mpg x=class,y=hwy + box outlier.colour='red'") # FIXME
    #ggbash("gg mpg x=class,y=hwy + box outlier.colour='red' outlier.shape=1") # FIXME

    ggbash("gg mpg x=class,y=hwy + box colour=drv")
    #ggbash("gg diamonds carat, price + box")
    # ggbash("gg diamonds + carat price + box group=")FIXME
    #gplot(diamonds, aes(carat, price)) +
    #    geom_boxplot(aes(group = cut_width(carat, 0.25)))

    # It's possible to draw a boxplot with your own computations if you
    # use stat = "identity":
    # y <- rnorm(100)
    # df <- data.frame(
    #     x = 1,
    #     y0 = min(y),
    #     y25 = quantile(y, 0.25),
    #     y50 = median(y),
    #     y75 = quantile(y, 0.75),
    #     y100 = max(y)
    # )
    # ggplot(df, aes(x)) +
    #     geom_boxplot(
    #         aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
    #         stat = "identity"
    #     )
    #
})

test_that("geom_contour", {

})

# nolint end
