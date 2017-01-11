context("ggplot2-docs-2.1.0")
# nolint start

assign('mpg', ggplot2::mpg, envir = .GlobalEnv)
assign("diamonds", ggplot2::diamonds, envir = .GlobalEnv)
assign("faithfuld", ggplot2::faithfuld, envir = .GlobalEnv)
assign("mtcars2",
       transform(mtcars, mpg = ifelse(runif(32) < .2, NA, mpg)),
       envir = .GlobalEnv)

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

    gbash("gg diamonds x y + bin2d")
    # FIXME no xlim ylim

    gbash("gg diamonds x y + bin2d bins=10")
    gbash("gg diamonds x y + bin2d bins=30")
    #
    # d + geom_bin2d(binwidth = c(0.1, 0.1))
})

test_that("geom_boxplot", {
    gbash("gg mpg x=class y=hwy + box")
    #ggbash("gg mpg x=class,y=hwy + box + jitter width=.2")
    # FIXME p + geom_boxplot() + coord_flip()
    #ggbash("gg mpg x=class,y=hwy + box notch=TRUE") # FIXME non-aes
    #ggbash("gg mpg x=class,y=hwy + box varwidth=TRUE") # FIXME non-aes
    gbash("gg mpg x=class y=hwy + box fill='white' color='#3366FF'") # FIXME non-aes
    #ggbash("gg mpg x=class,y=hwy + box outlier.colour='red'") # FIXME
    #ggbash("gg mpg x=class,y=hwy + box outlier.colour='red' outlier.shape=1") # FIXME

    gbash("gg mpg x=class y=hwy + box colour=drv")
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
    gbash("gg faithfuld waiting eruptions z=density + contour") # defaultZproblem

    gbash("gg faithful waiting eruptions + density_2d")
    gbash("gg faithfuld waiting eruptions z=density + contour bins=2")
    gbash("gg faithfuld waiting eruptions z=density + contour bins=10")
    gbash("gg faithfuld waiting eruptions z=density + contour binwidth=0.01")
    gbash("gg faithfuld waiting eruptions z=density + contour binw=0.001")

    # v + geom_contour(aes(colour = ..level..))

    gbash("gg faithfuld waiting eruptions z=density + contour colour = 'red'")
    ee(bash("gg faithfuld w e z=d + rast fill=d + contour c = 'white'"),
       "ggplot(faithfuld, aes(waiting, eruptions, z=density)) + " %+%
        "geom_raster(aes(fill=density)) + geom_contour(colour='white')"
       )

})

test_that("geom_count", {
    gbash("gg mpg x=cty y=hwy + point")
    gbash("gg mpg x=cty y=hwy + count")
    # ggbash("gg mpg x=cty, y=hwy + count") # + scale_size_area

    #ggbash("gg diamonds x=cut, y=clarity + count size=..prop.. ")
    #ggbash("gg diamonds x=cut, y=clarity + count size = ..prop.. group=1")
})

test_that("geom_crossbar", {
    assign("crossbar_df",
            data.frame(
                trt = factor(c(1, 1, 2, 2)),
                resp = c(1, 5, 3, 4),
                group = factor(c(1, 2, 1, 2)),
                upper = c(1.1, 5.3, 3.3, 4.2),
                lower = c(0.8, 4.6, 2.4, 3.6)
            ),
           envir =.GlobalEnv)

    # ggbash("gg crossbar_df trt resp colour=group + liner ymin=lower ymax=upper")

    # ggbash("gg crossbar_df trt resp colour=group + pointr ymin=lower ymax=upper")

    # ...
})

test_that("geom_density", {
    gbash("gg diamonds carat + density ")
    # ggbash("gg diamonds carat + density adjust = 1/5")
    gbash("gg diamonds carat + density adjust = 5")

    # ggplot(diamonds, aes(depth, colour = cut)) +
    #     geom_density() +
    #     xlim(55, 70)

    # ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
    #     geom_density(alpha = 0.1) +
    #     xlim(55, 70)

    # ggbash("gg diamonds carat fill=cut + density position='stack'")

    #ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
    #    geom_density(position = "stack")
    #
    # ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
    #     geom_density(position = "fill")
    #
})

test_that("geom_point", {
    gbash("gg mtcars wt mpg + point")
    # gbash("gg mtcars wt mpg + point colour=factor(cyl)")
    # p + geom_point(aes(shape = factor(cyl)))

    gbash("gg mtcars wt mpg + point size=qsec")
    # ggbash("gg mtcars wt mpg + point size = qsec")

    gbash("gg mtcars wt mpg + point colour=cyl")

    # p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
    # FIXME ggbash(...) + scale_... should work

    # p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
    gbash("gg mtcars wt mpg + point colour='red' size=3")

    # d <- ggplot(diamonds, aes(carat, price))
    # d + geom_point(alpha = 1/10)
    # d + geom_point(alpha = 1/20)
    # d + geom_point(alpha = 1/100)

    gbash("gg mtcars w m + p shape=21 col='black' f='white' siz=5 st=5")

    # You can create interesting shapes by layering multiple points of
    # # different sizes
    # p <- ggplot(mtcars, aes(mpg, wt, shape = factor(cyl)))
    # p + geom_point(aes(colour = factor(cyl)), size = 4) +
    #     geom_point(colour = "grey90", size = 1.5)

    # p + geom_point(colour = "black", size = 4.5) +
    #     geom_point(colour = "pink", size = 4) +
    #     geom_point(aes(shape = factor(cyl)))

    # p + geom_point(colour = "black", size = 4.5, show.legend = TRUE) +
    #     geom_point(colour = "pink", size = 4, show.legend = TRUE) +
    #     geom_point(aes(shape = factor(cyl)))

    ggbash("gg mtcars wt mpg + point")
    # ggbash("gg mtcars wt mpg + point na.rm =TRUE")
})


# nolint end
