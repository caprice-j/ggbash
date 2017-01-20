context("ggplot2-docs-2.1.0")
# nolint start

assign('mpg', ggplot2::mpg, envir = .GlobalEnv)
assign("diamonds", ggplot2::diamonds, envir = .GlobalEnv)
assign("faithfuld", ggplot2::faithfuld, envir = .GlobalEnv)
assign("mtcars2",
       transform(mtcars, mpg = ifelse(runif(32) < .2, NA, mpg)),
       envir = .GlobalEnv)
assign("economics", ggplot2::economics, envir = .GlobalEnv)
assign("economics_long", ggplot2::economics_long, envir = .GlobalEnv)


ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

assign("values",
        data.frame(id = ids,
                   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)),
       envir = .GlobalEnv)

assign("positions",
       data.frame(
           id = rep(ids, each = 4),
           x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
                 0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
           y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
                 2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
       ),
       envir = .GlobalEnv
       )

assign("datapoly",
       merge(values, positions, by=c("id")),
       envir = .GlobalEnv)

assign("stream",
       data.frame(
           x = cumsum(runif(50, max = 0.1)),
           y = cumsum(runif(50,max = 0.1))
       ),
       envir = .GlobalEnv)

test_that("geom_abline", {
    gbash("gg mtcars wt mpg + point + vline xintercept = 5")

    ee(bash(gg(mtcars,wt,mpg) + point + vline(xintercept = 1:5)),
       "ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_vline(xintercept=1:5)")

    gbash("gg mtcars wt mpg + point + hline yintercept = 20")

    gbash("gg mtcars wt mpg + point + abline") # outside the range of the data

    gbash("gg mtcars wt mpg + point + abline intercept = 20")

    gbash("gg mtcars wt mpg + point + abline intercept = 37 slope = -5")

    ee(bash("gg mtcars wt mpg + point + smooth method='lm' se=FALSE"),
       "ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method='lm', se=FALSE)")

    mean_wt <- data.frame(cyl = c(4, 6, 8),
                          wt = c(2.28, 3.11, 4.00))
    # ggbash(gg(mtcars,wt,mpg) + point + hline(yint = wt, mean_wt) ) + facet_wrap(~ cyl)
    # TODO data frame

})

# GEOMBAR COMPLETED
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

    gbash(gg(mpg, class) + bar(fill=drv, position="fill"))

    reorder_size <- function(x) {
       factor(x, levels = names(sort(table(x))))
    }
    ee(bash(gg(mpg, x=reorder_size(class)) + bar),
       "ggplot(mpg, aes(reorder_size(class))) + geom_bar()")
    # FIXME = should be optional

})

test_that("geom_bin2d", {

    gbash("gg diamonds x y + bin2d")
    # FIXME no xlim ylim

    gbash("gg diamonds x y + bin2d bins=10")
    gbash("gg diamonds x y + bin2d bins=30")
    # FIXME
    ee(bash(gg(diamonds,x,y) + bin2d(binwidth=c(.1,.1))),
       "ggplot(diamonds, aes(x, y)) + geom_bin2d(binwidth=c(0.1,0.1))")
})

test_that("geom_boxplot", {
    bash("gg mpg x=class y=hwy + box")
    bash("gg mpg x=class,y=hwy + box + jitter width=.2")
    # FIXME p + geom_boxplot() + coord_flip()
    bash("gg mpg x=class,y=hwy + box notch=TRUE")
    bash("gg mpg x=class,y=hwy + box varwidth=TRUE")
    bash("gg mpg x=class y=hwy + box fill='white' color='#3366FF'")
    bash("gg mpg x=class,y=hwy + box outlier.colour='red'")
    ee(bash("gg mpg x=class,y=hwy + box outlier.colour='red' outlier.shape=1"),
       "ggplot(mpg, aes(class, hwy)) + " %++%
        "geom_boxplot(outlier.colour='red', outlier.shape=1)"
    )
    # outlier.colour is the example of a dot in variable name
    # (affects lexing rules)

    bash("gg mpg x=class y=hwy + box colour=drv")
    bash("gg diamonds carat, price + box")
    ee(bash(gg(diamonds, carat, price) + box(group=cut_width(carat, 0.25))),
       "ggplot(diamonds, aes(carat, price)) + geom_boxplot(aes(group=cut_width(carat,0.25)))")

    # MAYBE-LATER special boxplot
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

# COMPLETED geom_contour
test_that("geom_contour", {
    # defaultZproblem
    ee(bash("gg faithfuld waiting eruptions z=density + contour"),
        "ggplot(faithfuld, aes(waiting, eruptions, z=density)) + geom_contour()")

    gbash("gg faithful waiting eruptions + density_2d")
    gbash("gg faithfuld waiting eruptions z=density + contour bins=2")
    gbash("gg faithfuld waiting eruptions z=density + contour bins=10")
    gbash("gg faithfuld waiting eruptions z=density + contour binwidth=0.01")
    gbash("gg faithfuld waiting eruptions z=density + contour binw=0.001")

    # v + geom_contour(aes(colour = ..level..))
    ee(bash(gg(faithfuld, waiting, eruptions, z=density) +
                contour(colour=..level..)),
       "ggplot(faithfuld, aes(waiting, eruptions, z=density)) + " %++%
           "geom_contour(aes(colour=..level..))")

    gbash("gg faithfuld waiting eruptions z=density + contour colour = 'red'")
    ee(bash("gg faithfuld w e z=d + rast fill=d + contour c = 'white'"),
       "ggplot(faithfuld, aes(waiting, eruptions, z=density)) + " %++%
        "geom_raster(aes(fill=density)) + geom_contour(colour='white')"
       )

})

# GEOM_COUNT COMPLETED

test_that("geom_count", {
    gbash("gg mpg x=cty y=hwy + point")
    gbash("gg mpg x=cty y=hwy + count")
    # works: ggbash("gg mpg x=cty, y=hwy + count")  + scale_size_area()

    ee(bash(gg(diamonds,x=cut,y=clarity) + count(size=..prop..)),
       "ggplot(diamonds, aes(cut, clarity)) + geom_count(aes(size=..prop..))")
    ee(bash(gg(diamonds,x=cut,y=clarity) + count(size=..prop.., group=1)),
       "ggplot(diamonds, aes(cut, clarity)) + geom_count(aes(size=..prop.., group=1))")
    ee(bash(gg(diamonds,x=cut,y=clarity) + count(size=..prop.., group=cut)),
       "ggplot(diamonds, aes(cut, clarity)) + geom_count(aes(size=..prop.., group=cut))")

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

    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + liner(ymin=lower,ymax=upper)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_linerange(aes(ymin=lower, ymax=upper))"
    )
    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + pointr(ymin=lower,ymax=upper)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_pointrange(aes(ymin=lower, ymax=upper))"
    )
    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + cross(ymin=lower,ymax=upper, width=.2)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_crossbar(aes(ymin=lower, ymax=upper), width=0.2)"
    )

    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + errorb(ymin=lower,ymax=upper, width=.2)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)"
    )

    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + geom_line(group=group) + errorb(ymin=lower,ymax=upper, width=.2)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_line(aes(group=group)) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)"
    )

    ee(
        bash(gg(crossbar_df,trt,resp,colour=group) + geom_line(group=group) + errorb(ymin=lower,ymax=upper, width=.2)),
        "ggplot(crossbar_df, aes(trt, resp, colour=group)) + geom_line(aes(group=group)) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)"
    )

    ee(
        bash(gg(crossbar_df, trt, resp, fill = group)
             + bar(pos = "dodge", stat = "identity")
             + errorb(ymi = l, yma = u, pos = "dodge", wid = .25)
                 ),
        "ggplot(crossbar_df, aes(trt, resp, fill=group)) + geom_bar(position=\"dodge\", stat=\"identity\") + geom_errorbar(aes(ymin=lower, ymax=upper), position=\"dodge\", width=0.25)"
    )

    # FIXME position_jitter
    # bash(gg(crossbar_df, trt, resp, fill = group)
    #      + bar(pos = position_dodge(width=.9), stat = "identity")
    #      + errorbar(ymi = l, yma = u, pos = position_dodge(width=0.9), wid = .25))
})


# geom_density 6/8
test_that("geom_density", {
    gbash("gg diamonds carat + density ")
    ee(
        bash(gg(diamonds,carat) + density(adjust = 1/5)),
        "ggplot(diamonds, aes(carat)) + geom_density(adjust=1/5)"
    )
    gbash("gg diamonds carat + density adjust = 5")

    # ggplot(diamonds, aes(depth, colour = cut)) +
    #     geom_density() +
    #     xlim(55, 70)

    # ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
    #     geom_density(alpha = 0.1) +
    #     xlim(55, 70)

    bash(gg(diamonds,carat,fill=cut) + density(position="stack"))

    ee(
        bash(gg(diamonds, carat, ..count.., fill=cut) + density(position="stack")),
        "ggplot(diamonds, aes(carat, ..count.., fill=cut)) + geom_density(position=\"stack\")"
        )
    bash(gg(diamonds, carat, ..count.., fill=cut) + density(position="fill"))
})

# "geom_density2d" xlim

# GEOM_DOTPLOT COMPLETED

test_that("geom_dotplot", {

    bash(gg(mtcars, x=mpg) + dot)
    bash(gg(mtcars, x=mpg) + dot(binwdith=1.5))
    bash(gg(mtcars, x=mpg) + dot(method="histodot", binwdith=1.5))
    bash(gg(mtcars, x=mpg) + dot(stackdir="center", binwdith=1.5))
    bash(gg(mtcars, x=mpg) + dot(stackdi="centerwhole", binw=1.5))
    # y axis isn't really meaningful, so hide it
    #ggbash(gg(mtcars, x = mpg) + dot(binwidth=1.5)) +
    #    scale_y_continuous(NULL, breaks = NULL)
    # worked
    bash(gg(mtcars, x=mpg) + dot(binwdith=1.5, stackratio = .7))
    bash(gg(mtcars, x=mpg) + dot(binwdith=1.5, dotsize = 1.25))
    bash(gg(mtcars, x=1, y=mpg) + dot(binaxis="y", stackdir="center"))
    bash(gg(mtcars, x=factor(cyl), y=mpg) + dot(binaxis="y", stackdir="center"))
    bash(gg(mtcars, x=factor(cyl), y=mpg) + dot(binaxis="y", stackdir="centerwhole"))

ee(
    bash(gg(mtcars,x=factor(vs),fill=factor(cyl),y=mpg)
         + dot(binax="y", stackdir = "center", pos = "dodge")),
    "ggplot(mtcars, aes(factor(vs), fill=factor(cyl), mpg)) + geom_dotplot(binaxis=\"y\", stackdir=\"center\", position=\"dodge\")"
)

    bash(gg(mtcars, x=f(am), y=mpg) + dot(binaxis="y", stackdir="center", binpositions="all"))

    ee(
        bash(gg(mtcars, x=mpg, fill=factor(cyl))
             + dot(stackgroups = TRUE, binwidth = 1,
                   inpositions = "all")),
        "ggplot(mtcars, aes(mpg, fill=factor(cyl))) + geom_dotplot(stackgroups=TRUE, binwidth=1, binpositions=\"all\")"
    )

    ee(
        bash(gg(mtcars, x=mpg, fill=factor(cyl))
             + dot(stackg=TRUE, binw=1, method="histodot")),
        "ggplot(mtcars, aes(mpg, fill=factor(cyl))) + geom_dotplot(stackgroups=TRUE, binwidth=1, method=\"histodot\")"
    )

    bash(gg(mtcars,x=1,y=mpg,fill=factor(cyl))
         + dotplot(binax="y", stackg=TRUE, binw=1, method="histodot"))
})

test_that("geom_errorbarh", {
    assign("errorbarh_df",
           data.frame(
               trt = factor(c(1, 1, 2, 2)),
               resp = c(1, 5, 3, 4),
               group = factor(c(1, 2, 1, 2)),
               se = c(0.1, 0.3, 0.3, 0.2)
           ),
           envir =.GlobalEnv)

    ee(1, 1)

    # FIXME
    # ggbash(gg(errorbarh_df, resp, trt, col=group)
    #       + p + errorbarh(xmax=resp+se, xmin=resp-se) )

})

# GEOM_FREQPOLY COMPLETED 6/6

test_that("geom_freqpoly", {
    bash(gg(diamonds, carat) + hist)
    bash(gg(diamonds, carat) + hist(binw=.01))
    bash(gg(diamonds, carat) + hist(bins=200))
    bash(gg(diamonds, price, fill=cut) + hist(binw=500))
    bash(gg(diamonds, price, col=cut) + freqpoly(binwidth=500))
    ee(
        bash(gg(diamonds, price, ..density.., col=cut)
             + freqpoly(binwidth=500)),
        "ggplot(diamonds, aes(price, ..density.., " %++%
            "colour=cut)) + geom_freqpoly(binwidth=500)"
        )
    # MAYBE-LATER ..density.. should be partial matched
})

# geom_hex COMPLETED 5/5

test_that("geom_hex", {
    bash(gg(diamonds,x=carat,y=price) + hex())
    bash(gg(diamonds,x=carat,y=price) + hex(bins = 10))
    bash(gg(diamonds,x=carat,y=price) + hex(bins=30))
    ee(
        bash(gg(diamonds,x=carat,y=price) + hex(binw = c(1, 1000))),
        "ggplot(diamonds, aes(carat, price)) + geom_hex(binwidth=c(1,1000))"
    )
    bash(gg(diamonds,x=carat,y=price) + hex(binw = c(.1, 500)))
})

# geom_jitter COMPLETED 6/6

test_that("geom_jitter", {
    bash(gg(mpg, cyl, hwy) + point)
    bash(gg(mpg, cyl, hwy) + jitter)
    ee(bash(gg(mpg, cyl, hwy) + geom_jitter(c=class)),
        "ggplot(mpg, aes(cyl, hwy)) + geom_jitter(aes(colour=class))"
    )
    bash(gg(mpg, cyl, hwy) + geom_jitter(wid=0.25))
    bash(gg(mpg, cty, hwy) + geom_jitter())
    bash(gg(mpg, cty, hwy) + geom_jit(wid=.5, height=.5))
})

test_that("geom_label", {
    # ggbash(g(mtcars,wt,mpg,lab=rownames(mtcars))+text) # FIXME
    ee(1, 1)
})

test_that("geom_map", {
    ee(1, 1)
})

test_that("geom_path", {
    bash(gg(economics, date, unemploy) + line)
    bash(gg(economics_long, date, value01, c=variable) + line)
    assign("recent",
           economics[economics$date > as.Date("2013-01-01"), ],
           envir = .GlobalEnv)
    bash(gg(recent, date, unemploy) + line)
    ee(bash(gg(recent, date, unemploy) + step),
       "ggplot(recent, aes(date, unemploy)) + geom_step()"
       )
    # ggbash(gg(economics, unemploy/pop, psavert) + path) # FIXME
    #
    bash(gg(economics, date, unemploy) + geom_line(c="red"))

    # ggbash(gg(economics, date, pop) + line(arrow=arrow()))
    #
    #
    #
    #

})

# geom_point COMPLETED

test_that("geom_point", {
    gbash("gg mtcars wt mpg + point")
    bash(gg(mtcars,wt,mpg) + point(colour=factor(cyl)))
    gbash(g(mtcars, wt, mpg) + p(c=factor(cyl)))
    gbash(g(mtcars, wt, mpg) + p(sh=factor(cyl)))
    gbash(g(mtcars, wt, mpg) + p(sz=qse))
    #bash(gg(mtcars, wt, mpg) + point(colour=cyl)) + scale_color_gradient(low = "blue") # worked

    # bash(gg(mtcars, wt, mpg) + point(sh=factor(cyl))) + scale_shape(solid = FALSE) # worked

    gbash("gg mtcars wt mpg + point colour='red' size=3")

    gbash(gg(diamonds, carat, price) + p(a=1/10))
    gbash(gg(diamonds, carat, price) + p(a=1/20))
    gbash(gg(diamonds, carat, price) + p(a=1/100))

    gbash("gg mtcars w m + p shape=21 col='black' f='white' siz=5 st=5")

    ee(bash(g(mtcars, mpg, wt, sh=factor(cyl))
            + p(c=factor(cyl), sz=4)
            + p(c="gray90", sz=1.5)),
       "ggplot(mtcars, aes(mpg, wt, shape=factor(cyl))) + " %++%
           "geom_point(aes(colour=factor(cyl)), size=4) + " %++%
           "geom_point(colour=\"gray90\", size=1.5)"
       )

    gbash(gg(mtcars, mpg, wt)
           + point(col = "black", sz=4.5, legend=TRUE)
           + point(col = "pink",  sz=4,   legend=TRUE)
           + point(shp = factor(cyl)))

    bash("gg mtcars wt mpg + point")

    bash(gg(mtcars2, wt, mpg) + p)
    bash(gg(mtcars2, wt, mpg) + p(na.rm=TRUE))
})


# GEOM_POLYGON COMPLETED 2/2
test_that("geom_polygon", {
    ee(bash(gg(datapoly, x, y) + polygon(fill=value, group=id)),
       "ggplot(datapoly, aes(x, y)) + geom_polygon(aes(fill=value, group=id))")

    ee(bash(gg(datapoly, x, y) + polygon(fill=value, group=id)
            + geom_line(data=stream, c="grey30", sz=5)),
        "ggplot(datapoly, aes(x, y)) + geom_polygon(aes(fill=value, group=id)) + geom_line(data=stream, colour=\"grey30\", size=5)"
    )
})

test_that("geom_quantile", {ee(1, 1)})
test_that("geom_raster", {ee(1, 1)})
test_that("geom_ribbon", {ee(1, 1)})
test_that("geom_rug", {ee(1, 1)})
test_that("geom_segment", {ee(1, 1)})
test_that("geom_smooth", {
    bash(gg(mpg, displ, hwy) + point + smooth)
    bash(gg(mpg, displ, hwy) + point + geom_smooth(span=.3))
    bash(gg(mpg, displ, hwy) + point
           + geom_smooth(method="lm", se=FALSE))

    # ggplot(mpg, aes(displ, hwy)) +
    #     geom_point() +
    #     geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)
    #

    ee(
        bash(gg(mpg, displ, hwy, c=class) + point
             + geom_smooth(se = FALSE, method = "lm")),
        "ggplot(mpg, aes(displ, hwy, colour=class)) + geom_point() + geom_smooth(se=FALSE, method=\"lm\")"
    )

    # facet

    # binomial smooth
})

test_that("geom_violin", {
    bash(gg(mtcars, x=factor(cyl), mpg) + violin) # need x=

    ee(
        bash(gg(mtcars, x=factor(cyl), mpg)
             + violin + geom_jitter(height=0)),
        "ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin() + geom_jitter(height=0)"
    )

    # coord flip

    bash(gg(mtcars, x=factor(cyl), mpg) + violin(scale="count"))
    bash(gg(mtcars, x=factor(cyl), mpg) + violin(scale="width"))
    # bash(gg(mtcars, x=factor(cyl), mpg) + violin(trim=FALSE))
    bash(gg(mtcars, x=factor(cyl), mpg) + violin(adjust = .5))

    bash(gg(mtcars, x=factor(cyl), mpg) + violin(fill=cyl))
    bash(gg(mtcars, x=factor(cyl), mpg) + violin(fill=factor(cyl)))

    bash(gg(mtcars, x=factor(cyl), mpg) + violin(fill=factor(vs)))
    bash(gg(mtcars, x=factor(cyl), mpg) + violin(fill=factor(am)))

    ee(
        bash(gg(mtcars, x=factor(cyl), mpg) + violin(fill="grey80", colour="#3366FF")),
        "ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin(fill=\"grey80\", colour=\"#3366FF\")"
    )

    ee(
        bash(gg(mtcars, x=factor(cyl), mpg) + violin(draw_quantiles = c(.25, .5, .75))),
        "ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin(draw_quantiles=c(0.25,0.5,0.75))"
    )

    # complex one here
})
# nolint end
