library(ggbash)
context("save-ggplot")

test_that("save png", {

    tmpdir <- tempdir()
    oldwd <- setwd(tmpdir)
    on.exit(setwd(oldwd))
    ggstr <-
        "ggplot2::ggplot(mtcars) + ggplot2::geom_point(ggplot2::aes(cyl,mpg))"
    conf <- list(aes = c("x=cyl", "y=mpg"), non_aes = c(), geom_list = "point")
    datadirv <- c("mtcars-32")
    e <- function(x) expect_message(x)

    for (dev in c("png", "pdf")) {
        e(save_ggplot(datadirv[1], ggstr, conf, argv = c(dev)))
        e(save_ggplot(datadirv[1], ggstr, conf, argv = c(dev, "500*250")))
        e(save_ggplot(datadirv[1], ggstr, conf,
                      argv = c(dev, "500*250", "\"my-filename\"")))
        argv <- c("500*250", "\"my-filename\"")
        for (indices in list(c(1, 2), c(2, 1)))
            e(save_ggplot(datadirv[1], ggstr, conf,
                          argv = c(dev, argv[indices])))
        unlink(x = paste0(tmpdir, "/", datadirv[1]), recursive = TRUE)
    }

    ggbash("gg iris + p Sepal.W Sepal.L col=Sp | png my_image")
})
