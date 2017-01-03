library(ggbash)
context('save-ggplot')

test_that('save png', {
    tmpdir <- tempdir()
    oldwd <- setwd(tmpdir)
    on.exit(setwd(oldwd))
    statl <-
    list(cmd  = 'ggplot2::ggplot(mtcars) + ggplot2::geom_point(ggplot2::aes(cyl,mpg))',
         conf = list(geom='point', aes=c('x=cyl', 'y=mpg'), non_aes=c('colour="blue"')))
    datadirv <- c('mtcars-32')
    e <- function(x) expect_message(x)

    for (dev in c('png', 'pdf')) {
        e(save_ggplot(datadirv[1], statl, argv=c(dev)))
        e(save_ggplot(datadirv[1], statl, argv=c(dev, 'big')))
        e(save_ggplot(datadirv[1], statl, argv=c(dev, 'small')))
        e(save_ggplot(datadirv[1], statl, argv=c(dev, '500x250')))
        e(save_ggplot(datadirv[1], statl, argv=c(dev, '500x250', '"my-filename"')))
        argv <- c('500x250', '"my-filename"')
        for (indices in list(c(1,2), c(2,1)))
            e(save_ggplot(datadirv[1], statl, argv=c(dev, argv[indices])))
        unlink(x = paste0(tmpdir,'/',datadirv[1]), recursive = TRUE)
    }
})
