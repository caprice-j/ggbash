library(ggbash)
context('save-ggplot')

test_that('save png', {
    tmpdir <- tempdir()
    setwd(tmpdir)
    statl <-
    list(cmd  = 'ggplot2::ggplot(mtcars) + ggplot2::geom_point(ggplot2::aes(cyl,mpg))',
         conf = list('x=cyl', 'y=mpg') )
    datadirv <- c('mtcars-32')
    e <- function(x) expect_message(x)
    e(save_ggplot(datadirv[1], statl, argv=c('png')))
    e(save_ggplot(datadirv[1], statl, argv=c('png', 'big')))
    e(save_ggplot(datadirv[1], statl, argv=c('png', 'small')))
    e(save_ggplot(datadirv[1], statl, argv=c('png', '500x250')))
    e(save_ggplot(datadirv[1], statl, argv=c('png', '500x250', '"my-filename"')))
    argv <- c('500x250', '"my-filename"', '50')
    for (indices in list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1)))
        e(save_ggplot(datadirv[1], statl, argv=c('png', argv[indices])))
    unlink(x = paste0(tmpdir,'/',datadirv[1]), recursive = TRUE)
})
