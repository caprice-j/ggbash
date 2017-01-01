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
    unlink(x = paste0(tmpdir,'/',datadirv[1]), recursive = TRUE)
})
