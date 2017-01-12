library(ggbash)
context("get_analogue")

ee <- testthat::expect_equal

test_that("use case 1", {

    all <- get_all_theme_aes()$name

    possibilities <- c("axis.text", "axis.text.x")
    a <- get_analogue("a.t.x", possibilities)$name
    ee(a[2], possibilities[1])
    ee(a[1], possibilities[2])

    for ( case in list(
        list(input = "lgnd.key", output = "legend.key"),
        list(input = "lgnd.bg",  output = "legend.background"),
        list(input = "l.b.b",  output = "legend.box.background"),
        list(input = "axis.ttl", output = "axis.title"),
        list(input = "a.txt.x",  output = "axis.text.x"),
        list(input = "a.ttl.x",  output = "axis.title.x")
    ))
        ee(get_analogue(case$input, all)$name[1], case$output)

})
