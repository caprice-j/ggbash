library(ggbash)
library(futile.logger)
context("compiler-1")

ggbashenv$layer_coll <- list("NA", "NA", "NA", "NA", "NA")

test_that("ggplot2 lexer", {

    lex$input("gg iris##point abc def##smooth ghi jkl")
    out <- lex$token()
    ee(out$type,  "GGPLOT")
    ee(out$value, "ggplot2::ggplot(iris")
    ee(out$lineno, 1)
    ee(out$lexpos, 1)
    ee(lex$token()$value, " + geom_point")
    ee(lex$token()$value, "abc")
    ee(lex$token()$value, "def")
    ee(lex$token()$value, " + geom_smooth")
   ##Note: improper regex for GGPLOT sometimes
   ##      results in ggplot(hi (all characters before g"hi" are replaced)
    ee(lex$token()$value, "ghi")
    ee(lex$token()$value, "jkl")
    ee(lex$token()$value, NULL)

    lex$input(paste0("gg mtcars##point cyl mpg colour=\"blue\"",
                      " size=4##smooth colour=\"blue\""))
    ee(lex$token()$value, "ggplot2::ggplot(mtcars")
    ee(lex$token()$value, " + geom_point")
    ee(lex$token()$value, "cyl")
    ee(lex$token()$value, "mpg")
    ee(lex$token()$value, "colour=\"blue\"")
    ee(lex$token()$value, "size=4")
    ee(lex$token()$value, " + geom_smooth")
    ee(lex$token()$value, "colour=\"blue\"")
    ee(lex$token()$value, NULL)
})

test_that("ggplot2 parser", {

    ee(yacc$parse("gg iris", lex), "ggplot2::ggplot(iris)")
    ee(yacc$parse("gg iris Sepal.Width", lex),
       "ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width))")
    ee(yacc$parse("gg iris Sepal.Width Sepal.Length", lex),
       "ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length))")
    ee(yacc$parse("gg iris Sepal.Width Sepal.Length##point", lex),
       "ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length)) + " %++%
       "ggplot2::geom_point()")
    expect_message(
        yacc$parse("gg iris SepalWidth##point", lex),
       "No such column name"
       )

    pre <- "ggplot2::ggplot(iris) + ggplot2::geom_"
    ee(yacc$parse("gg iris##point", lex), pre %++% "point()")
    ee(yacc$parse("gg iris##point Sepal.W Sepal.L", lex),
       pre %++% "point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))")
    ee(yacc$parse("gg iris##rect Sepal.W Sepal.L Petal.L Petal.W", lex),
       pre %++% "rect(ggplot2::aes(xmin=Sepal.Width, xmax=Sepal.Length, " %++%
           "ymin=Petal.Length, ymax=Petal.Width))")

    ee(yacc$parse("gg iris##point Sepal.W Sepal.L##smooth", lex),
       pre %++% "point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + " %++%
           "ggplot2::geom_smooth()")
    ee(yacc$parse("gg iris##point Sepal.W Sepal.L##smooth Sepal.W", lex),
       pre %++% "point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + " %++%
           "ggplot2::geom_smooth(ggplot2::aes(x=Sepal.Width))")
    ee(yacc$parse("gg iris##point Sepal.W Sepal.L##smooth Sepal.W Sepal.L",
                  lex),
       pre %++% "point(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)) + " %++%
           "ggplot2::geom_smooth(ggplot2::aes(x=Sepal.Width, y=Sepal.Length))"
    )

    yacc_pre <- "gg iris##point Sepal.W Sepal.L colour=\"blue\""
    pre <- "ggplot2::ggplot(iris) + ggplot2::geom_point" %++%
        "(ggplot2::aes(x=Sepal.Width, y=Sepal.Length)," %++%
        " colour=\"blue\""
    ee(yacc$parse(yacc_pre, lex), pre %++% ")")
    ee(yacc$parse(yacc_pre %++% " size=4", lex), pre %++% ", size=4)")
    ee(yacc$parse(yacc_pre %++% " size=4##smooth Sepal.W Sepal.L", lex),
        pre %++%
        ", size=4) + ggplot2::geom_smooth(" %++%
           "ggplot2::aes(x=Sepal.Width, y=Sepal.Length))")
    ee(yacc$parse(yacc_pre %++% " size=4##smooth colour=\"blue\"", lex),
       pre %++% ", size=4) + ggplot2::geom_smooth(colour=\"blue\")")
    ee(yacc$parse("gg iris##p Sepal.W Sepal.L c=\"blue\" " %++%
        "si=4##sm c=\"blue\"", lex),
       pre %++% ", size=4) + ggplot2::geom_smooth(colour=\"blue\")")

    ee(yacc$parse("gg iris Sepal.W Sepal.L##geom_point##geom_smooth", lex),
       "ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length)) + " %++%
       "ggplot2::geom_point() + ggplot2::geom_smooth()")

    yacc$parse("gg iris ## point Sepal.W Sepal.L col=Spec siz=Petal.W", lex)
})

my_pattern <-
  "ggplot2::ggplot\\(iris\\) \\+ ggplot2::theme\\(text = ggplot2::element_text"
test_that("ggplot2 parse theme", {
    lex$input("gg iris##theme text size=4")
    expect_true(grepl(my_pattern,
                      yacc$parse("gg iris##theme text size=4", lex)))
    expect_true(
        grepl(my_pattern,
              yacc$parse("gg iris##theme text size=4 color=\"red\"", lex)))
})
