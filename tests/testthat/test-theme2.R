context("theme2")
library(ggplot2)

test_that("theme2", {
    ee(theme2(axis.text(sz=20), text(f="bold")),
       theme(axis.text = element_text(size=20),
             text = element_text(face="bold")))

    ee(theme2(axis.text(sz=20), text(f="bold"), as_string = TRUE),
       "theme(axis.text = element_text(size=20)," %++%
           " text = element_text(face=\"bold\"))")

    ggplot(mtcars) + geom_point(aes(mpg, wt)) +
    theme(axis.text = element_text(size=20, face="bold"),
          axis.line = element_line(color = "black"))
    theme2(axis.text(size=20, face="bold"), axis.line(color="black"))
})

